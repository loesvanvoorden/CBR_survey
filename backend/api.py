from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from backend.db import save_conversation
from backend.prompt_templates import custom_prompt
from backend.preprocessing import load_json_data, load_csv_data
from langchain.chat_models import ChatOpenAI
from langchain.agents import initialize_agent, AgentType
from langchain.tools import tool
from langchain.callbacks import get_openai_callback
import os
import json
from typing import List, Dict, Optional
from langchain_core.messages import HumanMessage, AIMessage
import httpx
import pandas as pd
import sqlite3 # Added for DB access
import psycopg2 # For PostgreSQL
from psycopg2.extras import RealDictCursor # To get results as dictionaries

# Load environment variables
from dotenv import load_dotenv
load_dotenv()
openai_api_key = os.getenv("OPENAI_API_KEY")

# Load data
predictions_json_file = load_json_data("data/predictions_3000m.json")
three_km_data = load_csv_data("data/3kmn.csv")

# --- Load trackcor.csv for Python-side adjustments ---
trackcor_df = None
try:
    # Assuming api.py is in backend/, and data/ is at the project root (e.g., ../data)
    # Adjust path if your project structure is different
    trackcor_csv_path = "data/trackcor.csv" 
    if not os.path.exists(trackcor_csv_path):
        # Try another common location if the first fails (e.g. if running from project root)
        trackcor_csv_path = os.path.join(os.path.dirname(__file__), "..", "data", "trackcor.csv")

    if os.path.exists(trackcor_csv_path):
        trackcor_df = pd.read_csv(trackcor_csv_path)
        print("trackcor.csv loaded successfully for Python adjustments.")
    else:
        print(f"Warning: trackcor.csv not found at expected paths. Python-side track adjustments will not work.")
except Exception as e:
    print(f"Warning: Could not load trackcor.csv: {e}. Python-side track adjustments will not work.")
# --- End trackcor.csv loading ---

# --- Database path --- OLD SQLITE
# DATABASE_PATH = os.path.join(os.path.dirname(__file__), "..", "data", "skater_activity.db") 

# NEW PostgreSQL connection details from Railway Environment Variables
DATABASE_URL_STR = os.getenv("DATABASE_URL") # Railway provides this

# Fallback to individual components if DATABASE_URL is not set (less likely on Railway but good for local dev)
DB_HOST = os.getenv("PGHOST") 
DB_PORT = os.getenv("PGPORT", "5432")
DB_NAME = os.getenv("PGDATABASE")
DB_USER = os.getenv("PGUSER")
DB_PASSWORD = os.getenv("PGPASSWORD")

# Define tools

# This is the original tool, let's keep it for now and add a new one for Plumber.
# The new tool will eventually replace this one if successful.
@tool
def get_prediction_by_name_from_json(name: str) -> dict:
    """(DEPRECATED - Use get_skater_prediction_from_r) Retrieve the prediction results for a specific athlete by name from a static JSON file."""
    data = predictions_json_file.get(name)
    if not data:
        return {"error": f"No prediction found for {name} in JSON file."}
    
    predicted_time_cs = data.get("predicted_time", 0)
    predicted_time_sec = predicted_time_cs / 100
    minutes = int(predicted_time_sec // 60)
    seconds = predicted_time_sec % 60
    data["predicted_time_seconds"] = predicted_time_sec
    data["predicted_time_minutes"] = f"{minutes}m {seconds:.2f}s"

    predicted_paces_cs = data.get("predicted_paces", [])
    predicted_paces_sec = [pace / 100 for pace in predicted_paces_cs]
    predicted_paces_minutes = [
        f"{int(pace_sec // 60)}m {pace_sec % 60:.2f}s" for pace_sec in predicted_paces_sec
    ]
    data["predicted_paces_seconds"] = predicted_paces_sec
    data["predicted_paces_minutes"] = predicted_paces_minutes
    return data

# Helper function for track correction (simplified)
def get_python_track_correction(track_name: str, distance: int, lap_time_cs: float) -> float:
    if trackcor_df is None or track_name is None or lap_time_cs is None or not isinstance(lap_time_cs, (int, float)):
        return lap_time_cs 
    try:
        # Ensure track_name is uppercase like in CSV headers (e.g., "AL", "HV")
        track_name_upper = track_name.upper()
        correction_row = trackcor_df[trackcor_df['distance'] == distance]
        if not correction_row.empty and track_name_upper in correction_row.columns:
            factor = correction_row[track_name_upper].iloc[0]
            if pd.notna(factor) and factor != 0:
                return round(lap_time_cs / factor, 2)
        else:
            # print(f"Debug: No correction factor found for track {track_name_upper}, distance {distance}.")
            pass
    except Exception as e:
        print(f"Error in get_python_track_correction for {track_name_upper}, dist {distance}: {e}")
    return lap_time_cs

PLUMBER_BASE_URL = "http://localhost:8001" # Assuming Plumber runs on port 8001

@tool
async def get_skater_prediction_from_r(
    skater_name: str,
    target_age_pb: Optional[int] = None, # Example: 23
    target_track_pb: str = "AL",      # Default target track for PB prediction
    use_adjust: bool = True,
    use_npb_3000m: bool = True,       # Corresponds to npb_val in Plumber
    use_pb_500m: bool = True,         # Corresponds to pb500_val
    use_pb_1000m: bool = True,        # Corresponds to pb1000_val
    use_pb_1500m: bool = True,        # Corresponds to pb1500_val
    ft_range_val_cs: int = 500,       # Finish time range in centiseconds
    age_range_val_years: int = 0      # Age range in years
) -> dict:
    """
    (Fallback if no saved prediction is found) Retrieves a 3000m prediction for an athlete by calling the R Plumber API.
    It attempts to fetch the athlete's PBs/SBs to form a query.
    Parameters:
        skater_name (str): Name of the skater.
        target_age_pb (Optional[int]): Target age for the PB prediction (e.g., 23). If None, uses age from latest 3k race.
        target_track_pb (str): Target track abbreviation for the PB prediction (e.g., "AL", "HV"). Defaults to "AL".
        use_adjust (bool): Whether to apply track adjustments. Defaults to True.
        use_npb_3000m (bool): Use non-PB 3000m data for prediction. Defaults to True.
        use_pb_500m (bool): Use 500m PB data. Defaults to True.
        use_pb_1000m (bool): Use 1000m PB data. Defaults to True.
        use_pb_1500m (bool): Use 1500m PB data. Defaults to True.
        ft_range_val_cs (int): Finish time filter range in centiseconds. Defaults to 500 (5s).
        age_range_val_years (int): Age filter range in years. Defaults to 0 (off).
    """
    q_dict = {}

    # --- 1. Get PersonID, Gender, and base data for the skater ---
    if three_km_data is None:
        return {"error": "3kmn.csv data not loaded in Python API."}
    
    normalized_name_query = skater_name.strip().lower()
    skater_rows = three_km_data[three_km_data['Name'].str.lower() == normalized_name_query]
    
    if skater_rows.empty:
        return {"error": f"Skater '{skater_name}' not found in 3kmn.csv."}

    # Use the first match for PersonID and Gender (assuming Name is unique enough for this context)
    # In Shiny, 'candidates' offers choices if names are ambiguous. Here, we simplify.
    person_id = skater_rows['PersonID'].iloc[0]
    gender = skater_rows['Gender'].iloc[0] # Assuming 'Gender' column exists and is 'H' or 'D'
    
    # For q_dict, we need a full template. The Plumber endpoint expects a q_json that can become a 1-row tibble
    # ideally with all columns that predict_pb's 'q' argument would have after the merge step in Shiny.
    # This is complex to replicate fully in Python without access to R's 'cb' structure easily.
    # Simplified approach: build q_dict with essential known fields, Plumber might need enhancement
    # to merge this with a cb[1,] template row.
    # For now, q_dict will contain only the values we explicitly set.

    q_dict['PersonID'] = person_id
    q_dict['Gender_n'] = gender 
    q_dict['Track_p'] = target_track_pb # Target track for PB prediction

    # --- 2. Fetch race data from Plumber to find PBs/SBs and their laps ---
    # This part needs to be robust and carefully select races similar to Shiny.
    # Placeholder: For now, we'll use hardcoded NAs or 0s for lap times.
    # A real implementation would call PLUMBER_BASE_URL + f"/skater_races?skater_id={person_id}"
    # then parse results, find best/latest races, then call PLUMBER_BASE_URL + f"/lap_times?race_link={link}" for each.
    
    # Example: Get latest 3000m race for age_perf_n and endtime_n (if use_npb_3000m)
    # For simplicity, let's try to find one 3000m race for this skater to get some defaults
    # In a real scenario, you'd call Plumber's /skater_races
    skater_3k_races = skater_rows[skater_rows['distance'] == 3000].sort_values(by='Date', ascending=False) # DtypeWarning: Columns (4) have mixed types.

    age_for_pred = target_age_pb
    if age_for_pred is None: # If no target age, use age from latest 3k race or a default
        if not skater_3k_races.empty and pd.notna(skater_3k_races['age_perf'].iloc[0]):
            age_for_pred = int(skater_3k_races['age_perf'].iloc[0])
        else: # Fallback age if no 3k races or age_perf is NA
            age_for_pred = 23 # Default age
    q_dict['age_perf_n'] = age_for_pred

    # Placeholder default values for laps (in centiseconds)
    # These should be fetched and processed based on skater's actual PBs/SBs
    # And adjusted if use_adjust is True
    default_lap_value_cs = 0 # Or NA if Plumber/R handles NA imputation

    # For 3000m non-PB (if use_npb_3000m)
    q_dict['Track_n'] = skater_3k_races['Track'].iloc[0] if not skater_3k_races.empty else "AL" # Track of the reference 3000m
    q_dict['endtime_n'] = skater_3k_races['endtime'].iloc[0] if not skater_3k_races.empty else 30000 # Default endtime in cs
    for i in range(8): # r_0_n to r_7_n
        lap_val_cs = skater_3k_races[f'r_{i}'].iloc[0] if not skater_3k_races.empty and f'r_{i}' in skater_3k_races.columns and pd.notna(skater_3k_races[f'r_{i}'].iloc[0]) else default_lap_value_cs
        q_dict[f'r_{i}_n'] = lap_val_cs
        q_dict[f'ar_{i}_n'] = get_python_track_correction(q_dict['Track_n'], 3000, lap_val_cs) if use_adjust else lap_val_cs

    # For 500m PB (if use_pb_500m) - Similar logic to fetch PB, its track, and its laps
    # Simplified:
    skater_500_pb_row = skater_rows[(skater_rows['distance'] == 500) & (skater_rows['PB'] == 1)].sort_values(by='endtime').head(1)
    track_500m = skater_500_pb_row['Track'].iloc[0] if not skater_500_pb_row.empty else "AL"
    r_500_0_cs = skater_500_pb_row['r_0'].iloc[0] if not skater_500_pb_row.empty and pd.notna(skater_500_pb_row['r_0'].iloc[0]) else default_lap_value_cs
    r_500_1_cs = skater_500_pb_row['r_1'].iloc[0] if not skater_500_pb_row.empty and pd.notna(skater_500_pb_row['r_1'].iloc[0]) else default_lap_value_cs
    q_dict['r_500_0_n'] = r_500_0_cs
    q_dict['r_500_1_n'] = r_500_1_cs
    q_dict['ar_500_0_n'] = get_python_track_correction(track_500m, 500, r_500_0_cs) if use_adjust else r_500_0_cs
    q_dict['ar_500_1_n'] = get_python_track_correction(track_500m, 500, r_500_1_cs) if use_adjust else r_500_1_cs
    
    # For 1000m PB (if use_pb_1000m) - Simplified:
    skater_1000_pb_row = skater_rows[(skater_rows['distance'] == 1000) & (skater_rows['PB'] == 1)].sort_values(by='endtime').head(1)
    track_1000m = skater_1000_pb_row['Track'].iloc[0] if not skater_1000_pb_row.empty else "AL"
    r_1000_0_cs = skater_1000_pb_row['r_0'].iloc[0] if not skater_1000_pb_row.empty and pd.notna(skater_1000_pb_row['r_0'].iloc[0]) else default_lap_value_cs
    r_1000_1_cs = skater_1000_pb_row['r_1'].iloc[0] if not skater_1000_pb_row.empty and pd.notna(skater_1000_pb_row['r_1'].iloc[0]) else default_lap_value_cs
    r_1000_2_cs = skater_1000_pb_row['r_2'].iloc[0] if not skater_1000_pb_row.empty and pd.notna(skater_1000_pb_row['r_2'].iloc[0]) else default_lap_value_cs
    q_dict['r_1000_0_n'] = r_1000_0_cs
    q_dict['r_1000_1_n'] = r_1000_1_cs
    q_dict['r_1000_2_n'] = r_1000_2_cs
    q_dict['ar_1000_0_n'] = get_python_track_correction(track_1000m, 1000, r_1000_0_cs) if use_adjust else r_1000_0_cs
    q_dict['ar_1000_1_n'] = get_python_track_correction(track_1000m, 1000, r_1000_1_cs) if use_adjust else r_1000_1_cs
    q_dict['ar_1000_2_n'] = get_python_track_correction(track_1000m, 1000, r_1000_2_cs) if use_adjust else r_1000_2_cs

    # For 1500m PB (if use_pb_1500m) - Simplified:
    skater_1500_pb_row = skater_rows[(skater_rows['distance'] == 1500) & (skater_rows['PB'] == 1)].sort_values(by='endtime').head(1)
    track_1500m = skater_1500_pb_row['Track'].iloc[0] if not skater_1500_pb_row.empty else "AL"
    r_1500_0_cs = skater_1500_pb_row['r_0'].iloc[0] if not skater_1500_pb_row.empty and pd.notna(skater_1500_pb_row['r_0'].iloc[0]) else default_lap_value_cs
    r_1500_1_cs = skater_1500_pb_row['r_1'].iloc[0] if not skater_1500_pb_row.empty and pd.notna(skater_1500_pb_row['r_1'].iloc[0]) else default_lap_value_cs
    r_1500_2_cs = skater_1500_pb_row['r_2'].iloc[0] if not skater_1500_pb_row.empty and pd.notna(skater_1500_pb_row['r_2'].iloc[0]) else default_lap_value_cs
    r_1500_3_cs = skater_1500_pb_row['r_3'].iloc[0] if not skater_1500_pb_row.empty and pd.notna(skater_1500_pb_row['r_3'].iloc[0]) else default_lap_value_cs
    q_dict['r_1500_0_n'] = r_1500_0_cs
    q_dict['r_1500_1_n'] = r_1500_1_cs
    q_dict['r_1500_2_n'] = r_1500_2_cs
    q_dict['r_1500_3_n'] = r_1500_3_cs
    q_dict['ar_1500_0_n'] = get_python_track_correction(track_1500m, 1500, r_1500_0_cs) if use_adjust else r_1500_0_cs
    q_dict['ar_1500_1_n'] = get_python_track_correction(track_1500m, 1500, r_1500_1_cs) if use_adjust else r_1500_1_cs
    q_dict['ar_1500_2_n'] = get_python_track_correction(track_1500m, 1500, r_1500_2_cs) if use_adjust else r_1500_2_cs
    q_dict['ar_1500_3_n'] = get_python_track_correction(track_1500m, 1500, r_1500_3_cs) if use_adjust else r_1500_3_cs

    # The q_json needs to represent a single-row structure.
    # Plumber's fromJSON will parse this. R's as_tibble(q_list) expects q_list to be a list
    # where each element can be a column. So, each value in q_dict should be a single value, not a list.
    # If q_dict's values are single items, json.dumps(q_dict) is fine.
    # If R's as_tibble needs list-like columns even for one row, then q_dict values should be lists of one item.
    # jsonlite::fromJSON behavior: if it's a simple dict -> named list. as_tibble(named_list) -> 1-row tibble.
    
    # Ensure all values that should be numeric are, and handle potential NAs from data
    for k, v in q_dict.items():
        if isinstance(v, pd.Series): # handle if data selection returned a Series
            q_dict[k] = v.iloc[0] if not v.empty else None
        if pd.isna(q_dict[k]): # Convert Pandas NA to None for JSON
             q_dict[k] = None # Or a suitable numeric default like 0 if R side expects numbers
        # R's predict_pb might expect numeric types for lap times, age, endtime.
        # NAs in R are often represented as None/null in JSON. jsonlite::fromJSON handles this.

    q_json_payload = json.dumps(q_dict)

    params_for_plumber = {
        "q_json": q_json_payload,
        "ft_range_val": ft_range_val_cs,
        "age_range_val": age_range_val_years,
        "npb_val": use_npb_3000m,
        "pb500_val": use_pb_500m,
        "pb1000_val": use_pb_1000m,
        "pb1500_val": use_pb_1500m,
        "adjust_val": use_adjust
    }

    try:
        async with httpx.AsyncClient() as client:
            response = await client.post(f"{PLUMBER_BASE_URL}/run_prediction", data=params_for_plumber, timeout=30.0) # Send as form data
            response.raise_for_status() # Raise an exception for HTTP errors (4xx or 5xx)
            return response.json()
    except httpx.RequestError as exc:
        return {"error": f"HTTP Request to Plumber failed: {exc}"}
    except httpx.HTTPStatusError as exc:
        return {"error": f"Plumber API returned an error: {exc.response.status_code} - {exc.response.text}"}
    except Exception as e:
        return {"error": f"An unexpected error occurred: {str(e)}"}

@tool
def get_saved_prediction_for_skater(skater_name: str) -> dict:
    """
    Retrieves the latest saved prediction for a specific athlete by name
    from the PostgreSQL database. This database is populated by the R Shiny app.
    """
    conn = None
    
    if not DATABASE_URL_STR and not all([DB_HOST, DB_NAME, DB_USER, DB_PASSWORD]):
        return {"error": "Database connection environment variables (DATABASE_URL or PGHOST, PGDATABASE, etc.) not set for Python API."}
    
    try:
        if DATABASE_URL_STR:
            conn = psycopg2.connect(DATABASE_URL_STR)
            # print("Connected to PostgreSQL using DATABASE_URL") # For debugging
        else: # Fallback to component variables
            conn = psycopg2.connect(
                host=DB_HOST,
                port=DB_PORT,
                dbname=DB_NAME,
                user=DB_USER,
                password=DB_PASSWORD
            )
            # print("Connected to PostgreSQL using component variables") # For debugging
        
        cursor = conn.cursor(cursor_factory=RealDictCursor) 

        cursor.execute("""
            SELECT prediction_output_json, input_parameters_json, prediction_timestamp 
            FROM saved_skater_predictions 
            WHERE skater_name_at_prediction = %s 
            ORDER BY prediction_timestamp DESC 
            LIMIT 1
        """, (skater_name,))
        
        row = cursor.fetchone()
        cursor.close()

        if row:
            prediction_data = row["prediction_output_json"] 
            input_params_data = row["input_parameters_json"]
            prediction_timestamp = row["prediction_timestamp"]

            if isinstance(prediction_data, str):
                prediction_data = json.loads(prediction_data)
            if isinstance(input_params_data, str):
                input_params_data = json.loads(input_params_data) # Should be dict if from JSONB
            
            output_to_return = {
                "retrieved_prediction_timestamp": prediction_timestamp.isoformat() if prediction_timestamp else None, 
                "input_params_for_this_prediction": input_params_data if input_params_data else {},
                "raw_prediction_from_db": prediction_data if prediction_data else {}
            }

            if prediction_data and 'predicted_pb_summary' in prediction_data and prediction_data['predicted_pb_summary']:
                raw_pred_vector = prediction_data['predicted_pb_summary']
                if len(raw_pred_vector) >= 10: 
                    predicted_time_cs = raw_pred_vector[1] 
                    predicted_paces_cs = raw_pred_vector[2:10] 
                    output_to_return["predicted_time"] = predicted_time_cs 
                    output_to_return["predicted_paces"] = predicted_paces_cs
                    predicted_time_sec = predicted_time_cs / 100
                    minutes = int(predicted_time_sec // 60)
                    seconds = predicted_time_sec % 60
                    output_to_return["predicted_time_seconds"] = predicted_time_sec
                    output_to_return["predicted_time_minutes"] = f"{minutes}m {seconds:.2f}s"
                    predicted_paces_sec = [p / 100 for p in predicted_paces_cs]
                    predicted_paces_minutes = [
                        f"{int(ps // 60)}m {ps % 60:.2f}s" for ps in predicted_paces_sec
                    ]
                    output_to_return["predicted_paces_seconds"] = predicted_paces_sec
                    output_to_return["predicted_paces_minutes"] = predicted_paces_minutes
                else:
                    output_to_return["warning_prediction_structure"] = "Predicted PB summary from DB has unexpected structure."

            if prediction_data and 'similar_cases_pb_details' in prediction_data:
                 output_to_return["similar_cases"] = prediction_data['similar_cases_pb_details']
            else:
                output_to_return["similar_cases"] = [] 
            
            return output_to_return
        else:
            return {"message": f"No saved prediction found for '{skater_name}'. You can ask to generate one using the R model, or generate it in the R Shiny app first."}

    except psycopg2.Error as e:
        # Specific check for common Railway initial connection issue if tables not ready
        if "relation \"saved_skater_predictions\" does not exist" in str(e).lower():
             return {"message": f"The predictions table isn't ready yet in the database for skater '{skater_name}'. Please try generating a prediction in the R Shiny app first, then ask again in a few moments."}
        return {"error": f"PostgreSQL Database error: {e}"}
    except Exception as e:
        return {"error": f"An unexpected error occurred: {str(e)}"}
    finally:
        if conn:
            conn.close()

@tool
def get_lap_times(track: str, name: str) -> dict:
    """
    Retrieve the lap times (r_0 to r_7) for a specific Track and Name.
    Recognizes both track abbreviations and full city names.
    Lap times are converted from centiseconds to seconds and to minutes:seconds format.
    """
    # Mapping of track abbreviations to city names
    track_mapping = {
        "AL": "De Meent Alkmaar",
        "AM": "Jaap Edenbaan Amsterdam",
        "AS": "Bonte Wever Assen",
        "BR": "Ijsbaan Breda",
        "DH": "de Uithof Den Haag",
        "DT": "Leisure World Dronten",
        "DV": "De Scheg Deventer", 
        "EN": "Ijsbaan Twente Enschede",
        "EV": "IJssportcentrum Eindhoven",
        "GL": "Glanerbrook Geleen", 
        "GR": "Kardinge Groningen",
        "HA": "Kennemerland Haarlem",
        "HN": "De Westfries Hoorn",
        "HV": "Thialf Heerenveen",
        "LE": "Elfstedenhal Leeuwarden",
        "TB": "Ireen Wüst IJsbaan Thialf",
        "UT": "De Vechtsebanen Utrecht"
    }

    # Reverse the mapping to allow lookup by city name
    reverse_track_mapping = {v: k for k, v in track_mapping.items()}

    # Normalize track input to match the dataset abbreviation
    track = track.strip().lower()
    normalized_track = reverse_track_mapping.get(track.title(), None)

    # Fallback: Check for partial matches in reverse_track_mapping
    if not normalized_track:
        for full_name, abbreviation in reverse_track_mapping.items():
            if track in full_name.lower():
                normalized_track = abbreviation
                break

    if not normalized_track:
        return {"error": f"Track '{track}' not recognized. Please provide a valid track name or abbreviation."}

    # Normalize name to lowercase
    name = name.strip().lower()

    # Normalize dataset columns for comparison
    three_km_data['Track'] = three_km_data['Track'].str.upper()
    three_km_data['Name'] = three_km_data['Name'].str.lower()

    # Filter data based on normalized track and name
    filtered_data = three_km_data[
        (three_km_data['Track'] == normalized_track) & (three_km_data['Name'] == name)
    ]

    if filtered_data.empty:
        return {"error": f"No data found for Track: {track} and Name: {name}"}

    # Extract lap times and convert
    columns_of_interest = [f"r_{i}" for i in range(8)]
    lap_times_raw = filtered_data[columns_of_interest].to_dict(orient='records')

    # Convert lap times for each record
    lap_times = []
    for record in lap_times_raw:
        lap_seconds = []
        lap_minutes = []
        for i in range(8):
            cs = record.get(f"r_{i}", 0)
            sec = cs / 100
            lap_seconds.append(sec)
            lap_minutes.append(f"{int(sec // 60)}m {sec % 60:.2f}s")
        lap_times.append({
            "lap_times_seconds": lap_seconds,
            "lap_times_minutes": lap_minutes
        })

    return {"lap_times": lap_times}

# Initialize LLM and agent
llm = ChatOpenAI(model="gpt-4o-mini", temperature=0.5)
tools = [
    get_saved_prediction_for_skater,      # New tool to get predictions from DB
    get_prediction_by_name_from_json,     # Old tool for static JSON (can be deprecated)
    get_skater_prediction_from_r,         # Tool to call Plumber (can be fallback)
    get_lap_times
]
agent_chain = initialize_agent(
    tools,
    llm,
    agent=AgentType.OPENAI_FUNCTIONS,
    verbose=True,
)

# Initialize FastAPI router
router = APIRouter()

class UserQuery(BaseModel):
    question: str
    language: str
    chat_history: Optional[List[Dict[str, str]]] = None

@router.post("/ask")
async def ask_question(query: UserQuery):
    """
    Handles user questions. Chat history is managed client-side.
    """
    try:
        # Convert client chat_history (list of dicts) to list of LangChain messages
        # This history is BEFORE the current user question
        processed_history_messages = []
        if query.chat_history:
            for msg_data in query.chat_history:
                if msg_data.get("type") == "human" and msg_data.get("content") is not None:
                    processed_history_messages.append(HumanMessage(content=str(msg_data["content"])))
                elif msg_data.get("type") == "ai" and msg_data.get("content") is not None:
                    processed_history_messages.append(AIMessage(content=str(msg_data["content"])))

        # Format the processed history for the prompt string
        history_for_prompt_str = ""
        for msg in processed_history_messages:
            if isinstance(msg, HumanMessage):
                history_for_prompt_str += f"Human: {msg.content}\n"
            elif isinstance(msg, AIMessage):
                history_for_prompt_str += f"AI: {msg.content}\n"
        
        # Format the prompt using the custom_prompt template
        # The 'chat_history' variable in the prompt should expect this string.
        formatted_prompt = custom_prompt.format(context="", question=query.question, chat_history=history_for_prompt_str)

        with get_openai_callback() as cb:
            result = agent_chain.run(formatted_prompt)

            token_usage = {
                "prompt_tokens": cb.prompt_tokens,
                "completion_tokens": cb.completion_tokens,
                "total_tokens": cb.total_tokens,
                "total_cost": cb.total_cost
            }

        token_usage_json = json.dumps(token_usage)
        save_conversation(query.question, result, query.language, token_usage_json, len(result))

        # Construct the full history to return to the client, including the current exchange
        current_question_as_message = HumanMessage(content=query.question)
        ai_answer_as_message = AIMessage(content=result)
        
        # The history sent by client + current Q + current A
        updated_full_history_typed = processed_history_messages + [current_question_as_message, ai_answer_as_message]
        
        # Serialize history back to list of dicts for the client
        serializable_chat_history = []
        for msg in updated_full_history_typed:
            if isinstance(msg, HumanMessage):
                serializable_chat_history.append({"type": "human", "content": msg.content})
            elif isinstance(msg, AIMessage):
                serializable_chat_history.append({"type": "ai", "content": msg.content})
        
        return {
            "answer": result, # The direct answer for immediate display if needed by frontend
            "token_usage": token_usage,
            "chat_history": serializable_chat_history # The complete updated history for client to store
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/reset")
async def reset_conversation():
    """
    Backend is stateless regarding chat history.
    Client should handle clearing its local history.
    """
    return {"message": "Chat reset. Please clear chat history on your client."}
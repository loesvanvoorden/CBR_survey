from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field
from backend.db import save_conversation
from backend.prompt_templates import custom_prompt
from backend.preprocessing import load_json_data, load_csv_data
from langchain.chat_models import ChatOpenAI
from langchain.agents import initialize_agent, AgentType
from langchain.tools import tool
from langchain.callbacks import get_openai_callback
import os
import json
from typing import List, Dict, Optional, Any
from langchain_core.messages import HumanMessage, AIMessage

# Load environment variables
from dotenv import load_dotenv
load_dotenv()
openai_api_key = os.getenv("OPENAI_API_KEY")

# Load data
predictions = load_json_data("data/predictions_3000m.json")
three_km_data = load_csv_data("data/3kmn.csv")


# Define tools
#@tool
#def get_prediction_by_name(name: str) -> dict:
    #"""Retrieve the prediction results for a specific athlete by name, including pacing and similar cases."""
    #return predictions.get(name, {"error": f"No prediction found for {name}"})

@tool
def get_prediction_by_name(name: str) -> dict:
    """Retrieve the prediction results for a specific athlete by name, including pacing and similar cases.
    The predicted time is converted from centiseconds to seconds and then to minutes:seconds format.
    """
    data = predictions.get(name)
    if not data:
        return {"error": f"No prediction found for {name}"}
    
    predicted_time_cs = data.get("predicted_time", 0)  # in centiseconds
    predicted_time_sec = predicted_time_cs / 100  # convert to seconds
    minutes = int(predicted_time_sec // 60)
    seconds = predicted_time_sec % 60

    # Add converted time to the result
    data["predicted_time_seconds"] = predicted_time_sec
    data["predicted_time_minutes"] = f"{minutes}m {seconds:.2f}s"
    
    return data

@tool
def get_lap_times(track: str, name: str) -> dict:
    """
    Retrieve the lap times (r_0 to r_7) for a specific Track and Name.
    Recognizes both track abbreviations and full city names.
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

    # Extract lap times
    columns_of_interest = [f"r_{i}" for i in range(8)]
    lap_times = filtered_data[columns_of_interest].to_dict(orient='records')

    return {"lap_times": lap_times}
# Initialize LLM and agent
llm = ChatOpenAI(model="gpt-4o-mini", temperature=0.5)
tools = [get_prediction_by_name, get_lap_times]
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

# --- New Pydantic models for Shiny data ---
class SkaterRaceDataItem(BaseModel):
    # These fields are based on typical columns in the 'races' dataframe in Shiny.
    # Adjust them to accurately reflect the structure of `data_for_tool_payload` items.
    # Using Optional for all fields to be flexible.
    id: Optional[str] = None
    date: Optional[str] = None
    track: Optional[str] = None
    distance: Optional[int] = None
    time: Optional[str] = None # Original time string e.g., "4:05.67"
    note: Optional[str] = None
    season: Optional[str] = None
    cat: Optional[str] = None
    link: Optional[str] = None
    endtime: Optional[float] = None # Time in seconds or centiseconds, ensure consistency
    # lastSeason: Optional[str] = None # If you send this
    SB: Optional[int] = None # 0 or 1
    PB: Optional[int] = None # 0 or 1
    # curSB: Optional[int] = None # If you send this
    # prevSB: Optional[int] = None # If you send this
    flag: Optional[str] = None # e.g., "PB", "SB", "PB SB"
    # Add any other fields that are present in each item of the racesData list

class ShinyEventPayload(BaseModel):
    timestamp: str
    sessionId: str
    eventType: str
    skaterId: str
    selectedSkaterName: str
    racesData: List[SkaterRaceDataItem] # Expect a list of the structure defined above

# --- End of new Pydantic models ---

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

# --- New Endpoint to receive data from R Shiny app ---
@router.post("/receive_shiny_data")
async def receive_shiny_data(payload: ShinyEventPayload):
    print(f"Received event: {payload.eventType} from Shiny for session: {payload.sessionId}")
    print(f"Skater ID: {payload.skaterId}, Name: {payload.selectedSkaterName}")
    print(f"Timestamp: {payload.timestamp}")
    
    if payload.racesData:
        print(f"Received {len(payload.racesData)} race(s):")
        for i, race in enumerate(payload.racesData):
            print(f"  Race {i+1}:")
            # Print a few key details from the race Pydantic model
            print(f"    Date: {race.date}, Track: {race.track}, Time: {race.time}, Endtime (numeric): {race.endtime}, Flag: {race.flag}")
    else:
        print("No race data in this payload.")

    # TODO: Implement your logic here to process/store the received data.
    # For example, you might want to:
    # 1. Store `payload.racesData` associated with `payload.sessionId` in a database or cache.
    # 2. If your frontend uses WebSockets, you could emit an event to the client with `payload.sessionId`.
    # 3. Trigger other backend processes based on this new data.

    # How to make this data available to "a user that is on the same browser or laptop":
    # - If the user interacts with this Python backend (e.g., via its /ask endpoint or another UI it serves),
    #   that interaction will have the same `payload.sessionId`.
    # - Your other endpoints (e.g., /ask or a new one for fetching user-specific data)
    #   can then retrieve the stored skater data using this sessionId.

    return {"message": "Data received successfully from Shiny", "sessionId": payload.sessionId, "skaterId": payload.skaterId}
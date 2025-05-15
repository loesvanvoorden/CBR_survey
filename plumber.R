# 3000/plumber.R

library(plumber)
library(dplyr)
library(readr) # For read_csv
library(DescTools) # Added because HmsToSec is used in sourced utils.R and data prep
library(stringr)   # Added for predict_pb if used via utils.R
library(FNN)       # Added for predict_pb if used via utils.R

# DescTools should be loaded by utils.R if HmsToSec is used there
# rvest should be loaded by utils.R if getAllRaces/getLaps are used there

# --- Load data globally for Plumber API ---
# This data needs to be accessible by functions in utils.R when sourced,
# especially predict_pb which expects 'cb' and trkcor variables.

# Define paths relative to the plumber.R file location (CBR_survey/3000/)
base_data_path <- "../data"

# Load and prepare 'cb' (case base)
m3000_file_path <- file.path(base_data_path, "3kmn.csv")
if (file.exists(m3000_file_path)) {
  m3000 <- read_csv(m3000_file_path, show_col_types = FALSE) %>% filter(cat != "pup")
  pbs_data <- filter(m3000, pb) # Ensure 'pb' column exists and is logical
  non_pbs_data <- filter(m3000, !pb)
  cb <- inner_join(pbs_data, non_pbs_data, by = "PersonID", suffix = c("_p", "_n"), relationship = "many-to-many")
} else {
  warning(paste("Data file for case base not found:", m3000_file_path, "Predictions will not work."))
  cb <- data.frame() # Empty case base
}

# Load and prepare track correction vectors
trackcor_file_path <- file.path(base_data_path, "trackcor.csv")
if (file.exists(trackcor_file_path)) {
  trackcor_data <- read_csv(trackcor_file_path, show_col_types = FALSE)
  trkcor     <- unlist(select(filter(trackcor_data, distance == 3000), -distance))
  trkcor500  <- unlist(select(filter(trackcor_data, distance == 500),  -distance))
  trkcor1000 <- unlist(select(filter(trackcor_data, distance == 1000), -distance))
  trkcor1500 <- unlist(select(filter(trackcor_data, distance == 1500), -distance))
} else {
  warning(paste("Track correction file not found:", trackcor_file_path, "Track adjustments may not work correctly."))
  # Define empty or default trkcor if file is missing, so utils.R doesn't break if it expects them
  trkcor <- numeric(0); trkcor500 <- numeric(0); trkcor1000 <- numeric(0); trkcor1500 <- numeric(0)
}
# --- End Data Loading ---


# Source the utility functions
# utils.R is in CBR_survey/, plumber.R is in CBR_survey/3000/
utils_path <- "../utils.R"
if (file.exists(utils_path)) {
  source(utils_path)
} else {
  warning(paste("utils.R not found at", utils_path, "API functionality will be limited."))
  # Define placeholder functions if utils.R is critical and not found
  getAllRaces <- function(skate_id) { data.frame(error = "utils.R not found") }
  getLaps <- function(link) { numeric(0) }
  predict_pb <- function(...) { list(error = "utils.R not found") }
}

#* @apiTitle Skater Data API
#* @apiDescription Provides access to skater race data fetched from OSTA and predictions.

#* CORS filter to allow requests from any origin
#* You might want to restrict this to your specific frontend domain in production
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    # Handle preflight request
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* Get processed race data for a skater
#* @param skater_id:character The unique ID of the skater (e.g., from OSTA).
#* @get /skater_races
function(req, res, skater_id="") { # Added res for status setting
  if (is.null(skater_id) || skater_id == "") {
    res$status <- 400 # Bad Request
    return(list(error = "Parameter 'skater_id' is required and cannot be empty."))
  }

  # Fetch races using the refactored function from utils.R
  raw_races <- tryCatch({
    getAllRaces(skater_id)
  }, error = function(e) {
    warning(paste("Error in getAllRaces for skater_id", skater_id, ":", e$message))
    NULL # Return NULL on error
  })

  if (is.null(raw_races)) {
    res$status <- 500 # Internal Server Error
    return(list(error = paste("Failed to fetch races for skater_id:", skater_id)))
  }
  if (nrow(raw_races) == 0) {
    # It's not an error if a skater has no races, return empty list or appropriate message
    return(list(skater_id = skater_id, races = list(), message = "No races found for this skater."))
  }

  # Process races (simplified version of the logic in Shiny's observeEvent(input$rb, ...))
  # This adds PB/SB flags. This logic can be expanded or refined.
  races_processed <- raw_races %>%
    filter(id == skater_id) %>% # Ensure we only have races for the requested id
    group_by(distance) %>%
    mutate(
      min_endtime_for_distance = min(endtime, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(distance, season) %>%
    mutate(
      min_endtime_for_season_distance = min(endtime, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      PB = ifelse(!is.na(endtime) & !is.na(min_endtime_for_distance) & endtime == min_endtime_for_distance, 1, 0),
      SB = ifelse(!is.na(endtime) & !is.na(min_endtime_for_season_distance) & endtime == min_endtime_for_season_distance, 1, 0),
      flag = case_when(
        PB == 1 & SB == 1 ~ "PB SB",
        PB == 1 ~ "PB",
        SB == 1 ~ "SB",
        TRUE ~ ""
      )
    ) %>%
    select(-min_endtime_for_distance, -min_endtime_for_season_distance)

  return(list(skater_id = skater_id, races = races_processed))
}

#* Get lap times for a specific race
#* @param race_link:character The OSTA link for the race (from the 'link' column of /skater_races output)
#* @get /lap_times
function(req, res, race_link = ""){
    if(is.null(race_link) || race_link == ""){
        res$status <- 400
        return(list(error = "Parameter 'race_link' is required."))
    }
    laps <- tryCatch({
        getLaps(race_link)
    }, error = function(e){
        warning(paste("Error in getLaps for race_link", race_link, ":", e$message))
        NULL
    })

    if(is.null(laps)){
        res$status <- 500
        return(list(error = paste("Failed to fetch lap times for race_link:", race_link)))
    }
    if(length(laps) == 0){
        return(list(race_link = race_link, lap_times_cs = list(), message = "No lap times found or processed for this race link."))
    }
    # Lap times are returned in centiseconds by getLaps
    return(list(race_link = race_link, lap_times_cs = laps))
}

#* Run a 3000m prediction using predict_pb
#* @param q_json:string A JSON string representing the query object 'q'. It should contain all necessary fields like PersonID, Gender_n, age_perf_n, endtime_n, and all r_..._n, ar_..._n lap time fields.
#* @param ft_range_val:numeric Finish time range in centiseconds for filtering comparable cases.
#* @param age_range_val:numeric Age range in years for filtering comparable cases.
#* @param npb_val:bool Whether to use non-PB 3000m times in KNN.
#* @param pb500_val:bool Whether to use PB 500m times in KNN.
#* @param pb1000_val:bool Whether to use PB 1000m times in KNN.
#* @param pb1500_val:bool Whether to use PB 1500m times in KNN.
#* @param adjust_val:bool Whether to apply track adjustment.
#* @post /run_prediction
function(req, res, q_json, ft_range_val, age_range_val, npb_val, pb500_val, pb1000_val, pb1500_val, adjust_val) {
  if (is.null(cb) || nrow(cb) == 0) {
    res$status <- 503 # Service Unavailable
    return(list(error = "Case base ('cb') is not loaded or empty. Cannot run prediction."))
  }
  if (is.null(utils_path) || !exists("predict_pb")) { # Check if predict_pb was loaded
      res$status <- 503
      return(list(error = "predict_pb function not available from utils.R. Cannot run prediction."))
  }

  # Convert boolean params from string if they come as strings from HTTP query
  # Plumber typically handles type conversion for simple types, but explicit is safer for POST body or complex queries.
  # For POST with JSON body, plumber does a good job, but let's assume they might be query params for now.
  npb_val <- as.logical(npb_val)
  pb500_val <- as.logical(pb500_val)
  pb1000_val <- as.logical(pb1000_val)
  pb1500_val <- as.logical(pb1500_val)
  adjust_val <- as.logical(adjust_val)
  ft_range_val <- as.numeric(ft_range_val)
  age_range_val <- as.numeric(age_range_val)

  # Parse the q_json string into an R list/object
  # The structure of q_json must perfectly match what predict_pb expects for `q`
  # This usually means it should be a single-row data.frame or a tibble after parsing.
  q_list <- tryCatch({
    jsonlite::fromJSON(q_json)
  }, error = function(e) {
    warning(paste("Error parsing q_json:", e$message))
    NULL
  })

  if (is.null(q_list)) {
    res$status <- 400 # Bad Request
    return(list(error = "Failed to parse q_json. Ensure it is a valid JSON string representing a single row structure for the query."))
  }

  # Convert q_list to a tibble or data.frame if it's not already.
  # predict_pb likely expects q to be a data.frame/tibble with specific column names and types.
  # The `merge(cb[1,], query, all=T)[2,1:40]` from Shiny ensures q has all columns from cb.
  # Here, we must ensure q_json provides a compatible structure.
  # For simplicity, we assume q_json provides a list that can be directly made into a 1-row tibble
  # with correct names and types. This is a critical contract with the calling client (api.py).
  q_tibble <- tryCatch({
    as_tibble(q_list) # q_list should be a named list where names are colnames
  }, error = function(e) {
      warning(paste("Error converting q_list to tibble:", e$message))
      NULL
  })
  
  if (is.null(q_tibble) || nrow(q_tibble) != 1) {
      res$status <- 400
      return(list(error = "Parsed q_json could not be converted to a single-row tibble."))
  }
  
  # Ensure q_tibble has the same columns as cb[1, ] before passing to predict_pb, similar to Shiny
  # This is tricky if q_json doesn't have all columns. A safer way is for q_json to represent
  # only the *query-specific* values, and this endpoint merges it with a cb template.
  # For now, let's assume q_json is already providing the full structure. 
  # (This part might need refinement based on how api.py constructs q_json)

  prediction_result <- tryCatch({
    predict_pb(q = q_tibble, 
               cb = cb, 
               ft_range = ft_range_val, 
               age_range = age_range_val, 
               npb = npb_val, 
               pb500 = pb500_val, 
               pb1000 = pb1000_val, 
               pb1500 = pb1500_val, 
               adjust = adjust_val)
  }, error = function(e) {
    warning(paste("Error during predict_pb execution:", e$message))
    list(error = paste("Prediction failed:", e$message))
  })

  if ("error" %in% names(prediction_result)) {
    res$status <- 500 # Internal Server Error
  }
  
  return(prediction_result)
}

# To run this Plumber API:
# 1. Save this as 'plumber.R' in your '3000/' directory.
# 2. Ensure 'utils.R' is in the same '3000/' directory and contains the refactored functions.
# 3. Open R in the '3000/' directory (or set working directory to it).
# 4. Install plumber: install.packages("plumber")
# 5. Run in R console:
#    api <- plumber::plumb("plumber.R")
#    api$run(port=8001) # Or any other available port
#
# Example Endpoints to test in browser or with curl:
# http://localhost:8001/skater_races?skater_id=YOUR_SKATER_ID_FROM_OSTA
# http://localhost:8001/lap_times?race_link=rit.php?%E8%A8%83%E8%88%91%E8%A8%8312345 (use a real link from skater_races output)
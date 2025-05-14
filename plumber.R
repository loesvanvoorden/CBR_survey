# 3000/plumber.R

library(plumber)
library(dplyr)
# DescTools should be loaded by utils.R if HmsToSec is used there
# rvest should be loaded by utils.R if getAllRaces/getLaps are used there

# Source the utility functions
# Ensure the working directory for Plumber is the '3000' directory,
# or adjust the path to utils.R accordingly.
source("utils.R") # This will also load trkcor data if utils.R is set up to do so.

#* @apiTitle Skater Data API
#* @apiDescription Provides access to skater race data fetched from OSTA.

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
# http://localhost:8001/lap_times?race_link=rit.php?तान舑तान12345 (use a real link from skater_races output)
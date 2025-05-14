library(rvest)
library(dplyr)
library(DescTools) # For HmsToSec
library(FNN)       # For predict_pb
library(stringr)   # For predict_pb (str_sub, etc.), and general string ops if needed
library(readr)     # For read_csv for trackcor data

# Function to get all races for a skater ID from OSTA
getAllRaces<- function(skate_id)
{
  l<-paste0("https://www.osta.nl/index.php?pid=",skate_id,"&Seizoen=ALL&Afstand=&perAfstand=0&Verkort=1")
  ar<-read_html(l)
  allraces <-ar %>%  html_nodes(xpath="//div[@class='seizoen']")
  s<-allraces %>% html_nodes(xpath="h2") %>% html_text()
  season<-substr(s,1,4)
  cat<-substr(s,11,100)

  racesL<-data.frame()

  for (i in 1:length(allraces))
  {
    r_table_list <- allraces[[i]] %>% html_elements("table") # Use html_elements for safety
    if (length(r_table_list) == 0) {next}
    r <- r_table_list[[1]] %>% html_table() # Process only the first table if multiple exist
    if (nrow(r)==0) {next} # skip if no content for this season
    names(r)<-c("date","track","distance","time", "note")
    r<-filter(r,time!="")
    links <-allraces[[i]] %>% html_nodes(xpath = "table/tr/td[4]/a") %>% html_attr("href")
    r$season=season[i]
    r$cat=cat[i]
    r$id=skate_id
    r$link=links
    racesL<-rbind(racesL,r)
  }
    if (nrow(racesL)>0)
      {
      racesL<-filter(racesL, distance>300)
      # Ensure HmsToSec is available, gracefully handle cases where time might be malformed for HmsToSec
      racesL$endtime <- tryCatch({
        HmsToSec(lapply(racesL$time, function(x) {
          if(is.na(x) || x == "") return("00:00:00") # Handle NA or empty strings
          num_colons <- lengths(regmatches(x, gregexpr(":", x)))
          if(num_colons == 0) return(paste("00:00:", x, sep=""))
          if(num_colons == 1) return(paste("00:", x, sep=""))
          return(x)
        }))
      }, error = function(e) {
        warning(paste("HmsToSec conversion error in getAllRaces:", e$message))
        NA_real_ # Return NA if conversion fails
      })
      racesL$date<-strftime(as.POSIXct(racesL$date, format="%d-%m-%Y"))
    }
    return(racesL)
}

# Function to get lap times for a specific race link from OSTA
getLaps <- function(link)
{
  page_url <- paste0("https://www.osta.nl/", link)
  page <- tryCatch(read_html(page_url), error = function(e) {
    warning(paste("Failed to read URL:", page_url, "-", e$message))
    return(NULL)
  })
  if(is.null(page)) return(numeric(0)) # Return empty if page read fails

  times_list <- page %>% html_nodes(xpath="//*[@id='main']/table")
  if(length(times_list) == 0) return(numeric(0))

  times <- times_list[[1]] %>% html_table() %>% data.frame()
  if(nrow(times) == 0 || !("Rondetijd" %in% names(times)) || !("Tussentijd" %in% names(times))) return(numeric(0))

  # Ensure HmsToSec is available
  times$Rondetijd_sec <- tryCatch({
    HmsToSec(lapply(times$Rondetijd, function(x) {
      if(is.na(x) || x == "") return("00:00:00")
      num_colons <- lengths(regmatches(x, gregexpr(":", x)))
      if(num_colons == 0) return(paste("00:00:", x, sep=""))
      if(num_colons == 1) return(paste("00:", x, sep=""))
      return(x)
    }))
  }, error = function(e) {
    warning(paste("HmsToSec conversion error for Rondetijd in getLaps:", e$message))
    NA_real_
  })

  laptimes <- times$Rondetijd_sec * 100
  if (length(laptimes) > 0 && !is.na(times$Tussentijd[1])) {
      # Ensure Tussentijd[1] is numeric before conversion
      first_tussentijd <- suppressWarnings(as.numeric(as.character(times$Tussentijd[1])))
      if (!is.na(first_tussentijd)) {
          laptimes[1] <- first_tussentijd * 100
      } else {
          warning("First Tussentijd is not a valid number in getLaps. Using original Rondetijd.")
      }
  } else if (length(laptimes) > 0) {
      warning("First Tussentijd is NA or laptimes is empty before first lap assignment in getLaps.")
  }
  return(laptimes[!is.na(laptimes)]) # Return only non-NA lap times
}

# --- Track Correction Data Loading ---
# It's good practice to load data needed by functions within this util script,
# or ensure they are passed as arguments.
trkcor <- NULL
trkcor500 <- NULL
trkcor1000 <- NULL
trkcor1500 <- NULL

if (file.exists("3000/data/trackcor.csv")) {
  trkcor_data <- readr::read_csv("3000/data/trackcor.csv", show_col_types = FALSE)
  if (nrow(trkcor_data) > 0 && "distance" %in% names(trkcor_data)) {
    trkcor     <- dplyr::select(dplyr::filter(trkcor_data, distance == 3000), -distance) %>% unlist()
    trkcor500  <- dplyr::select(dplyr::filter(trkcor_data, distance == 500),  -distance) %>% unlist()
    trkcor1000 <- dplyr::select(dplyr::filter(trkcor_data, distance == 1000), -distance) %>% unlist()
    trkcor1500 <- dplyr::select(dplyr::filter(trkcor_data, distance == 1500), -distance) %>% unlist()
  } else {
    warning("trackcor.csv is empty or does not contain a 'distance' column.")
  }
} else {
  warning("trackcor.csv not found in 3000/data/ directory. Track corrections will not work or might use global variables if available elsewhere.")
}
# --- End Track Correction Data Loading ---


# CBR prediction function
predict_pb <- function (q, cb, ft_range=500, age_range=5, adjust=FALSE, npb = TRUE, pb500 = FALSE, pb1000 = FALSE, pb1500=FALSE, sameTrack=FALSE, noCases=10)
{
  #get gender from query
  q_gender=q$Gender_n;
  q_track=q$Track_n;
  # filter cases based on input vars
  cases<-filter(cb, Gender_n==q_gender & PersonID!=q$PersonID) # Ensure cb is passed or globally available
  if (age_range>0 && "age_perf_p" %in% names(cases) && "age_perf_n" %in% names(q)) { # Check column existence
      cases<-filter(cases, age_perf_p < q$age_perf_n + age_range & age_perf_p > q$age_perf_n - age_range)
  }
  if (ft_range>0 && "endtime_n" %in% names(cases) && "endtime_n" %in% names(q)) { # Check column existence
      cases<-filter(cases, endtime_n < q$endtime_n + ft_range & endtime_n > q$endtime_n - ft_range)
  }
  if (sameTrack && "Track_n" %in% names(cases) && "Track_n" %in% names(q)) { # Check column existence
      cases<-filter(cases, Track_n == q_track)
  }

  a<-ifelse(adjust, "^a", "^")
  p500_str <- ifelse(pb500, paste0(a,"r_500_.?_n|"), "")
  p1000_str <- ifelse(pb1000, paste0(a,"r_1000_.?_n|"), "")
  p1500_str <- ifelse(pb1500, paste0(a,"r_1500_.?_n|"), "")
  np_str <-ifelse(npb, paste0(a,"r_.?_n|"),"") # Assuming this means any 3000m non-PB based on context in app
  
  regStr_parts <- c(p500_str, p1000_str, p1500_str, np_str)
  regStr_parts <- regStr_parts[regStr_parts != ""] # Remove empty strings
  
  if(length(regStr_parts) == 0) {
    warning("No distance types selected for prediction (pb500, pb1000, pb1500, npb for 3000m). Cannot select columns for KNN.")
    # Decide how to handle: return error, empty list, or current q as prediction?
    # For now, returning an empty structure that matches expected output list length
    empty_paces <- setNames(rep(NA_real_, 8), paste0("r", 0:7))
    return(list(c(0, NA_real_, empty_paces), data.frame(), data.frame(), data.frame()))
  }
  
  regStr = paste(regStr_parts, collapse="")
  regStr = substr(regStr, 1, nchar(regStr)-1) # Remove trailing |

  # Ensure necessary columns exist before select_matches
  required_cols_pattern = regStr
  cols_for_select_p <- names(cases)[grepl(".*_p", names(cases))] # All _p columns
  
  # Check if 'cases' has columns matching regStr and other required columns
  # This is a simplified check; robust checking would parse regStr
  if(nrow(cases) == 0 || !any(grepl(gsub("\\^|\\.\\?","" , regStr), names(cases)))) {
      warning("No cases match initial filters or no relevant data columns for KNN based on regStr.")
      empty_paces <- setNames(rep(NA_real_, 8), paste0("r", 0:7))
      return(list(c(0, NA_real_, empty_paces), data.frame(), data.frame(), data.frame()))
  }

  selected_cases_cols <- names(cases)[matches(regStr, vars=names(cases))]
  if(length(selected_cases_cols) == 0 && !("PersonID" %in% names(cases))) { # Simplified check
      warning("No columns selected by regStr, or PersonID missing. Aborting predict_pb.")
      empty_paces <- setNames(rep(NA_real_, 8), paste0("r", 0:7))
      return(list(c(0, NA_real_, empty_paces), data.frame(), data.frame(), data.frame()))
  }
  
  # Select necessary columns, handle missing age_perf_n in cases if not used for filtering
  cols_to_select_distinct <- c(selected_cases_cols, "PersonID", cols_for_select_p, "endtime_n", "Track_n")
  if("age_perf_n" %in% names(cases)) { # Original code removes age_perf_n from cases
      cols_to_select_distinct <- setdiff(cols_to_select_distinct, "age_perf_n")
  }
  
  cases_filtered <- cases %>% select(any_of(unique(cols_to_select_distinct))) %>% distinct()
  if(nrow(cases_filtered) == 0) {
      warning("No distinct cases left after selecting columns for KNN.")
      empty_paces <- setNames(rep(NA_real_, 8), paste0("r", 0:7))
      return(list(c(0, NA_real_, empty_paces), data.frame(), data.frame(), data.frame()))
  }
  cases <- cases_filtered # Use the filtered and distinct cases

  nk=min(noCases, nrow(cases))
  if(nk == 0) {
      warning("No cases available for KNN (nk=0).")
      empty_paces <- setNames(rep(NA_real_, 8), paste0("r", 0:7))
      return(list(c(0, NA_real_, empty_paces), data.frame(), data.frame(), data.frame()))
  }

  # generate y (target variable for KNN)
  if (adjust && "aendtime_p" %in% names(cases)) {y=cases$aendtime_p}
  else if ("endtime_p" %in% names(cases)) {y=cases$endtime_p}
  else {
      warning("Target variable 'endtime_p' or 'aendtime_p' not found in cases.")
      empty_paces <- setNames(rep(NA_real_, 8), paste0("r", 0:7))
      return(list(c(nk, NA_real_, empty_paces), data.frame(), data.frame(), data.frame()))
  }

  X_cols_for_knn <- names(cases)[matches(regStr, vars=names(cases))]
  X.t_cols_for_knn <- names(q)[matches(regStr, vars=names(q))]
  
  # Ensure X and X.t have the same columns in the same order
  common_cols_for_knn <- intersect(X_cols_for_knn, X.t_cols_for_knn)
  if(length(common_cols_for_knn) == 0){
      warning("No common columns found for KNN between cases (X) and query (X.t) based on regStr.")
      empty_paces <- setNames(rep(NA_real_, 8), paste0("r", 0:7))
      return(list(c(nk, NA_real_, empty_paces), data.frame(), data.frame(), data.frame()))
  }

  X=select(cases, all_of(common_cols_for_knn))
  X.t = select(q, all_of(common_cols_for_knn))
  
  # Check for NA/NaN/Inf in X and X.t, and y (knn.reg will error)
  if(any(!is.finite(as.matrix(X))) || any(!is.finite(as.matrix(X.t))) || any(!is.finite(y))){
      warning("Non-finite values (NA/NaN/Inf) found in data for KNN. Imputation or filtering might be needed.")
      # Optionally, attempt to remove rows with non-finite values from X and corresponding y
      finite_rows_X <- complete.cases(X)
      finite_y <- is.finite(y)
      valid_indices <- finite_rows_X & finite_y
      X <- X[valid_indices, , drop = FALSE]
      y <- y[valid_indices]
      if(nrow(X) < nk || nrow(X) == 0) {
          warning("Not enough finite cases remaining for KNN after attempting to clean data.")
          empty_paces <- setNames(rep(NA_real_, 8), paste0("r", 0:7))
          return(list(c(nk, NA_real_, empty_paces), data.frame(), data.frame(), data.frame()))
      }
      nk = min(nk, nrow(X)) # Adjust nk if rows were removed
  }


  knn_result=tryCatch(knn.reg(X, X.t, y, k=nk), error = function(e) {
      warning(paste("knn.reg error:", e$message))
      return(NULL)
  })

  if(is.null(knn_result) || is.null(knn_result$pred)) {
    pred_val <- NA_real_
  } else {
    pred_val <- knn_result$pred
  }

  # Track correction for prediction
  # Ensure trkcor is loaded and q$Track_p is a valid name in trkcor
  final_pred <- if (adjust && !is.null(trkcor) && q$Track_p %in% names(trkcor) && !is.na(pred_val)) {
    trkcor[[q$Track_p]] * pred_val
  } else if (!is.na(pred_val)) {
    pred_val
  } else {
    NA_real_
  }

  nni_result <- NULL
  nn <- data.frame() # Initialize nn
  pred_pb_paces <- setNames(rep(NA_real_, 8), paste0("dr", 0:7, "_p")) # Initialize with NAs

  if(nk > 0 && !is.null(knn_result)){ # Only proceed if nk > 0 and knn_result was successful
      nni_result <- tryCatch(get.knnx(X, X.t, k=nk), error = function(e) {
          warning(paste("get.knnx error:", e$message))
          return(NULL)
      })
  }

  if (!is.null(nni_result) && !is.null(nni_result$nn.index)) {
    nn <- cases %>% filter(row_number() %in% nni_result$nn.index)
    # Calculate relative paces if nn is not empty and has the required columns
    dr_p_cols <- names(nn)[matches("^dr.?_p", vars=names(nn))]
    if (nrow(nn) > 0 && length(dr_p_cols) > 0) {
        # Ensure all columns for colMeans are numeric and handle NAs
        rel_paces_data <- nn %>% select(all_of(dr_p_cols)) %>% mutate(across(everything(), as.numeric))
        rel_paces <- colMeans(rel_paces_data, na.rm = TRUE)
        
        # Ensure pred_val is not NA for pace calculation
        if(!is.na(final_pred)) {
             pred_pb_paces_calculated <- rel_paces * (final_pred / 7.5) # 7.5 laps in 3000m
             # Match calculated paces to the expected 8 lap structure (dr0_p to dr7_p)
             # This assumes rel_paces names correspond directly or need mapping
             for(name in names(pred_pb_paces_calculated)){
                 if(name %in% names(pred_pb_paces)) pred_pb_paces[[name]] <- pred_pb_paces_calculated[[name]]
             }
        }
    }
  }
  
  # Prepare output, ensuring columns exist in nn or q
  cases_p_cols <- names(nn)[matches("^r_.?_p", vars=names(nn))]
  cases_p <- if(nrow(nn) > 0 && length(cases_p_cols) > 0) select(nn, all_of(cases_p_cols)) else data.frame()

  cases_n_cols <- names(nn)[matches(regStr, vars=names(nn))]
  # Also include endtime_n, Track_n if they exist in nn
  if("endtime_n" %in% names(nn)) cases_n_cols <- c(cases_n_cols, "endtime_n")
  if("Track_n" %in% names(nn)) cases_n_cols <- c(cases_n_cols, "Track_n")
  cases_n <- if(nrow(nn) > 0 && length(cases_n_cols) > 0) select(nn, any_of(unique(cases_n_cols))) else data.frame()

  pb_short_cols <- c("endtime_500_p", "trk_500_p", "endtime_1000_p", "trk_1000_p", "endtime_1500_p", "trk_1500_p")
  pb_short <- if(nrow(nn) > 0 && all(pb_short_cols %in% names(nn))) select(nn, all_of(pb_short_cols)) else data.frame()

  # Ensure pred_pb_paces is a vector of 8, even if some are NA
  final_pred_pb_paces_vec <- rep(NA_real_, 8)
  names(final_pred_pb_paces_vec) <- paste0("r", 0:7) # Or matching the expected output format e.g. dr0_p
  # Assuming pred_pb_paces from colMeans has names like "dr0_p", "dr1_p" etc.
  # and we want to return a simple vector of 8 lap times for the prediction summary.
  # This part needs to align with how the Shiny app expects the 8 lap times for the prediction.
  # The original list returns c(nk, final_pred, pred_pb_paces).
  # If pred_pb_paces is a named vector from colMeans, it might already be suitable.
  # If it's meant to be an unnamed vector of 8 values:
  # For now, let's assume pred_pb_paces is structured correctly by the colMeans step (e.g. 8 values).
  # If not, this is where mapping/padding to 8 values would occur.
  # The original code `c(nk, final_pred, pred_pb_paces)` implies pred_pb_paces is a vector.
  # The length of pred_pb_paces depends on `matches("^dr.?_p")`. If it has 8 elements, great.
  # Let's make sure the return value is always nk + final_pred + 8 pace values.
  
  prediction_summary_paces <- pred_pb_paces # This is a named vector based on "dr._p" cols
  # We need to ensure it's 8 values for the summary vector.
  # Let's assume the Shiny code expects 8 values named consistently (e.g. r0...r7 for the output table)
  # and the predict_pb_paces variable coming from colMeans might not be in that exact format.
  # The Shiny app (tablePB reactive) does: round(pred[3:10]/100,2) for the "voorspeld PB" row
  # This implies it expects 8 values starting from index 3 of the first list element.
  # So, pred_pb_paces should be a vector of 8 values.
  
  # Standardize pred_pb_paces to be a vector of 8, using NA for missing 'drX_p' values
  standard_paces_names <- paste0("dr", 0:7, "_p")
  final_paces_for_summary <- sapply(standard_paces_names, function(name) {
      if (name %in% names(prediction_summary_paces)) {
          return(prediction_summary_paces[[name]])
      } else {
          return(NA_real_)
      }
  }, USE.NAMES = FALSE)


  list_element1 <- c(nk, final_pred, final_paces_for_summary)

  # Ensure the structure of returned data frames for nn related parts
  # Select age_perf_p, Track_p, endtime_p from nn if available
  nn_summary_cols <- c()
  if("age_perf_p" %in% names(nn)) nn_summary_cols <- c(nn_summary_cols, "age_perf_p")
  if("Track_p" %in% names(nn)) nn_summary_cols <- c(nn_summary_cols, "Track_p")
  if("endtime_p" %in% names(nn)) nn_summary_cols <- c(nn_summary_cols, "endtime_p")
  
  list_element2_part1 <- if(nrow(nn) > 0 && length(nn_summary_cols) > 0) select(nn, all_of(nn_summary_cols)) else data.frame()
  list_element2 <- if(ncol(list_element2_part1) > 0 && ncol(cases_p) > 0) cbind(list_element2_part1, cases_p)
                   else if(ncol(list_element2_part1) > 0) list_element2_part1
                   else if(ncol(cases_p) > 0) cases_p # Should not happen if part1 is empty due to nn being empty
                   else data.frame()
  
  return(list(list_element1, list_element2, cases_n, pb_short))
}

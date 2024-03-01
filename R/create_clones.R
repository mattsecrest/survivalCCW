#' Create clones for CCW analysis
#' 
#' Pass a one-row-per-patient data.frame and get back a data.frame with one row per clone.
#' 
#' @param df A data.frame with one row per patient.
#' @param id The name of the column in `df` that contains the patient identifier.
#' @param event The name of the column in `df` that contains the event of interest.
#' @param time_to_event The name of the column in `df` that contains the time to event.
#' @param exposure The name of the column in `df` that contains the exposure.
#' @param time_to_exposure The name of the column in `df` that contains the time to exposure.
#' @param ced_window The date at which the clinical eligibility window closes. Can be left empty, in which case the clinical eligibility window is assumed to be part of 
#' `exposure` and `time_to_exposure`
#' 
#' @return A data.frame with one row per clone.
#' 
#' @export 
#' 
#' @examples 
#' 
#' # Load the toy dataset
#' data(toy_df)
#' 
#' # Create clones
#' clones <- create_clones(toy_df, 
#'                         id = "id", 
#'                         event = "death", 
#'                         time_to_event = "fup_obs", 
#'                         exposure = "surgery", 
#'                         time_to_exposure = "timetosurgery", 
#'                         ced_window = 365.25/2)
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.
create_clones <- function(
   df, 
   id,
   event,
   time_to_event,
   exposure,
   time_to_exposure,
   ced_window = NULL
) {

   # Check inputs
   valid_inputs <- check_inputs(df = df, 
                                              id = id,
                                              event = event,
                                              time_to_event = time_to_event,
                                              exposure = exposure,
                                              time_to_exposure = time_to_exposure,
                                              ced_window = ced_window)

   if (!valid_inputs) stop("something went wrong")

   # Update exposure and time-to-exposure based on CED window
   n_pts_to_update <- sum(!is.na(df[, time_to_exposure]) &  df[, time_to_exposure] > ced_window)
   if (n_pts_to_update > 0) {
      message(paste0("Updating ", n_pts_to_update, " patients' exposure and time-to-exposure based on CED window"))
      ced_window_na_type <- ifelse(is.integer(ced_window), NA_integer_, NA_real_)
      df[, exposure] <- ifelse(!is.na(df[, time_to_exposure]) &  df[, time_to_exposure] > ced_window, 0L, df[, exposure])
      df[, time_to_exposure] <- ifelse(!is.na(df[, time_to_exposure]) &  df[, time_to_exposure] > ced_window, ced_window_na_type, df[, time_to_exposure])
   }

   # Create clones
   df_0 <- df_1 <- df
   df_0$outcome <- df_1$outcome <- rep(NA_integer_, NROW(df))
   df_0$fup_outcome <- df_1$fup_outcome <- rep(NA_real_, NROW(df))
   df_0$censor <- df_1$censor <- rep(NA_integer_, NROW(df))
   df_0$fup_censor <- df_1$fup_censor <- rep(NA_real_, NROW(df))
   df_0$clone <- 0L
   df_1$clone <- 1L

   # OUTCOMES
   ## EXPOSED
   ### Truly exposed --> keep outcomes
   df_1[df_1[, exposure] == 1L, "outcome"] <- df_1[df_1[, exposure] == 1L, event]
   df_1[df_1[, exposure] == 1L, "fup_outcome"] <- df_1[df_1[, exposure] == 1L, time_to_event]
   
   ### Truly not exposed, follow-up ends before CED --> keep outcomes
   df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, "outcome"] <- df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, event]
   df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, "fup_outcome"] <- df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, time_to_event]

   ### Truly not exposed, follow-up ends after CED --> censor
   df_1[df_1[, exposure] == 0L & df_1[, time_to_event] > ced_window, "outcome"] <- 0L
   df_1[df_1[, exposure] == 0L & df_1[, time_to_event] > ced_window, "fup_outcome"] <- ced_window

   ## UNEXPOSED
   ### Truly unexposed --> keep outcomes
   df_0[df_0[, exposure] == 0L, "outcome"] <- df_0[df_0[, exposure] == 0L, event]
   df_0[df_0[, exposure] == 0L, "fup_outcome"] <- df_0[df_0[, exposure] == 0L, time_to_event]

   ### Truly not exposed --> censor at exposure
   df_0[df_0[, exposure] == 1L, "outcome"] <- 0L
   df_0[df_0[, exposure] == 1L, "fup_outcome"] <- df_0[df_0[, exposure] == 1L, time_to_exposure]

   # CENSORING
   ## EXPOSED
   ### Truly exposed --> Do not censor. Risk of censoring ends at exposure date
   df_1[df_1[, exposure] == 1L, "censor"] <- 0L
   df_1[df_1[, exposure] == 1L, "fup_censor"] <- df_1[df_1[, exposure] == 1L, time_to_exposure]

   ### Truly not exposed, true censorship before/on CED --> Do not censor. Risk of censoring ends at event date
   df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, "censor"] <- 0L
   df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, "fup_censor"] <- df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, time_to_event]

   ### Truly not exposed, true censorship on/after CED --> Censor at CED.
   df_1[df_1[, exposure] == 0L & df_1[, time_to_event] > ced_window, "censor"] <- 1L
   df_1[df_1[, exposure] == 0L & df_1[, time_to_event] > ced_window, "fup_censor"] <- ced_window

   ## UNEXPOSED
   ### Truly exposed --> Censored at time of exposure.
   df_0[df_0[, exposure] == 1L, "censor"] <- 1L
   df_0[df_0[, exposure] == 1L, "fup_censor"] <- df_0[df_0[, exposure] == 1L, time_to_exposure]

   ### Truly not exposed, true censorship before/on CED --> Do not censor. Risk of censoring ends at event date
   df_0[df_0[, exposure] == 0L & df_0[, time_to_event] <= ced_window, "censor"] <- 0L
   df_0[df_0[, exposure] == 0L & df_0[, time_to_event] <= ced_window, "fup_censor"] <- df_0[df_0[, exposure] == 0L & df_0[, time_to_event] <= ced_window, time_to_event]

   ### Truly not exposed, true censorship on/after CED --> Do not censor. Risk of censoring ends at CED.
   df_0[df_0[, exposure] == 0L & df_0[, time_to_event] > ced_window, "censor"] <- 0L
   df_0[df_0[, exposure] == 0L & df_0[, time_to_event] > ced_window, "fup_censor"] <- ced_window

   # Combine and return 
   df_clones <- rbind(df_1, df_0)
   df_clones <- df_clones[order(df_clones[, id], df_clones[, "clone"]), ]

   # Add class
   class(df_clones) <- c("ccw_clones", class(df_clones))

   # Pass attributes
   attributes(df_clones)$id <- id
   attributes(df_clones)$event <- event
   attributes(df_clones)$time_to_event <- time_to_event
   attributes(df_clones)$exposure <- exposure
   attributes(df_clones)$time_to_exposure <- time_to_exposure
   attributes(df_clones)$ced_window <- ced_window

   # Remove rownames
   rownames(df_clones) <- NULL

   # Return
   return(df_clones)
   
}


#' Check inputs to create clones
#' 
#' @param df A data.frame with one row per patient.
#' @param id The name of the column in `df` that contains the patient identifier.
#' @param event The name of the column in `df` that contains the event of interest.
#' @param time_to_event The name of the column in `df` that contains the time to event.
#' @param exposure The name of the column in `df` that contains the exposure.
#' @param time_to_exposure The name of the column in `df` that contains the time to exposure.
#' @param ced_window The date at which the clinical eligibility window closes. 
#' `exposure` and `time_to_exposure`
#' 
#' @return TRUE if inputs are valid else false
check_inputs <- function(
   df, 
   id,
   event,
   time_to_event,
   exposure,
   time_to_exposure,
   ced_window
) {

   inputs_good <- FALSE 

   # Check all input types 
   checkmate::assert_class(df, "data.frame")
   checkmate::assert_class(id, "character")
   checkmate::assert_class(event, "character")
   checkmate::assert_class(time_to_event, "character")
   checkmate::assert_class(exposure, "character")
   checkmate::assert_class(time_to_exposure, "character")
   checkmate::assert_class(ced_window, "numeric")

   # Check that all columns are in data
   checkmate::assert_subset(c(id, event, time_to_event, exposure, time_to_exposure), names(df))

   # Check that there are no missing data in the study columns (except time to exposure)
   cc_sum <- sum(stats::complete.cases(df[, c(id, event, time_to_event, exposure)]))
   if (cc_sum != NROW(df)) {
      stop(paste0("There are missing data in one of the study columns: ", id, ", ", event, ", ", time_to_event, ", ", exposure))
   }

   # Check time to exposure is missing just for when exposure is 0/F
   if (any(!is.na(df[df[, exposure] == 0L, time_to_exposure]))) {
      stop("Time to exposure should only be for patients who received the exposure at some time")
   }

   if (any(is.na(df[df[, exposure] == 1L, time_to_exposure]))) {
      stop("Time to exposure should be complete for patients who have exposure = 1")
   }

   # Check exposure is just 0/1 or T/F
   if (any(df[, exposure] != 0L & df[, exposure] != 1L)) {
      stop("Exposure should be 0/1 or T/F")
   }

   # Make sure the user did not pass the same column name twice
   if (NROW(unique(c(id, event, time_to_event, exposure, time_to_exposure))) != NROW(c(id, event, time_to_event, exposure, time_to_exposure))) {
      stop("You passed the same column name twice")
   }

   # Check that the respective columns are numeric
   checkmate::assert_numeric(df[, time_to_event])
   checkmate::assert_numeric(df[, time_to_exposure])
   checkmate::assert_true(class(df[,event]) %in% c("integer", "logical"))
   checkmate::assert_true(class(df[,exposure]) %in% c("integer", "logical"))
   checkmate::assert_true(class(df[,id]) %in% c("integer", "numeric", "character"))

   # Some protected names
   protected_names <- c("clone", "outcome", "fup_outcome", "censor", "fup_censor", 
                        "t_start", "t_stop", "time_id", "t_event", "weight_cox",
                        "p_uncens", "hazard", "lp")
   for (name in protected_names) {
      if (name %in% names(df)) {
         stop("'", name, "' is a protected column name and will be used by the function. Please rename this column")
      }
   }

   # Check no outcomes are before exposure dates
   if (any(df[!is.na(df[,time_to_exposure]), time_to_event] < df[!is.na(df[,time_to_exposure]), time_to_exposure])) {
      stop("There are outcomes before exposure dates")
   }

   inputs_good <- TRUE 
   
   return(inputs_good)

}


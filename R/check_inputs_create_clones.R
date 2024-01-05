#' Check inputs to create clones
#' 
#' @param df A data.frame with one row per patient.
#' @param id The name of the column in `df` that contains the patient identifier.
#' @param event The name of the column in `df` that contains the event of interest.
#' @param time_to_event The name of the column in `df` that contains the time to event.
#' @param exposure The name of the column in `df` that contains the exposure.
#' @param time_to_exposure The name of the column in `df` that contains the time to exposure.
#' @param ced_windodw The date at which the clinical eligibility window closes. Can be left empty, in which case the clinical eligibility window is assumed to be part of 
#' `exposure` and `time_to_exposure`
#' 
#' @return TRUE if inputs are valid else false
check_inputs_create_clones <- function(
   df, 
   id,
   event,
   time_to_event,
   exposure,
   time_to_exposure,
   ced_window = NULL
) {

   inputs_good <- FALSE 

   # Check all input types 
   checkmate::assert_class(df, "data.frame")
   checkmate::assert_class(id, "character")
   checkmate::assert_class(event, "character")
   checkmate::assert_class(time_to_event, "character")
   checkmate::assert_class(exposure, "character")
   checkmate::assert_class(time_to_exposure, "character")
   checkmate::assert_class(ced_window, "numeric", null.ok = TRUE)

   # Check that all columns are in data
   checkmate::assert_subset(c(id, event, time_to_event, exposure, time_to_exposure), names(df))

   # Check that there are no missing data in the study columns (except time to exposure)
   cc_sum <- sum(complete.cases(df[, c(id, event, time_to_event, exposure)]))
   if (cc_sum != NROW(df)) {
      stop("There are missing data in the study columns")
   }

   # Check time to exposure is missing just for when exposure is 0/F
   if (any(!is.na(df[df[, exposure] == 0L, time_to_exposure]))) {
      stop("Time to exposure should only be for patients who received the exposure at some time")
   }

   if (any(is.na(df[df[, exposure] == 1L, time_to_exposure]))) {
      stop("Time to exposure should be complete for patients who have exposure = 1")
   }

   # Check that the data is one-row-per-patient
   if (NROW(unique(df[, id])) != NROW(df)) {
      stop("The data is not one-row-per-patient")
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


   inputs_good <- TRUE 
   
   return(inputs_good)

}


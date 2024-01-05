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
#' @param ced_windodw The date at which the clinical eligibility window closes. Can be left empty, in which case the clinical eligibility window is assumed to be part of 
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
#' clones <- create_clones(toy_df, id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2)
create_clones <- function(
   df, 
   id,
   event,
   time_to_event,
   exposure,
   time_to_exposure,
   ced_window = NULL
) {

   # Check all input types 
   checkmate::assert_class(df, "data.frame")
   checkmate::assert_class(id, "character", "pass the name of the column in `df` that contains the patient identifier")
   checkmate::assert_class(event, "character", "pass the name of the column in `df` that contains the event of interest")
   checkmate::assert_class(time_to_event, "character", "pass the name of the column in `df` that contains the time to event")
   checkmate::assert_class(exposure, "character", "pass the name of the column in `df` that contains the exposure")
   checkmate::assert_class(time_to_exposure, "character", "pass the name of the column in `df` that contains the time to exposure")
   checkmate::assert_class(ced_window, "numeric", "pass the date at which the clinical eligibility window closes. Can be left empty, in which case the clinical eligibility window is assumed to be part of `exposure` and `time_to_exposure`")

   # Check that all columns are in data
   checkmate::assert_subset(c(id, event, time_to_event, exposure, time_to_exposure), names(df))

   # Check that there are no missing data in the study columns
   df_cc <- complete.cases(df[, c(id, event, time_to_event, exposure, time_to_exposure)])
   if (NROW(df_cc) != NROW(df)) {
      stop("There are missing data in the study columns")
   }

   # Check that the data is one-row-per-patient
   if (NROW(unique(df[, id])) != NROW(df)) {
      stop("The data is not one-row-per-patient")
   }

   

}
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
create_clones <- function(
   df, 
   id,
   event,
   time_to_event,
   exposure,
   time_to_exposure,
   ced_window = NULL
) {

   valid_inputs <- create_clones_check_inputs(df = df, 
                                              id = id,
                                              event = event,
                                              time_to_event = time_to_event,
                                              exposure = exposure,
                                              time_to_exposure = time_to_exposure,
                                              ced_window = ced_window)

   if (!valid_inputs) stop("something went wrong")

   # 

   return(df)
   
   

}
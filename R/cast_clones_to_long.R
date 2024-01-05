#' Cast one-row-per-clone data to long format
#' 
#' @param df A data.frame with one row per clone as returned by [create_clones()]
#' 
#' @return A data.frame with one row per patient per time period.
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
#' 
#' clones_long <- cast_clones_to_long(clones)
cast_clones_to_long <- function(df) {

   # Check inputs
   checkmate::assert_class(df, "ccw_clones")
   if (!all(c("outcome", "fup_outcome", "censor", "fup_censor", "clone") %in% names(df))) {
      stop("The input data.frame is missing one of the required columns: outcome, fup_outcome, censor, fup_censor, clone. Did you remove this?")
   }
   if (!all(c("id", "event", "time_to_event", "exposure", "time_to_exposure", "ced_window") %in% names(attributes(df)))) {
      stop("The input data.frame is missing attributes: id, event, time_to_event, exposure, time_to_exposure, ced_window. Did you remove these or try to make a custom data.frame?")
   }

   # Now convert to long



   df_long <- df

   class(df_long) <- c("ccw_clones_long", class(df_long))

   return(df_long)
}

#' Generate clone censor weights (CCW) on the long data.frame
#' 
#' Currently, the only way to generate weights is via multivariable Cox, as described in Maringe et al. 2020
#' 
#' @param df A data.frame with one row per clone per observation period as returned by [cast_clones_to_long()]
#' @param predvar The variables that will be used to derive weights (subset of those in your data.frame originally)
#' 
#' @return The same data.frame with weights added.
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
#' clones_long_w <- generate_ccw_on_long_df(clones_long)
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.
generate_ccw_on_long_df <- function(df, predvar) {
   
}
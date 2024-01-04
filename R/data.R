#' Toy dataset from Maringe et al. (2020)
#'
#' A toy dataset provided in Maringe et al. (2020) to demonstrate the clone-censor weight approach.
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.
#'
#' @format ## `maringe_tab`
#' A data frame with 200 rows and 12 columns:
#' \describe{
#'   \item{id}{patient identifier}
#'   \item{fup_obs}{observed follow-up time (time to death or 1 year if censored alive)}
#'   \item{death}{observed event of interest (all-cause death) 1: dead, 0:alive}
#'   \item{timetosurgery}{time to surgery (NA if no surgery)}
#'   ...
#' }
#' @source <https://doi.org/10.1093/ije/dyaa057>
"maringe_tab"

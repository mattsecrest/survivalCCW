#' Toy dataset from Maringe et al. (2020)
#'
#' A toy dataset provided in Maringe et al. (2020) to demonstrate the clone-censor weight approach.
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.
#'
#' @format ## `toy_df`
#' A data frame with 200 rows and 12 columns:
#' \describe{
#'   \item{id}{patient identifier}
#'   \item{fup_obs}{observed follow-up time (time to death or 1 year if censored alive)}
#'   \item{death}{observed event of interest (all-cause death) 1: dead, 0:alive}
#'   \item{timetosurgery}{time to surgery (NA if no surgery)}
#'   \item{age}{age at diagnosis}
#'   \item{sex}{patient's sex}
#'   \item{perf}{performance status at diagnosis}
#'   \item{stage}{stage at diagnosis}
#'   \item{deprivation}{deprivation score}
#'   \item{charlson}{Charlson's comorbidity index}
#'   \item{emergency}{route to diagnosis}
#' }
#' @source <https://doi.org/10.1093/ije/dyaa057>
"toy_df"

#' Testing data for `create_clones`
#'
#' The clones dataset from Maringe et al. (2020) named `tab`
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.

#' @source <https://doi.org/10.1093/ije/dyaa057>
"tab"

#' Testing data for `cast_clones_to_long`
#'
#' The long clones dataset from Maringe et al. (2020) named `data_final`
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.

#' @source <https://doi.org/10.1093/ije/dyaa057>
"data_final"

#' Testing data for `cast_clones_to_long`
#'
#' The long clones dataset from Maringe et al. (2020) named `data_final`
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.

#' @source <https://doi.org/10.1093/ije/dyaa057>
"data_final"

#' Testing data for `generate_ccw_calc_weights`, surgery recipients
#'
#' The long clones dataset with weights for surgery recipients from Maringe et al. (2020) named `data_long`
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.

#' @source <https://doi.org/10.1093/ije/dyaa057>
"data_long"

#' Testing data for `generate_ccw_calc_weights`, no surgery recipients
#'
#' The long clones dataset with weights for no surgery recipients from Maringe et al. (2020) named `data_long_2`
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.

#' @source <https://doi.org/10.1093/ije/dyaa057>
"data_long_2"

#' Testing data for `generate_ccw`
#'
#' The long clones dataset with weights for all recipients from Maringe et al. (2020) named `data_long_cox`
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.

#' @source <https://doi.org/10.1093/ije/dyaa057>
"data_long_cox"
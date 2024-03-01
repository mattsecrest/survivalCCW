#' Generate clone censor weights (CCW) on the long data.frame
#' 
#' Currently, the only way to generate weights is via multivariable Cox, as described in Maringe et al. 2020
#' 
#' @param df A data.frame with one row per clone per observation period as returned by [cast_clones_to_long()]
#' @param predvars The variables that will be used to derive weights (subset of those in your data.frame originally). At least one covariate must be used.
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
#' clones_long_w <- generate_ccw(clones_long, predvars = c("age"))
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.
generate_ccw <- function(df, predvars) {
   
   # Check inputs
   checkmate::assert_class(df, "ccw_clones_long")
   if (!all(c("outcome", "fup_outcome", "censor", "fup_censor", "clone", "t_start", "t_stop", "time_id", "t_event") %in% names(df))) {
      stop("The input data.frame is missing at least one of the required columns: outcome, fup_outcome, censor, fup_censor, clone, t_start, t_stop, time_id, t_event. Did you remove this?")
   }
   if (!all(c("id", "event", "time_to_event", "exposure", "time_to_exposure", "ced_window") %in% names(attributes(df)))) {
      stop("The input data.frame is missing at least one attribute: id, event, time_to_event, exposure, time_to_exposure, ced_window. Did you remove these or try to make a custom data.frame?")
   }

   id <- attributes(df)$id
   event <- attributes(df)$event
   exposure <- attributes(df)$exposure
   time_to_event <- attributes(df)$time_to_event
   time_to_exposure <- attributes(df)$time_to_exposure
   ced_window <- attributes(df)$ced_window
   event_times_df <- attributes(df)$event_times_df

   # Check predvars to make sure the columns are there
   if (!all(predvars %in% names(df))) {
      stop("At least one of these predvars columns is not on the data.frame: ", paste(predvars, collapse = ", "), ".")
   }

   # Make sure predvars is not NULL
   if (is.null(predvars)) {
      stop("predvars cannot be NULL. Please specify at least one variable to use for weights.")
   }

   # Make sure no predvars are character/factor
   if (any(sapply(df[, predvars], is.character) | sapply(df[, predvars], is.factor))) {
      stop("At least one of the predvars columns is character/factor. In this early version of `survivalCCW`, only numeric variables are considered. Please make dummy vars on your own! :)")
   }

   # Create weights
   df_1 <- calc_weights(df[df$clone == 1L, ], event_times_df, predvars)

   df_0 <- calc_weights(df[df$clone == 0L, ], event_times_df, predvars)

   # Combine 
   df <- rbind(df_0, df_1)

   # Check that all clones have weights
   if (any(is.na(df$weight_cox))) {
      stop("At least one clone is missing a weight. Please file a bug fix.")
   }

   # Update class
   class(df) <- c("ccw_clones_long_weights", class(df))

   # Remove rownames
   rownames(df) <- NULL

   return(df)

}

#' Calculate weights for each arm of a long data.frame
#' 
#' @param df the data.frame for a single arm
#' @param event_times_df the event times data.frame
#' @param predvars the baseline variables for adjustment
#' 
#' @return a data.frame with weight columns included
calc_weights <- function(df,  event_times_df, predvars) {

   id <- attributes(df)$id

   # Make sure no plus sign in input
   if (any(grepl("\\+", predvars))) {
      stop("No plus signs allowed in column names -- this impacts the formula creation")
   }

   model_fmla <- stats::as.formula(
      paste0(
         "survival::Surv(t_start, t_stop, censor) ~ ",
         paste(predvars, collapse = " + ")
      )
   )

   cens_model <- survival::coxph(model_fmla, data = df, ties = "efron")

   #@TODO allow factors and carry forward through previous functions
   # ref_df <- setNames(data.frame(matrix(0, nrow = 1, ncol = length(predvars))), predvars)
   df$lp <- as.matrix(df[, predvars]) %*% stats::coef(cens_model)
   baseline_hazard <- data.frame(
      survival::basehaz(cens_model, centered = FALSE)
   )
   names(baseline_hazard) <- c("hazard", "t")
   
   dat_base_times <- unique(
      merge(
         x = baseline_hazard,
         y = event_times_df,
         by.x = "t",
         by.y = "t_event",
         all.x = TRUE
      )
   )

   df <- merge(
      x = df, 
      y = dat_base_times,
      by = "time_id",
      all.x = TRUE
   )

   df <- df[order(df[, id], df$fup_outcome),]
   df$hazard <- ifelse(is.na(df$hazard), 0, df$hazard)
   df$p_uncens <- exp(-(df$hazard) * exp(df$lp))
   df$weight_cox  <- 1 / df$p_uncens
   df$weight_cox[df$time_id == 0] <- 1

   row.names(df) <- NULL   
   return(df)
}
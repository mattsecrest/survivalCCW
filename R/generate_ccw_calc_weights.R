#' Calculate weights for each arm of a long data.frame
#' 
#' @param df the data.frame for a single arm
#' @param model_fmla the formula for the Cox model
#' @param event_times_df the event times data.frame
#' @param predvars the baseline variables for adjustment
#' 
#' @return a data.frame with weight columns included
generate_ccw_calc_weights <- function(df, model_fmla, event_times_df, predvars) {

   cens_model <- survival::coxph(model_fmla, data = df, ties = "efron")

   #@TODO allow factors and carry forward through previous functions
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

   df <- df[order(df$id,df$fup_outcome),]
   df$hazard <- ifelse(is.na(df$hazard), 0, df$hazard)
   df$p_uncens <- exp(-(df$hazard) * exp(df$lp))
   df$weight_cox  <- 1 / df$p_uncens
   
   return(df)
}
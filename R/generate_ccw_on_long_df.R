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
#' clones_long_w <- generate_ccw_on_long_df(clones_long)
#' 
#' @references Maringe, Camille, et al. "Reflection on modern methods: trial emulation in the presence of immortal-time bias. Assessing the benefit of major surgery for elderly lung cancer patients using observational data." International journal of epidemiology 49.5 (2020): 1719-1729.
generate_ccw_on_long_df <- function(df, predvars) {
   
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

   # Now create weights
   model_fmla <- as.formula(
      paste0(
         "survival::Surv(t_start, t_stop, censor) ~ ",
         paste(predvars, collapse = " + ")
      )
   )

   cens_model <- survival::coxph(model_fmla, data = df)

   #@TODO allow factors and carry forward through previous functions
   df$lp <- as.matrix(df[, predvars]) %*% coef(cens_model)
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

   ### LEFT OFF HERE
   df<-df[order(df$id,df$fup_outcome),]
   df$hazard<-ifelse(is.na(df$hazard),0,df$hazard)
   df$their_p_uncens <-exp(-(df$hazard)*exp(df$their_lp))  
   
   
   #Estimating the probability of remaining uncensored at each time of event
   data.long$P_uncens<-exp(-(data.long$hazard)*exp(data.long$lin_pred))  



   their_dl_merge <- merge(their_lp, event_times_df, by.x = )

   lp <- predict(cens_model, newdata = df[, predvars, drop = FALSE], type = "lp")



   dd<-data.frame(survival::basehaz(cens_model,centered=F))
   names(dd)<-c("hazard","t")
   dat.base<-unique(merge(dat.base,times,by.x="t",by.y="tevent",all.x=T))

}
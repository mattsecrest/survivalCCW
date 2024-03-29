% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_clones_check_inputs.R
\name{create_clones_check_inputs}
\alias{create_clones_check_inputs}
\title{Check inputs to create clones}
\usage{
create_clones_check_inputs(
  df,
  id,
  event,
  time_to_event,
  exposure,
  time_to_exposure,
  ced_window
)
}
\arguments{
\item{df}{A data.frame with one row per patient.}

\item{id}{The name of the column in \code{df} that contains the patient identifier.}

\item{event}{The name of the column in \code{df} that contains the event of interest.}

\item{time_to_event}{The name of the column in \code{df} that contains the time to event.}

\item{exposure}{The name of the column in \code{df} that contains the exposure.}

\item{time_to_exposure}{The name of the column in \code{df} that contains the time to exposure.}

\item{ced_window}{The date at which the clinical eligibility window closes.
\code{exposure} and \code{time_to_exposure}}
}
\value{
TRUE if inputs are valid else false
}
\description{
Check inputs to create clones
}

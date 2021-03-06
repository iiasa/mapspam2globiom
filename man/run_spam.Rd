% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_spam.R
\name{run_spam}
\alias{run_spam}
\title{Runs SPAMc at the set administrative unit level}
\usage{
run_spam(param, out = TRUE)
}
\arguments{
\item{param}{Object of type \code{spam_par} that bundles all SPAM parameters,
including core model folders, alpha-3 country code, year, spatial
resolution, most detailed level at which subnational statistics are
available, administrative unit level at which the model is solved and type of
model.}

\item{out}{logical; should the GAMS model log be send to the screen as output?}
}
\description{
Run the selected model (min_entropy or max_score) and runs it in
GAMS. If \code{model_sel = 1}, the model is run for each individual
administrative unit at level 1. If \code{model_sel = 0} the model is run only
once for the total country. Selecting \code{out = TRUE} (default setting), the
model log will be sent to the screen after the model run is finished. The
log is as text file, whicn names starts with \code{model_log_} and is saved in
the \code{processed_data/intermediate_output} folder.  Note that, depending on
the size of the country and the selected resolution, the model might take a
lot of time to run. If the model is very large, there is a risk your
computer runs out of memory and an error message will be printed in the
model log.
}
\examples{
\dontrun{
run_spam(param, out = FALSE)
}

}

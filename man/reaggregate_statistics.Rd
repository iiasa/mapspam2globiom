% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reaggregate_statistics.R
\name{reaggregate_statistics}
\alias{reaggregate_statistics}
\title{Reaggregates subnational statistics from the bottom up so they are consistent}
\usage{
reaggregate_statistics(df, param)
}
\arguments{
\item{df}{tbl or data.frame}

\item{param}{Object of type \code{spam_par} that bundles all SPAM parameters,
including core model folders, alpha-3 country code, year, spatial
resolution, most detailed level at which subnational statistics are
available, administrative unit level at which the model is solved and type of
model.}
}
\value{
same class as \code{df}.
}
\description{
Reaggregates subnational statistics from the bottom up so they are consistent
}
\examples{

}

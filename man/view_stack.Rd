% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_stack.R
\name{view_stack}
\alias{view_stack}
\title{Show crop distributions maps using a stack for each farming system}
\usage{
view_stack(crp, var, param, viewer = TRUE, polygon = TRUE)
}
\arguments{
\item{crp}{Character. Crop for which the maps are shows. \code{crp}  has to be
one of the SPAMc four letter crop codes.}

\item{var}{Character. The variable to be plotted. \code{var} has to be physical
area (\code{"pa"}) or harvested area (\code{"ha"}).}

\item{param}{Object of type \code{spam_par} that bundles all SPAM parameters,
including core model folders, alpha-3 country code, year, spatial
resolution, most detailed level at which subnational statistics are
available, administrative unit level at which the model is solved and type of
model.}

\item{viewer}{Logical. Should the default web browers be used to show the
maps? \code{FALSE} will show the maps in the RStudio viewer pane.}

\item{polygon}{Logical; should the country polygon be overlayed?}
}
\description{
To quickly inspect the SPAMc results, \code{view_stack} shows crop
distribution maps for a selected crop stacking the maps for each farming
system. The maps are visualized using \href{https://leafletjs.com/}{leafletjs},
which makes it possible to select a number of background tiles (e.g.
OpenStreetMap).
}
\examples{
\dontrun{ view_stack(crop = "maiz", var = "ha", viewer = FALSE,
 polygon = FALSE) }

}

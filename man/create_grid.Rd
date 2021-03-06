% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_grid.R
\name{create_grid}
\alias{create_grid}
\title{Create spam country grid}
\usage{
create_grid(param = NULL)
}
\arguments{
\item{param}{Object of type \code{spam_par} that bundles all SPAM parameters,
including core model folders, alpha-3 country code, year, spatial
resolution, most detailed level at which subnational statistics are
available, administrative unit level at which the model is solved and type of
model.}
}
\description{
Creates the spatial grid that is used by SPAM to allocate physical area shares
for each crop and system. The border of the country is used as mask to
determine the grid and each grid is given a gridID number.
}
\details{
For technical reasons, gridID values are set before the raster is masked with
the country border, which means they are unique but non consecutive. Note that
grids at a resolution of 30 arcsec can become very large and might make some
time to create. The file is saved in \verb{/processed_data/maps/grid/}
}
\examples{
\dontrun{
create_grid(param)
}

}

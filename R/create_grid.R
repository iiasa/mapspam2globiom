#'Create spam country grid
#'
#'Creates the spatial grid that is used by SPAM to allocate physical area shares
#'for each crop and system. The border of the country is used as mask to
#'determine the grid and each grid is given a gridID number.
#'
#'Only two resolutions are allowed for `create_grid`: 5 arcmin and 30 arcsec,
#'which are set by `res`. For technical reasons, gridID values are set before
#'the raster is masked with the country border, which means they are unique but
#'non consecutive. The crs can be set to reproject the raster. The default
#'projection is WGS 84 (+proj=longlat +datum=WGS84 +no_defs). Note that creating
#'a SPAM grid at a resolution of 30 arcsec can be very large and might make some
#'time to create, in particular if it also has to be reprojected.
#'
#'@param res character with the resolution of SPAM. Accepted inputs are "5min"
#'  (default) and "30sec".
#'@param border sf or SpatialPolygonsDataFrame object with the country border of the SPAM target country.
#'@param crs coordinate reference system: integer with the EPSG code, or
#'  character with proj4string. The default is WGS 84 (+proj=longlat
#'  +datum=WGS84 +no_defs).
#'
#'@return RasterLayer
#'
#'@examples
#'\dontrun{
#'create_grid(res = "5min", border = adm, crs = "+proj=longlat +datum=WGS84 +no_defs")
#'}
#'@export
create_grid <- function(res = "5min", border, crs = "+proj=longlat +datum=WGS84 +no_defs"){
  if(res == "5min") {
    grid_fact <- 12
    message(glue::glue("Resolution is {res}"))
  } else if (res == "30sec"){
    grid_fact <- 120
    message(glue::glue("Resolution is {res}"))
  } else {
    stop("Resolution is not 5min or 30sec.")
  }

  # Create grid masked to country
  grid <- raster::raster() # 1 degree raster
  grid <- raster::disaggregate(grid, fact = grid_fact)
  grid <- raster::crop(grid, border)
  values(grid) <- 1:ncell(grid) # Add ID numbers
  grid <- raster::mask(grid, border)
  grid <- raster::projectRaster(grid, crs = crs)
  names(grid) <- "gridID"
  return(grid)
}

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
#'projection is WGS 84. Note that creating a SPAM grid at a resolution of 30
#'arcsec can be very large and might make some time to create, in particular if
#'it also has to be reprojected.
#'
#'@param param
#'@inheritParams create_spam_folders
#'
#'@return RasterLayer
#'
#'@examples
#'\dontrun{
#'create_grid(spam_par = spam_par, border = adm, crs = "+init=epsg:4326")
#'}
#'@export
create_grid <- function(param = NULL){

  load_data("adm_map", param, mess = FALSE, local = TRUE)
  stopifnot(inherits(param, "spam_par"))
  if(param$res == "5min") {
    grid_fact <- 12
    message(glue::glue("Resolution is {param$res}"))
  } else if (param$res == "30sec"){
    grid_fact <- 120
    message(glue::glue("Resolution is {param$res}"))
  }

  # Create grid masked to country using +init=epsg:4326 and then reproject to
  # user set crs
  grid <- raster::raster() # 1 degree raster
  grid <- raster::disaggregate(grid, fact = grid_fact)
  adm_map <- adm_map %>%
    sf::st_transform(crs = "+init=epsg:4326")
  grid <- raster::crop(grid, adm_map)
  values(grid) <- 1:ncell(grid) # Add ID numbers
  grid <- raster::mask(grid, adm_map)
  grid <- trim(grid)
  names(grid) <- "gridID"

  temp_path <- file.path(param$spam_path, glue::glue("processed_data/maps/grid/{param$res}"))
  dir.create(temp_path, showWarnings = F, recursive = T)
  writeRaster(grid, file.path(temp_path, glue::glue("grid_{param$res}_{param$year}_{param$iso3c}.tif")),
              overwrite = T)
}

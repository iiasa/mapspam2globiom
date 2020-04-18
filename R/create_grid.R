#' Create spam country grid
#'
#'
create_grid <- function(res, adm){
  if(res == "5min") {
    grid_fact <- 12
    message(glue("Resolution is {grid}"))
  } else if (res == "30sec"){
    grid_fact <- 120
    message(glue("resolution is {grid}"))
  } else {
    stop("grid is not 5min or 30sec.")
  }

  # Create grid masked to country
  grid <- raster::raster() # 1 degree raster
  grid <- raster::disaggregate(grid, fact = grid_fact)
  grid <- raster::crop(grid, adm)
  values(grid) <- 1:ncell(grid) # Add ID numbers
  grid <- raster::mask(grid, adm)
  names(grid) <- "gridID"
  return(grid)
}

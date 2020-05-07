#' Function to compare crop maps per system in a panel
#'
#' @export
view_4panel <- function(crp, vr, sy = c("S", "L", "H", "I"), param){

  load_data(c("grid", "results"), param, mess = FALSE, local = TRUE)
  ext <- raster::extent(grid)
  df <- df %>%
    dplyr::filter(crop == crp, system %in% sy) %>%
    dplyr::left_join(grid_df)
  sys <- unique(df$system)
  st <- lapply(sys, function(x) raster::rasterFromXYZ(df[df$system == x, c("x", "y", vr)], crs = crs(grid)))
  st <- lapply(st, function(x) raster::extend(x, ext)) # Harmonize exent for stacking
  st <- lapply(seq(length(st)), function(i){
    mapview::mapview(st[[i]], layer.name = paste(crp, sys[i], sep = "_"))
  })
  leafsync::sync(st)
}

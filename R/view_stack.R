#' Compare all farming systems of a selected crop in the browser using stack
#'
#' @export
view_stack <- function(crp, vr, sy = c("S", "L", "H", "I")){
  ext <- raster::extent(grid)
  df <- df %>%
    dplyr::filter(crop == crp, system %in% sy) %>%
    dplyr::left_join(grid_df)
  sys <- unique(df$system)
  st <- lapply(sys, function(x) raster::rasterFromXYZ(df[df$system == x, c("x", "y", vr)], crs = crs(grid)))
  st <- lapply(st, function(x) raster::extend(x, ext)) # Harmonize exent for stacking
  if(length(sys) >1){
    st <- raster::stack(st)
  }else{
    st <- st[[1]]
  }
  names(st) <- paste(crp, sys, vr, sep = "_")
  st[st==0] <- NA
  mapview(st, use.layer.names = T)
}

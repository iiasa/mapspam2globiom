# Process gaez
process_gaez <- function(file, adm_code, param) {

  load_intermediate_data(c("grid", "cl_harm"), adm_code, param, local = TRUE, mess = FALSE)
  r <- raster::raster(file)
  names(r) <- "value"

  cs <- basename(file)
  cs <- unlist(lapply(strsplit(cs, "_"), function(x) paste(x[1], x[2], sep="_")))
  cat("\n", cs)

  # Combine with grid, select only relevant gridID and add crop_system
  df <- as.data.frame(raster::rasterToPoints(raster::stack(grid, r))) %>%
    dplyr::filter(gridID %in% cl_harm$gridID) %>%
    dplyr::mutate(crop_system = cs)

  # Fix inconsistencies. Set any negative (some files have very small negative
  # values) and NA values to zero
  df <- df %>%
    dplyr::mutate(value = ifelse(is.na(value) | value < 0, 0, value))

  return(df)
}

file <- lookup$files_full[lookup$crop_system == "rice_I"]

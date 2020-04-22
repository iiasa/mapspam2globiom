#'Prepares the synergy irrigated area for SPAM
#'
#'@param adm_code vector with administrative unit codes. This is either a single value
#'  equal to the country alpha-3 country code if model solve level is set to 0
#'  or a vector with all level 1 administrative unit codes if the model solve
#'  level is set to 1.
#'@param ha tbl or data.frame with harvest area statistics
#'@param fs tbl or data.frame with farming system shares
#'@param ci tbl or data.frame with cropping intensity statistics
#'@param param
#'@inheritParams create_grid
#'
#'@examples
#'
#'@export

prepare_ir_area <- function(param){
  # load data
  load_data(c("adm_map_r", "adm_list","ia_max", "ia_rank", "grid"), param, local = TRUE)

  # Grid size
  grid_size <- raster::area(grid)
  grid_size <- grid_size * 100 # in ha
  names(grid_size) <- "grid_size"

  # Fix inconsistencies
  # Set ia_max to grid_size if it is larger than grid_size
  df <- df %>%
    dplyr::mutate(ia_max = ifelse(grid_size < ia_max, grid_size, ia_max))

  # Combine and remove cells where gridID is missing
  df <- as.data.frame(raster::rasterToPoints(raster::stack(grid, ia_rank, ia_max, grid_size))) %>%
    dplyr::filter(!is.na(gridID))

  # Remove gridID where ia_rank is NA
  df <- df %>%
    dplyr::filter(!is.na(ia_rank))

  # Set adm_level
  if(param$solve_level == 0) {
    adm_code_list <- unique(adm_list$adm0_code)
  } else {
    adm_code_list <- unique(adm_list$adm1_code)
  }

  # Save in line with solve level
  purrr::walk(adm_code_list, split_spatial, ia_df, "ia", adm_map_r, param)
}


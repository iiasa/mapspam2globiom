#'Harmonizes the subnational statistics, the cropland extent and the irrigated area
#'
#'@param param
#'@inheritParams create_grid
#'
#'@examples
#'
#'@import magrittr
#'@export

# Function to harmonize pa cl and ir according to solve_sel
harmonize_inputs <- function(param){

  load_data(c("adm_list", local = TRUE))
  if(param$solve_level == 0) {
    adm_code_list <- unique(adm_list$adm0_code)
  } else {
    adm_code_list <- unique(adm_list$adm1_code)
  }

  purrr::walk(adm_code_list, split_harm, param)
}

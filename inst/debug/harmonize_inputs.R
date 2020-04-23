
# Function to harmonize pa cl and ir according to solve_sel
harmonize_inputs <- function(adm_code_sel, param){

  if(param$solve_level == 0) {
    adm_code_list <- unique(adm_list$adm0_code)
  } else {
    adm_code_list <- unique(adm_list$adm1_code)
  }

  purrr::walk(adm_code_list, split_harm, param)
}

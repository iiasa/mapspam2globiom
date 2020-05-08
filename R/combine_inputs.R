#'@export
combine_inputs <- function(param) {
  stopifnot(inherits(param, "spam_par"))
  cat("\n\n############### COMBINE INPUTS ###############")
  load_data("adm_list", param, local = TRUE, mess = FALSE)

  # Set adm_level
  if(param$solve_level == 0) {
    adm_code_list <- unique(adm_list$adm0_code)
  } else {
    adm_code_list <- unique(adm_list$adm1_code)
  }

  purrr::walk(adm_code_list, combine_inputs_adm_level, param)
}



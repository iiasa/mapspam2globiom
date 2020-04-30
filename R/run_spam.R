#'@export
run_spam <- function(param) {
  load_data("adm_list", param, local = TRUE, mess = FALSE)

  # Set adm_level
  if(param$solve_level == 0) {
    adm_code_list <- unique(adm_list$adm0_code)
  } else {
    adm_code_list <- unique(adm_list$adm1_code)
  }

  if(param$model == "max_score") {
    purrr::walk(adm_code_list, run_gams_adm_level, param)
  } else {
    if(param$model == "min_entropy") {
      message("This model is not implemented yet")
      stop("Stop process")
    } else {
      message("This model is not implemented yet")
      stop("Stop process")
    }
  }
}




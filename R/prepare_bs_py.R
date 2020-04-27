prepare_bs_yg <- function(var, param) {
  load_data("adm_list", param, local = TRUE, mess = FALSE)

  # Set adm_level
  if(param$solve_level == 0) {
    adm_code_list <- unique(adm_list$adm0_code)
  } else {
    adm_code_list <- unique(adm_list$adm1_code)
  }


}

adm_code <- "MI02"
var <-"biophysical_suitability"





#'Function that iterates over adm level starting with the most detailed and
#'update cl so it is in line with pa.
update_cl <- function(df, param) {
  if (param$solve_level == 0) {
    for (i in param$adm_level:0) {
      problem_adm <- check_cl(df, i)
      df <- replace_cl(df, problem_adm, i)
    }
  }
  if (param$solve_level == 1) {
    for (i in seq_along(0:param$adm_level)) {
      vec_id <- c(2, 3)
      vec_adm_level <- c(2, 1)
      problem_adm <- check_cl(df, i)
      df <-
        replace_cl(df, problem_adm, i)
    }
  }
  return(df)
}

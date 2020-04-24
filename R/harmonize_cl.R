#'Function that iterates over adm level starting with the most detailed and
#'update cl so it is in line with pa.
harmonize_cl <- function(df, param) {
  if (param$solve_level == 0) {
    for (i in param$adm_level:0) {
      problem_adm <- check_cl(df, i)
      df <- update_cl(df, problem_adm, i)
    }
  }
  if (param$solve_level == 1) {
    for (i in seq_along(0:param$adm_level)) {
      problem_adm <- check_cl(df, i)
      df <-
        update_cl(df, problem_adm, i)
    }
  }
  return(df)
}

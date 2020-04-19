#'Validates spam_par class
#'
#'@param spam_par
#'@inheritParams create_grid
#'
#'@examples
#'\dontrun{}
#'@export
validate_spam_par <- function(spam_par) {
    if (is.null(spam_par$spam_path))
      stop("spam_path is not defined",
           call. = FALSE)

    if (is.null(spam_par$iso3c)) {
      stop("iso3c not defined",
           call. = FALSE)
    } else {
      if(!grepl("^[A-Z]{3}$", spam_par$iso3c)) {
        stop("iso3c is not a three letter character",
             call. = FALSE)
      }
    }
    if (is.null(spam_par$year)) {
      stop("year is not defined",
           call. = FALSE)
    } else {
      if(!is.numeric(spam_par$year)) {
        stop("year is not a value",
             call. = FALSE)
      } else {
        if(spam_par$year < 1000 | spam_par$year > 2300) {
          message("year seems to have an unrealistic value")
        }
      }
    }
    if (!spam_par$res %in% c("5min", "30sec"))
      stop("5min and 30sec are allowed values for res",
           call. = FALSE)
    if (!spam_par$adm_level %in% c(0, 1, 2))
      stop("0, 1, 2, are allowed values for adm_level",
           call. = FALSE)
    if (!spam_par$solve_level %in% c(0, 1))
      stop("0, 1 are only allowed values for solve_level",
           call. = FALSE)
    if (!spam_par$model %in% c("max_score", "min_entropy"))
      stop("max_score and min_entropy are allowed values for model",
           call. = FALSE)
}


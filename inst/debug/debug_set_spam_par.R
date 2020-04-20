spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
iso3c = "MWI", year = 2010, res = "5min", adm_level = 1,
solve_level = 0, model = "max_score",
crs = "+proj=longlat +datum=WGS84 +no_defs")

spam_par <-
  function(spam_path = NULL,
           raw_path = NULL,
           iso3c = NULL,
           year = NULL,
           res = "5min",
           adm_level = 1,
           solve_level = 0,
           model = "max_score",
           crs = "+proj=longlat +datum=WGS84 +no_defs") {

    if (is.null(raw_path)) {
      message("raw_path is not defined, set to raw_data in main folder")
      raw_path <- file.path(spam_path, "raw_data")
    }

    param <- list(
      iso3c = ifelse(!is.null(iso3c), toupper(iso3c), NA_character_),
      country = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "country.name"), NA_character_),
      iso3n = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "iso3n"), NA_character_),
      fao_code = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "fao"), NA_character_),
      continent = ifelse(!is.null(iso3c), countrycode::countrycode(iso3c, "iso3c", "continent"), NA_character_),
      year = year,
      resolution = res,
      adm_level = adm_level,
      solve_level = solve_level,
      model = model,
      spam_path = spam_path,
      raw_path = raw_path,
      crs = crs)
    class(param) <- "spam_par"
    validate_spam_par(param)
    return(par)
  }

print(param)


validate_spam_par <- function(param) {
  stopifnot(inherits(param, "spam_par"))
  if (is.null(param$spam_path))
    stop("spam_path is not defined",
         call. = FALSE)
  if (is.na(param$iso3c)) {
    stop("iso3c not defined",
         call. = FALSE)
  } else {
    if(!grepl("^[A-Z]{3}$", param$iso3c)) {
      stop("iso3c is not a three letter character",
           call. = FALSE)
    }
  }
  if (is.null(param$year)) {
    stop("year is not defined",
         call. = FALSE)
  } else {
    if(!is.numeric(param$year)) {
      stop("year is not a value",
           call. = FALSE)
    } else {
      if(param$year < 1000 | param$year > 2300) {
        message("year seems to have an unrealistic value")
      }
    }
  }
  if (!param$res %in% c("5min", "30sec"))
    stop("5min and 30sec are allowed values for res",
         call. = FALSE)
  if (!param$adm_level %in% c(0, 1, 2))
    stop("0, 1, 2, are allowed values for adm_level",
         call. = FALSE)
  if (!param$solve_level %in% c(0, 1))
    stop("0, 1 are allowed values for solve_level",
         call. = FALSE)
  if (!param$model %in% c("max_score", "min_entropy"))
    stop("max_score and min_entropy are allowed values for model",
         call. = FALSE)
}


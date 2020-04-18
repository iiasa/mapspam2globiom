#'Sets spam parameters
#'
#'`set_spam_par` sets all required parameters for spam to run, including: model
#'folders, country, year, resolution, administrative unit information, solve
#'level and model type.
#'
#'`set_spam_par` creates an object of class `spam_par`, which bundles all
#'required spam parameters set by the user, including: core model folders,
#'country, year, resolution, depth of administrative units for which subnational
#'statistics are available, level at which the model is solved and type of
#'model.
#'
#'\code{\link{create_spam_folders}} needs to be run before as it creates all
#'core folders, including the `parameters` folder where `set_spam_par` output is
#'stored. An error will be thrown if these essential folders do not exist when
#'the function is run.
#'
#'\code{\link[countrycode]{countrycode}} is used to determine the full country
#'name, three digit country code, three digit FAO country code and continent on
#'the basis of the alpha-3 country code. This information is required to extract
#'country specific information from several datasets.
#'
#'@param spam_path character string with the main SPAM folder. Note that R uses
#'  forward slash or double backslash to separate folder names.
#'@param raw_path character string with the raw data folder. This makes it
#'  possible to store the raw data on a server. If `raw_path` is not specified
#'  it is automatically set to the default raw data folder in the main model
#'  folder.
#'@param iso3c character string with the three letter ISO 3166-1 alpha-3 country
#'  code, also referred to as iso3c. A list of country codes can be found in
#'  [Wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3).
#'@param year numeric with the reference year for SPAM.
#'@param res character with the resolution of SPAM. Accepted inputs are "5min"
#'  (default) and "30sec".
#'@param adm_level integer with the level up to which subnational statistics are
#'  available. Accepted inputs are 0 (only national level data), 1 (national
#'  level and first level administrative unit - default) and 2 (national level,
#'  first and second level administrative unit).
#'@param solve_level integer that indicates the level at which the model is
#'  solved. Accepted inputs are 0 (model is run at national level - default) and
#'  1 (model is solved for each first level administrative unit separately).
#'  level and first level administrative unit - default)
#'@param model character that specifies the type of model that is run. Accepted
#'  inputs are "max_score" and "min_entropy". See package documentation for more
#'  information.
#'@param crs coordinate reference system: integer with the EPSG code, or
#'  character with proj4string.

#'
#'@return spam_par object
#'
#'@examples
#'\dontrun{
#'set_spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
#'  iso3c = "MWI", year = 2010, res = "5min", adm_level = 1,
#'  solve_level = 0, model = "max_score", crs = "+proj=longlat +datum=WGS84 +no_defs")
#'}
#'#'@export
set_spam_par <-
    function(spam_path = NULL,
             raw_path = NULL,
             iso3c = NULL,
             year = NULL,
             res = "5min",
             adm_level = 1,
             solve_level = 0,
             model = "max_score",
             crs = "+proj=longlat +datum=WGS84 +no_defs") {

        if (is.null(spam_path))
            stop("spam_path is not defined.")

        raw_path <- file.path(spam_path, "raw_data")
        par_path <- file.path(spam_path, "parameters")

        if (is.null(raw_path)) {
            message("raw_path is not defined, set to raw_data in main folder")
            raw_path <- file.path(spam_path, "raw_data")
        }
        if (is.null(iso3c)) {
            stop("iso3c not defined")
        } else {
            if(!grepl("^[a-zA-Z]{3}$", iso3c)) {
                stop("iso3c is not a three letter character")
            }
        }
        if (is.null(year)) {
            stop("year is not defined")
        } else {
            if(!is.numeric(year)) {
                stop("year is not a value")
            } else {
                if(year < 1000 | year > 2300) {
                    message("year seems to have an unrealistic value")
                }
            }
        }

        if (!res %in% c("5min", "30sec"))
            stop("5min and 30sec are allowed values for res")
        if (!adm_level %in% c(0, 1, 2))
            stop("0, 1, 2, are allowed values for adm_level")
        if (!solve_level %in% c(0, 1))
            stop("0, 1 are only allowed values for solve_level")
        if (!model %in% c("max_score", "min_entropy"))
            stop("max_score and min_entropy are allowed values for model")

        par <- list(
            iso3c = toupper(iso3c),
            country = countrycode::countrycode(iso3c, "iso3c", "country.name"),
            iso3n = countrycode::countrycode(iso3c, "iso3c", "iso3n"),
            fao_code = countrycode::countrycode(iso3c, "iso3c", "fao"),
            continent =
                countrycode::countrycode(iso3c, "iso3c", "continent"),
            year = year,
            resolution = resolution,
            adm_level = adm_level,
            solve_level = solve_level,
            model = model,
            spam_path = spam_path,
            raw_path = raw_path,
            crs = crs)

        attr(par, 'class') <- 'spam_par'
        if (!all(dir.exists(c(raw_path, par_path)))) {
            stop("The model has not been built. Run build_model() first")
        }
        saveRDS(par, file.path(spam_path, "parameters/spam_par.rds"))
        return(par)
    }


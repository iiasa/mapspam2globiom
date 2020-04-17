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
#'@param iso3c_sel character string with the three letter ISO 3166-1 alpha-3
#'  country code, also referred to as iso3c. A list of country codes can be
#'  found in [Wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3).
#'@param year_sel numeric with the reference year for SPAM.
#'@param grid_sel character with the resolution of SPAM. Accepted inputs are
#'  "5min" (default) and "30sec".
#'@param adm_sel integer with the level up to which subnational statistics are
#'  available. Accepted inputs are 0 (only national level data), 1 (national
#'  level and first level administrative unit - default) and 2 (national level,
#'  first and second level administrative unit).
#'@param solve_sel integer that indicates the level at which the model is
#'  solved. Accepted inputs are 0 (model is run at national level - default) and
#'  1 (model is solved for each first level administrative unit separately).
#'  level and first level administrative unit - default)
#'@param model_sel character that specifies the type of model that is run.
#'  Accepted inputs are "max_score" and "min_entropy". See package documentation
#'  for more information.
#'
#'@return spam_par object
#'
#'@examples
#'\dontrun{
#'set_spam_par(spam_path = "C:/Users/dijk158/spam_mwi",
#'  iso3c_sel = "MWI", year_sel = 2010, grid_sel = "5min", adm_sel = 1,
#'  solve_sel = 0, model_sel = "max_score")
#'}
#'
#'@export
set_spam_par <-
    function(spam_path = NULL,
             raw_path = NULL,
             iso3c_sel = NULL,
             year_sel = NULL,
             grid_sel = "5min",
             adm_sel = 1,
             solve_sel = 0,
             model_sel = "max_score") {
        if (is.null(spam_path))
            stop("spam_path is not defined.")

        raw_path <- file.path(spam_path, "raw_data")
        proc_path <- file.path(spam_path, "processed_data")
        par_path <- file.path(spam_path, "parameters")

        if (is.null(raw_path)) {
            message("raw_path is not defined, set to raw_data in main folder.")
            raw_path <- file.path(spam_path, "raw_data")
        }
        if (is.null(iso3c_sel)) {
            stop("iso3c_sel not defined.")
        } else {
            if(!grepl("^[a-zA-Z]{3}$", iso3c_sel)) {
                stop("iso3c_sel is not a three letter character.")
            }
        }
        if (is.null(year_sel)) {
            stop("year_sel is not defined.")
        } else {
            if(!is.numeric(year_sel)) {
                stop("year_sel is not a value.")
            } else {
                if(year_sel < 1000 | year_sel > 2300) {
                    message("year_sel seems to have an unrealistic value")
                }
            }
        }

        if (!grid_sel %in% c("5min", "30sec"))
            stop("5min and 30sec are allowed values for grid_sel.")
        if (!adm_sel %in% c(0, 1, 2))
            stop("0, 1, 2, are allowed values for adm_sel.")
        if (!solve_sel %in% c(0, 1))
            stop("0, 1 are only allowed values for solve_sel.")
        if (!model_sel %in% c("max_score", "min_entropy"))
            stop("max_score and min_entropy are allowed values for solve_sel.")

        par <- list(
            iso3c_sel = toupper(iso3c_sel),
            country_sel = countrycode::countrycode(iso3c_sel, "iso3c", "country.name"),
            iso3n_sel = countrycode::countrycode(iso3c_sel, "iso3c", "iso3n"),
            fao_sel = countrycode::countrycode(iso3c_sel, "iso3c", "fao"),
            continent_sel <-
                countrycode::countrycode(iso3c_sel, "iso3c", "continent"),
            year_sel = year_sel,
            grid_sel = grid_sel,
            adm_sel = adm_sel,
            solve_sel = solve_sel,
            model_sel = model_sel,
            spam_path = spam_path,
            raw_path = raw_path,
            proc_path = proc_path,
            par_path = par_path
        )

        attr(par, 'class') <- 'spam_par'
        if (!all(dir.exists(c(raw_path, proc_path, par_path)))) {
            stop("The model has not been built. Run build_model() first.")
        }
        saveRDS(par, file.path(spam_path, "parameters/spam_par.rds"))
        return(par)
    }


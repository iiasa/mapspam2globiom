#'Set mapspam parameters.
#'
#'Sets all required parameters for mapspam to run, including: model folders,
#'country, year, resolution, adm level, solve level and model type.
#'
#'This function creates an object of class `mapspam_par`, which bundles all
#'required mapspam parameters set by the user, including: model folders,
#'country, year, resolution, adm level, solve level and model type.
#'
#'
#'If one of the parameters in `set_mapspam_par` is missing an error message will
#'thrown. The object is automatically saved as `mapspam_par.rds` in the
#'`parameters` folder. One needs to run `build_mapspam()` before as it creates
#'all essential folders, which are stored as parametres in `mapspam_par`,
#'including the `parameters` folder. An error will be thrown if these essential
#'folders do not exist.
#'
#'@param model_path a character string with the mapspam folder. Note that R uses
#'  forward slash or double backslash to separate folder names.
#'@param glob_raw_path a character string with the raw data folder for global
#'  data. This makes it possible to store large files, such as global cropland
#'  maps at high resolution, on a server, while country specific data can be
#'  stored locally in the raw data folder. If `glob_raw_path` is not specified
#'  it is automatically set to the raw data folder.
#'@param iso3c_sel iso3c code
#'@param year_sel year
#'@param grid_sel model resolution
#'@param adm_sel
#'@param solve_sel
#'@param model_sel
#'
#'@return mapspam_par object
#'
#' @examples
#'\dontrun{
#'set_mapspam_par(model_path = "C:/Users/dijk158/Dropbox/mapspam_dev",
#'  iso3c_sel = "MWI", year_sel = 2010, grid_sel = "5min", adm_sel = 1,
#'  solve_sel = 0, model_sel = "max_score")
#'}

set_mapspam_par <-
    function(model_path = NULL,
             glob_raw_path = NULL,
             iso3c_sel = NULL,
             year_sel = NULL,
             grid_sel = "5min",
             adm_sel = 1,
             solve_sel = 0,
             model_sel = "max_score") {
        if (is.null(path))
            path <- path.expand("~")
        raw_path <- file.path(path, "raw_data")
        proc_path <- file.path(path, "processed_data")
        par_path <- file.path(path, "parameters")

        if (is.null(glob_raw_path)) {
            message("glob_raw_path is not defined, set to same path as raw_data")
            glob_raw_path <- file.path(path, "raw_data")
        }
        if (is.null(iso3c_sel))
            stop("iso3c country code is not defined. Add iso3c_sel")
        if (is.null(year_sel))
            stop("Year is not defined. Add year_sel")
        if (!grid_sel %in% c("5min", "30sec"))
            stop("5min and 30sec are allowed values for grid_sel")
        if (!adm_sel %in% c(0, 1, 2))
            stop("0, 1, 2, are allowed values for adm_sel")
        if (!solve_sel %in% c(0, 1))
            stop("0, 1 are only allowed values for solve_sel")
        if (!model_sel %in% c("max_score", "min_entropy"))
            stop("max_score and min_entropy are allowed values for solve_sel")

        par <- list(
            model_path = path,
            iso3c_sel = iso3c_sel,
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
            mapspam_path = path,
            glob_raw_path = glob_raw_path,
            raw_path = raw_path,
            proc_path = proc_path,
            par_path = par_path
        )

        attr(par, 'class') <- 'mapspam_par'
        if (!all(dir.exists(c(raw_path, proc_path, par_path)))) {
            stop("The model has not been built. Run build_model() first.")
        }
        saveRDS(par, file.path(path, "parameters/mapspam_par.rds"))
        print.mapspam_par(par, full = T)
        return(par)
    }


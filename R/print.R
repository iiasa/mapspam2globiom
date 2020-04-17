#'@export

print.spam_par <- function(x, ...) {
    cat("iso3c: ", x$iso3c_sel, "\n")
    cat("year: ", x$year_sel, "\n")
    cat("resolution: ", x$grid_sel, "\n")
    cat("adm level: ", x$adm_sel, "\n")
    cat("solve level: ", x$solve_sel, "\n")
    cat("model: ", x$model_sel, "\n")
    cat("spam path: ", x$spam_path, "\n")
    cat("raw data path: ", x$raw_path, "\n")
    cat("processed data: ", x$proc_path, "\n")
    cat("parameter path: ", x$par_path, "\n")
    cat("country name: ", x$country_sel, "\n")
    cat("iso3n: ", x$iso3n_sel, "\n")
    cat("fao code: ", x$fao_sel, "\n")
    cat("continent: ", x$continent_sel, "\n")
}


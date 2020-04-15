#'@export

print.spam_par <- function(obj) {
    cat("iso3c: ", obj$iso3c_sel, "\n")
    cat("year: ", obj$year_sel, "\n")
    cat("resolution: ", obj$grid_sel, "\n")
    cat("adm level: ", obj$adm_sel, "\n")
    cat("solve level: ", obj$solve_sel, "\n")
    cat("model: ", obj$model_sel, "\n")
    cat("spam path: ", obj$spam_path, "\n")
    cat("global raw data path: ", obj$glob_raw_path, "\n")
    cat("raw data path: ", obj$raw_path, "\n")
    cat("processed data: ", obj$proc_path, "\n")
    cat("parameter path: ", obj$par_path, "\n")
    cat("country name: ", obj$country_sel, "\n")
    cat("iso3n: ", obj$iso3n_sel, "\n")
    cat("fao code: ", obj$fao_sel, "\n")
    cat("continent: ", obj$continent_sel, "\n")
}


#' Create rspam folder structure and main scripts.
#'
#' @param path
#' @return creates main mapspam folders and copies scripts into them
#' @examples
#' xbuild_rmapspam
#'
build_model <- function(path = NULL){
    if(is.null(path)) path <- getwd()
    mapspam_folders <- c("agricultural_statistics", "harmonization", "maps")
    invisible(lapply(mapspam_folders, function(x) dir.create(file.path(path, x), showWarnings = F, recursive = T)))

    scripts_path <- system.file("model_scripts", package = "mapspam2globiom")
    scripts <- list.files(scripts_path, full.names = T)
    catch <- file.copy(scripts, path, recursive = TRUE)
}


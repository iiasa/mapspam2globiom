#'Load input data for further processing
#'
#'@export
load_input <- function(fl, param){
  fl <- match.arg(fl, c("grid", "gia", "gmia"), several.ok = TRUE)

  load_list <- list()

  if("grid" %in% fl) {
    file <- file.path(param$spam_path,
                                     glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["grid"]] <- raster::raster(file)
      names(load_list[["grid"]]) <- "grid"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("gia" %in% fl) {
    file <- file.path(param$spam_path,
                      glue("processed_data/maps/irrigated_area/gia_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["gia"]] <- raster::raster(file)
      names(load_list[["grid"]]) <- "gia"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("gmia" %in% fl) {
    file <- file.path(param$spam_path,
                      glue("processed_data/maps/irrigated_area/gmia_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["gmia"]] <- raster::raster(file)
      names(load_list[["grid"]]) <- "gmia"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }
  message(glue("{fPaste(fl)} loaded"))
  invisible(list2env(load_list, envir = .GlobalEnv))
}

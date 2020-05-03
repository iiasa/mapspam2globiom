#'Load input data for further processing
#'
#'@export
load_data <- function(fl, param, local = FALSE, mess = T){
  fl <- match.arg(fl, c("adm_list", "adm_map", "adm_map_r",
                        "cl_med", "cl_max", "cl_rank",
                        "ia_max", "ia_rank",
                        "grid", "gia", "gmia",
                        "pop", "acc", "urb",
                        "simu_r",
                        "ha", "fs", "ci",
                        "price",
                        "dm2fm", "crop2globiom",
                        "results"),
                  several.ok = TRUE)

  load_list <- list()

  if("grid" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/grid/{param$res}/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["grid"]] <- raster::raster(file)
      names(load_list[["grid"]]) <- "gridID"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("gia" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/irrigated_area/{param$res}/gia_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["gia"]] <- raster::raster(file)
      names(load_list[["gia"]]) <- "gia"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("gmia" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/irrigated_area/{param$res}/gmia_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["gmia"]] <- raster::raster(file)
      names(load_list[["gmia"]]) <- "gmia"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("pop" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/population/{param$res}/pop_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["pop"]] <- raster::raster(file)
      names(load_list[["pop"]]) <- "pop"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("acc" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/accessibility/{param$res}/acc_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["acc"]] <- raster::raster(file)
      names(load_list[["acc"]]) <- "acc"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }


  if("urb" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/population/{param$res}/urb_{param$year}_{param$iso3c}.rds"))
    if(file.exists(file)) {
      load_list[["urb"]] <- readRDS(file)
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("price" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/agricultural_statistics/crop_prices_{param$year}_{param$iso3c}.csv"))
    if(file.exists(file)) {
      load_list[["price"]] <- suppressMessages(readr::read_csv(file))
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("dm2fm" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("parameters/mappings_spam.xlsx"))
    if(file.exists(file)) {
      load_list[["dm2fm"]] <- suppressMessages(readxl::read_excel(file, sheet = "gaez_dm2fm"))
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("crop2globiom" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("parameters/mappings_spam.xlsx"))
    if(file.exists(file)) {
      load_list[["crop2globiom"]] <- suppressMessages(readxl::read_excel(file, sheet = "crop2globiom"))
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("adm_map" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/adm/{param$res}/adm_map_{param$year}_{param$iso3c}.rds"))
    if(file.exists(file)) {
      load_list[["adm_map"]] <- readRDS(file)
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("adm_map_r" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/adm/{param$res}/adm_map_r_{param$res}_{param$year}_{param$iso3c}.rds"))
    if(file.exists(file)) {
      load_list[["adm_map_r"]] <- readRDS(file)
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("adm_list" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/lists/adm_list_{param$year}_{param$iso3c}.csv"))
    if(file.exists(file)) {
      load_list[["adm_list"]] <- suppressMessages(readr::read_csv(file))
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("results" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/results/{param$res}/results_{param$res}_{param$year}_{param$iso3c}.rds"))
    if(file.exists(file)) {
      load_list[["results"]] <-readRDS(file)
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("ha" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/agricultural_statistics/ha_adm_{param$year}_{param$iso3c}.csv"))
    if(file.exists(file)) {
      load_list[["ha"]] <- suppressMessages(readr::read_csv(file))
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("fs" %in% fl) {
    file <- file.path(param$raw_path,
                      glue::glue("subnational_statistics/farming_system_shares_{param$year}_{param$iso3c}.csv"))
    if(file.exists(file)) {
      load_list[["fs"]] <- suppressMessages(readr::read_csv(file))
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("ci" %in% fl) {
    file <- file.path(param$raw_path,
                      glue::glue("subnational_statistics/cropping_intensity_{param$year}_{param$iso3c}.csv"))
    if(file.exists(file)) {
      load_list[["ci"]] <- suppressMessages(readr::read_csv(file))
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("cl_med" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/cropland/{param$res}/cl_med_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["cl_med"]] <- raster::raster(file)
      names(load_list[["cl_med"]]) <- "cl_med"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }


  if("cl_max" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/cropland/{param$res}/cl_max_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["cl_max"]] <- raster::raster(file)
      names(load_list[["cl_max"]]) <- "cl_max"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("cl_rank" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/cropland/{param$res}/cl_rank_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["cl_rank"]] <- raster::raster(file)
      names(load_list[["cl_rank"]]) <- "cl_rank"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("ia_max" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/irrigated_area/{param$res}/ia_max_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["ia_max"]] <- raster::raster(file)
      names(load_list[["ia_max"]]) <- "ia_max"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("ia_rank" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/irrigated_area/{param$res}/ia_rank_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["ia_rank"]] <- raster::raster(file)
      names(load_list[["ia_rank"]]) <- "ia_rank"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }

  if("simu_r" %in% fl) {
    file <- file.path(param$spam_path,
                      glue::glue("processed_data/maps/simu/{param$res}/simu_r_{param$res}_{param$year}_{param$iso3c}.tif"))
    if(file.exists(file)) {
      load_list[["simu_r"]] <- raster::raster(file)
      names(load_list[["simu_r"]]) <- "simu_r"
    } else {
      stop(paste(basename(file), "does not exist"),
           call. = FALSE)
    }
  }
  if(mess) {
    message(glue::glue("{fPaste(fl)} loaded"))
  }
  if(local == TRUE) {
    invisible(list2env(load_list, envir = parent.frame()))
  } else {
    invisible(list2env(load_list, envir = .GlobalEnv))
  }
}

library(mapspam2globiom)
param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
         solve_level = 1, model = "max_score", crs = "+init=EPSG:32633")

create_spam_folders(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi")

prepare_cropland(param)

adm_code <- "MI02"
df <- cl_df
var <- "cl"

prepare_spatial <- function(adm_code, df, var, adm_map_r, param){
  message(glue::glue("Save {var} for {adm_code}"))
  adm_sel <- paste0("adm", param$solve_level, "_code")
  df <- dplyr::left_join(df, adm_map_r, by = c("gridID")) %>%
    na.omit()
  df <- df[df[[adm_sel]] == adm_code_list,]

  temp_path <- file.path(param$spam_path,
                         glue::glue("processed_data/intermediate_output/{adm_code}"))
  dir.create(temp_path, recursive = T, showWarnings = F)
  saveRDS(df, file.path(temp_path,
                        glue::glue("{var}_{param$year}_{adm_code}_{param$iso3c}.rds")))
}
environment()
environment(f)
.GlobalEnv

f <- function() "top level function"
f2 <- function(){
  x <- environment(f)
  return(x)
}
load_data("ia_max", param)

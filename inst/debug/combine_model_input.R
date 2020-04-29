# Process_bs_py
combine_model_input <- function(adm_cd, param){

  #TODO adm_code is referred as adm_cd => not consistent
  cat("\nPrepare model input for", adm_cd)

  # Load data
  load_intermediate_data(c("pa", "pa_fs", "cl_harm", "ia_harm", "bs", "py", "rps", "score"),
                         adm_cd, param, local = TRUE, mess = FALSE)
  load_data(c("adm_list"), param, local = TRUE, mess = FALSE)


  ############### PREPARATIONS ###############
  # Put statistics in long format and filter out crops where pa = 0
  # These crops create artificial adms, which created conflicts
  pa <- pa %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level) %>%
    dplyr::filter(pa != 0)

  pa_fs <- pa_fs %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level, -system) %>%
    dplyr::filter(pa != 0)

  adm_list_at_lowest_level <- unique(pa$adm_code[pa$adm_level == param$adm_level])
  base <- expand.grid(adm_code = adm_list_at_lowest_level, crop = unique(pa$crop), stringsAsFactors = F) %>%
    dplyr::rename({{rn}} := adm_code) %>%
    dplyr::mutate(adm_level = param$adm_level) %>%
    dplyr::left_join(adm_list)

  # only select adm_code
  adm_code_list <- adm_list %>%
    dplyr::select(adm0_code, adm1_code, adm2_code)


  ############### CREATE ARTIFICIAL ADMS   ###############

  # Check if totals add up
  all.equal(sum(adm_art$pa), sum(pa$pa[pa$adm_code == adm_cd]))

  # Replace very small negative numbers which might occur because of rounding by 0
  adm_art <- adm_art %>%
    mutate(pa = if_else(pa < 0, 0, pa))
  unique(adm_art$adm_code)


  ############### CREATE GAMS PARAMETERS ###############
  # adm_area(k,s): Land use per lowest level adm, including artificial adms (k) and crop (s).
  adm_area <- adm_art %>%
    dplyr::select(adm_code, crop, pa)

  adm_area_gdx <- para_gdx(adm_area, c("adm_code", "crop"), "adm_area", "Crop area per adm")


  # lc(i): Crop cover for each gridcell (i)
  cl_m <- cl %>%
    dplyr::select(gridID, cl)

  cl_gdx <- para_gdx(cl_m, c("gridID"), "cl", "Cropland per grid cell")


  # crop_area(j): Total area per crop system (j)
  crop_area <- pa_fs %>%
    filter(adm_code == adm_code_sel) %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    ungroup() %>%
    dplyr::select(crop_system, pa)

  crop_area_gdx <- para_gdx(crop_area, c("crop_system"), "crop_area", "Total area per crop")


  # ir_area(i): Irrigated area per grid cell (i)
  ir_area <- cl_ir %>%
    filter(gridID %in% unique(cl$gridID)) %>%
    dplyr::select(gridID, cl_ir)

  ir_area_gdx <- para_gdx(ir_area, c("gridID"), "ir_area", "Irrigated area per grid cell")


  # ir_crop(j): Total irrigated area per crop system (j)
  ir_crop <- pa_fs %>%
    filter(adm_code == adm_code_sel) %>%
    filter(system == "I") %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    ungroup() %>%
    dplyr::select(crop_system, pa)

  ir_crop_gdx <- para_gdx(ir_crop, c("crop_system"), "ir_crop", "Total irrigated area per crop")


  # score(i,j): Score per grid cell and crop_system
  score <- score_raw %>%
    dplyr::select(gridID, crop_system, score)

  score_gdx <- para_gdx(score, c("gridID", "crop_system"), "score", "score per grid cell and crop_system")

  # # prior(i,j): prior per grid cell and crop_system
  # prior <- prior_raw %>%
  #   dplyr::select(gridID, crop_system, prior_scaled)
  #
  # prior_gdx <- para_gdx(prior, c("gridID", "crop_system"), "prior", "scaled prior per grid cell and crop_system")


  # rur_pop_s(i,j): Rural population share per grid cell
  rur_pop_share_gdx <- para_gdx(rur_pop_share, c("gridID", "crop_system"), "rur_pop_share", "Rural population shares")


  ### CREATE GAMS SETS
  # Grid cells (i)
  grid_s <- cl %>%
    dplyr::select(gridID) %>%
    unique()

  grid_s_gdx <- set_gdx(grid_s, c("gridID"), "i", "Grid cells")


  # Crops system combinations (j)
  crop_system_s <- pa_fs %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    filter(adm_code == adm_code_sel) %>%
    ungroup() %>%
    dplyr::select(crop_system) %>%
    unique()

  crop_system_s_gdx <- set_gdx(crop_system_s, c("crop_system"), "j", "Crop systems")


  # Crop (s)
  crop_s <- adm_art %>%
    dplyr::select(crop) %>%
    unique()

  crop_s_gdx <- set_gdx(crop_s, c("crop"), "s", "Crops")

  # Subsistence system
  s_system_s <- pa_fs %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    filter(adm_code == adm_code_sel, system == "S") %>%
    ungroup() %>%
    dplyr::select(crop_system) %>%
    unique()

  s_system_s_gdx <- set_gdx(s_system_s, c("crop_system"), "j_s", "Subsistence system combinations")


  # Adms with statistics (k)
  adm_s <- adm_area %>%
    dplyr::select(adm_code) %>%
    unique()

  adm_s_gdx <- set_gdx(adm_s, c("adm_code"), "k", "Administrative regions")


  # Crops with corresponding crop system combinations (s,j)
  crop_crop_system_s <- pa_fs %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    filter(adm_code == adm_code_sel) %>%
    ungroup() %>%
    dplyr::select(crop, crop_system) %>%
    unique()

  crop_crop_system_s_gdx <- set_gdx(crop_crop_system_s, c("crop","crop_system"), "n", "Crops with corresponding system combinations")


  # Administrative regions with corresponding grid cells (k,i)
  adm_grid_s <- cl %>%
    left_join(adm_art_map) %>%
    dplyr::select(adm_code = adm_code_art, gridID) %>%
    unique()

  adm_grid_s_gdx <- set_gdx(adm_grid_s, c("adm_code","gridID"), "l", "adm with corresponding grid cells")


  # Administrative regions with corresponding crops
  adm_crop_s <- adm_area %>%
    dplyr::select(adm_code, crop) %>%
    unique()

  adm_crop_s_gdx <- set_gdx(adm_crop_s, c("adm_code", "crop"), "m", "adm with corresponding crops")


  ############### CREATE GAMS SCALARS ###############
  # scalef: number of grid cells to scale optimization so numbers do not get too small
  scalef <- nrow(grid_s)
  scalef_gdx <- scalar_gdx(scalef, "scalef", "Scaling factor")


  ############### SAVE ###############
  temp_path <- file.path(proc_path, glue("harmonized/{adm_code_sel}"))
  dir.create(temp_path, recursive = T, showWarnings = F)

  # GDX
  wgdx(file.path(temp_path, glue("input_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.gdx")),
       cl_gdx,
       adm_area_gdx,
       ir_crop_gdx,
       ir_area_gdx,
       crop_area_gdx,
       s_system_s_gdx,
       score_gdx,
       grid_s_gdx, crop_system_s_gdx,
       adm_s_gdx,
       crop_crop_system_s_gdx,
       adm_grid_s_gdx,
       adm_crop_s_gdx, crop_s_gdx,
       rur_pop_share_gdx,
       scalef_gdx)


  ############### SAVE ###############
  # save
  saveRDS(rps, file.path(param$spam_path,
    glue::glue("processed_data/intermediate_output/{adm_cd}/rps_{param$res}_{param$year}_{adm_cd}_{param$iso3c}.rds")))
  saveRDS(score_df, file.path(param$spam_path,
    glue::glue("processed_data/intermediate_output/{adm_cd}/score_{param$res}_{param$year}_{adm_cd}_{param$iso3c}.rds")))
}


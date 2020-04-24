#'Harmonizes data in line with solve level
#'
#'@importFrom magrittr %>%
#'
#'
#'
#'



split_harm <- function(adm_code, param)

  adm_code <- "MWI"

  ############### STEP 1: PREPARATIONS ###############
  # Load data
  load_intermediate_data(c("adm_map", "adm_map_r", "grid", "cl", "ia", "pa", "pa_fs"), adm_code, param, local = T)

  # Put statistics in long format
  pa <- pa %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level)

  pa_fs <- pa_fs %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level, -system)

  # Caclulate total pa for each adm
  pa_adm_tot <- dplyr::bind_rows(
    pa %>%
      dplyr::filter(adm_level == 2) %>%
      dplyr::group_by(adm_code, adm_name, adm_level) %>%
      dplyr::summarise(pa = sum(pa, na.rm = T)),
    pa %>%
      dplyr::filter(adm_level == 1) %>%
      dplyr::group_by(adm_code, adm_name, adm_level) %>%
      dplyr::summarise(pa = sum(pa, na.rm = T)),
    pa %>%
      dplyr::filter(adm_level == 0) %>%
      dplyr::group_by(adm_code, adm_name, adm_level) %>%
      dplyr::summarise(pa = sum(pa, na.rm = T))) %>%
    dplyr::ungroup()


  ############### STEP 1: SET CL TO MEDIAN CROPLAND ###############
    # Create df of cl map,  set cl to median cropland
  # Remove few cells where gridID is missing, caused by masking grid with country borders using gdal.
  cl_df <- cl %>%
    dplyr::mutate(cl = cl_med)

  # Remove gridID where cl_rank is NA
  cl_df <- cl_df %>%
    dplyr::filter(!is.na(cl_rank))


  ############### STEP 2: HARMONIZE CL   ###############
  cl_df <- harmonize_cl(cl_df, param)


  ############### STEP 3: HARMONIZE IA ###############
  cl_df <- harmonize_ia(cl_df, param)


  ############### STEP 4: PREPARE FINAL CL MAP BY RANKING CELLS PER ADM ###############
  # We add 10 times the maximum grid_size as we need cl to equal cl or be slightly larger, not smaller. This will give a bit of slack.
  # If lu_adm = 0, which happens sometimes at the adm2 level, these adms are excluded from ranking.
  # TO_UPDATE/CHECK As there are many cells with  same rank, we also rank on area size from high to low so larger area is preferred over small

  # max grid size
  slack_cl = 10*max(cl1_df$grid_size)

  # Rank adm0 to match lu
  cl6_adm0 <- cl6_df %>%
    ungroup() %>%
    arrange(cl_rank3, desc(cl6)) %>%
    mutate(adm0_cum = cumsum(cl6)) %>%
    left_join(pa_adm_tot %>%
                filter(adm_level==0, !is.na(pa), pa != 0) %>%
                rename(adm0_code = adm_code)) %>%
    filter(adm0_cum <= pa + slack_cl)
  table(cl6_adm0$cl_rank3) # Max rank should be > 0
  sum(cl6_adm0$cl6)

  # Rank adm1 to match lu
  cl6_adm1 <- cl6_df %>%
    ungroup() %>%
    arrange(adm1_code, cl_rank3, desc(cl6)) %>%
    group_by(adm1_code) %>%
    mutate(adm1_cum = cumsum(cl6)) %>%
    left_join(pa_adm_tot %>%
                filter(adm_level==1, !is.na(pa), pa != 0) %>%
                rename(adm1_code = adm_code)) %>%
    filter(adm1_cum <= pa + slack_cl)
  table(cl6_adm1$cl_rank3) # Max rank should be > 0
  sum(cl6_adm1$cl6)

  # Rank adm2 to match lu
  cl6_adm2 <- cl6_df %>%
    ungroup() %>%
    arrange(adm2_code, cl_rank3, desc(cl6)) %>%
    group_by(adm2_code) %>%
    mutate(adm2_cum = cumsum(cl6)) %>%
    left_join(pa_adm_tot %>%
                filter(adm_level==2, !is.na(pa), pa != 0) %>%
                rename(adm2_code = adm_code)) %>%
    filter(adm2_cum <= pa + slack_cl)
  table(cl6_adm2$cl_rank3) # Max rank should be > 0
  sum(cl6_adm2$cl6)

  # Compare adm totals
  comp_cl6_adm <- left_join(
    cl6_adm0 %>%
      group_by(adm1_name, adm1_code) %>%
      summarize(cl6_adm0 = sum(cl6, na.rm = T)),
    cl6_adm1 %>%
      group_by(adm1_name, adm1_code) %>%
      summarize(cl6_adm1 = sum(cl6, na.rm = T))) %>%
    left_join(
      cl6_adm2 %>%
        group_by(adm1_name, adm1_code) %>%
        summarize(cl6_adm2 = sum(cl6, na.rm = T))
    )

  # Compare adm1 and cl
  adm1_cl <- cl6_df %>%
    group_by(adm1_name, adm1_code) %>%
    summarize(cl6 = sum(cl6, na.rm = T)) %>%
    left_join(pa_adm_tot %>%
                filter(adm_level == 1) %>%
                rename(adm1_code = adm_code)) %>%
    mutate(diff = cl6-pa)

  # Combine cl6_adm0-2
  # Note if adm2 data is complete for all crops and adms, there is no need to rank at adm0 and adm1 level.
  # Similarly, if adm1 is complete there is no need to rank at adm0 level.
  # Ranking at higher adm levels when adm1 data is complete results in different results
  # as a global ranking is different from grouped ranking.

  adm0_tot <- sum(pa_adm_tot$pa[pa_adm_tot$adm_level==0], na.rm = T)
  adm1_tot <- sum(pa_adm_tot$pa[pa_adm_tot$adm_level==1], na.rm = T)
  adm2_tot <- sum(pa_adm_tot$pa[pa_adm_tot$adm_level==2], na.rm = T)

  if(identical(adm0_tot, adm1_tot) & identical(adm1_tot, adm2_tot)){
    message("Full adm0-2 data, use adm2 ranking")
    gridID_union <- unique(cl6_adm2$gridID)
  } else {
    if(identical(adm0_tot, adm1_tot) & !identical(adm1_tot, adm2_tot)) {
      message("Incomplete adm2 data, combine adm1 and adm2 ranking")
      gridID_union <- unique(c(cl6_adm1$gridID, cl6_adm2$gridID))
    } else {
      if(!identical(adm0_tot, adm1_tot) & !identical(adm0_tot, adm1_tot)){
        message("Incomplete adm1 and adm 2 data, combine adm0, adm1 adm2 ranking")
        gridID_union <- unique(c(cl6_adm0$gridID, cl6_adm1$gridID, cl6_adm2$gridID))
      } else {
        if(!identical(adm0_tot, adm1_tot) & !identical(adm1_tot, adm2_tot)) {
          message("Incomplete but identical adm1 and adm 2 data, combine adm0 and adm2 ranking")
          gridID_union <- unique(c(cl6_adm0$gridID, cl6_adm2$gridID))
        }
      }
    }
  }


  cl7_df <- filter(cl6_df, gridID %in% gridID_union) %>%
    mutate(cl7 = cl6)
  sum(cl7_df$cl7, na.rm = T)


  ############### CONSISTENCY CHECKS ###############
  # available irrigated area > required irrigated area
  sum(cl7_df$cl7[!is.na(cl7_df$cl_ir)], na.rm = T) - pa_I_tot
  sum(cl7_df$cl7[!is.na(cl7_df$cl_ir)], na.rm = T)

  # Check if cl <= max(grid_size)
  max(cl7_df$cl7, na.rm = T) <= max(cl7_df$grid_size, na.rm = T)
  max(cl7_df$cl7, na.rm = T)
  max(cl7_df$grid_size, na.rm = T)

  # Compare cl and lu at adm1
  cl_adm1 <- cl7_df %>%
    group_by(adm1_name, adm1_code) %>%
    summarize(cl = sum(cl7, na.rm = T))

  pa_adm1 <- pa %>%
    filter(adm_level == 1) %>%
    rename(adm1_code = adm_code, adm1_name = adm_name) %>%
    group_by(adm1_code, adm1_name) %>%
    summarize(pa = plus(pa, na.rm = T))

  check_cl_pa_adm1 <- left_join(cl_adm1, pa_adm1) %>%
    mutate(diff = cl-pa)

  # Compare cl and lu at adm2
  cl_adm2 <- cl7_df %>%
    group_by(adm1_name, adm2_name, adm1_code, adm2_code) %>%
    summarize(cl = sum(cl7, na.rm = T))

  pa_adm2 <- pa %>%
    filter(adm_level == 2) %>%
    rename(adm2_name = adm_name, adm2_code = adm_code) %>%
    group_by(adm2_name, adm2_code) %>%
    summarize(pa = plus(pa, na.rm = T))

  check_cl_pa_adm2 <- left_join(cl_adm2, pa_adm2) %>%
    mutate(diff = cl-pa)


  ############### CREATE HARMONIZED CL DATABASE ###############
  # For irrigation
  cl_ir_harm <- cl7_df %>%
    dplyr::select(gridID, cl_ir) %>%
    na.omit

  # For landcover
  cl_harm <- cl7_df %>%
    mutate(cl = cl7)

  # cl versus pa => SUFFICIENT
  sum(cl_harm$cl, na.rm = T) > sum(pa$pa[pa$adm_code == adm_code_sel], na.rm = T)
  sum(cl_harm$cl, na.rm = T)
  sum(pa$pa[pa$adm_code == adm_code_sel], na.rm = T)

  # Final, rename adm_code_sel to adm_code
  adm_level_sel <- glue("adm{adm_sel}_code")
  cl_harm <- cl_harm[c("gridID", adm_level_sel, "cl")]
  names(cl_harm)[names(cl_harm) == adm_level_sel] <- "adm_code"
  cl_harm$adm_level <- adm_sel


  ############### PLOT ###############
  # cl map
  grid_df <- as.data.frame(rasterToPoints(grid))
  cl_harm_r <- left_join(cl_harm, grid_df) %>%
    transmute(x, y, cl)
  cl_harm_r <- rasterFromXYZ(cl_harm_r)
  crs(cl_harm_r) <- crs(adm)

  # mapview(cl_harm_r, map.types = "OpenStreetMap") +
  #   mapview(adm, alpha.regions = 0)

  # ir map
  cl_ir_harm_r <- left_join(grid_df, cl_ir_harm) %>%
    filter(!is.na(cl_ir)) %>%
    transmute(x, y, cl_ir)

  cl_ir_harm_r <- rasterFromXYZ(cl_ir_harm_r)
  crs(cl_ir_harm_r) <- crs(adm)

  # mapview(cl_ir_harm_r, map.types = "OpenStreetMap") +
  #   mapview(adm, alpha.regions = 0)


  ############### SAVE ###############
  # cl_harm
  temp_path <- file.path(proc_path, glue("harmonized/{adm_code_sel}"))
  dir.create(temp_path, recursive = T, showWarnings = F)
  saveRDS(cl_harm, file.path(temp_path, glue("cl_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.rds")))
  writeRaster(cl_harm_r, file.path(temp_path, glue("cropland_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.tif")), overwrite = T)

  # cl_ir_harm
  saveRDS(cl_ir_harm, file.path(temp_path, glue("cl_ir_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.rds")))
  writeRaster(cl_ir_harm_r, file.path(temp_path, glue("irrigated_area_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.tif")), overwrite = T)
}

walk(adm_code_list, harm_pa_cl_ir)


############### CLEAN UP ###############
rm(temp_path, adm, adm_r, cl_harm, cl_harm_r, cl_ir, cl_ir_harm, cl_ir_harm_r, cl_max_raw,
   cl_rank_raw, cl_raw, cl1_df, cl2_df, cl3_df, cl4_df, cl5_df, cl6_df, cl6_adm0, cl6_adm1,
   cl6_adm2, cl7_df, comp_cl6_adm, grid, grid_size, grid_df, ir_area_raw, pa_adm_tot, pa_adm1,
   pa_adm2, pa_fs_raw, pa_fs, pa, pa_raw)
rm(adm0_check, adm1_check, adm0_pa_gr_cl, adm1_cl, adm2_check,adm2_pa_gr_cl, check_cl_pa_adm1,
   check_cl_pa_adm2, cl_adm1, cl_adm2)
rm(adm_code_sel, adm0_tot, adm1_tot, adm2_tot, gridID_union, pa_I_tot, slack_cl, slack_ir)

#'Harmonizes data in line with solve level
#'
#'@import magrittr
#'
#'
#'
#'

adm_code <- "MI03"

split_harm <- function(adm_code, param)

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
      dplyr::summarise(pa = sum(pa, na.rm = T)))


  ############### STEP 1: SET CL TO MEDIAN CROPLAND ###############
    # Create df of cl map,  set cl to median cropland
  # Remove few cells where gridID is missing, caused by masking grid with country borders using gdal.
  cl1_df <- cl %>%
    dplyr::mutate(cl1 = cl_med)


  ############### STEP2: REPLACE CL WITH DT AND UPDATE RANK ###############
  # Adding additional information from OpenStreetMap and/or machine learning products (See Van Dijk et al. 2020)
  # is at the moment not supported in this version of spam.
  cl2_df <- cl1_df %>%
    dplyr::mutate(cl2 = cl1,
           dt = "N",
           cl_rank2 = cl_rank)

  # Remove gridID where cl_rank is NA
  cl2_df <- cl2_df %>%
    dplyr::filter(!is.na(cl_rank2))


  ############### STEP 3A: COMPARE CL AND PA FOR ADM2 ###############
  # adm2 where cl < pa
  adm2_check <- cl2_df %>%
    dplyr::group_by(adm2_name, adm2_code) %>%
    dplyr::summarize(cl2 = sum(cl2, na.rm = T),
              cl_max = sum(cl_max, na.rm = T)) %>%
    dplyr::left_join(pa_adm_tot %>%
                dplyr::filter(adm_level == 2) %>%
                  dplyr::rename(adm2_name = adm_name)) %>%
    dplyr::mutate(diff_cl = cl2-pa,
           diff_cl_max = cl_max-pa) %>%
    dplyr::filter(diff_cl < 0)

  # replace cl_area with cl_area max apart for dt if needed
  if(NROW(adm2_check) == 0) {
    message("No adjustments needed for cl")
    cl3_df <- cl2_df %>%
      mutate(cl3 = cl2)
  } else {
    message("cl need to be set to cl_max for following adm2s")
    print(adm2_check)
    cl3_df <- cl2_df %>%
      mutate(cl3 = ifelse(adm2_name %in% adm2_check$adm2_name & dt == "N", cl_max, cl2))

    # check again
    adm2_pa_gr_cl <- cl3_df %>%
      group_by(adm2_name, adm2_code) %>%
      summarize(cl3 = sum(cl3, na.rm = T),
                grid_size = sum(grid_size, na.rm = T)) %>%
      left_join(pa_adm_tot %>%
                  filter(adm_level == 2) %>%
                  rename(adm2_name = adm_name)) %>%
      mutate(diff = cl3-pa,
             exp_factor = pa/cl3) %>%
      filter(diff < 0)

    if(NROW(adm2_pa_gr_cl) == 0) {
      message("No further adjustments needed for adm2s")
    } else {
      message("Further adjustments needed for the following adm2s after setting to cl_max")
      print(adm2_pa_gr_cl)
    }
  }


  ############### STEP 3b: INCREASE CL FURTHER FOR PROBLEMATIC ADM2 IF NEEDED ###############
  #UPDATE ADD IF STATEMENT FOR THIS SECTION => if(!is.null(adm2_lu_gr_cl))

  # # Estimate expansion factor
  # exp_factor <- adm2_lu_gr_cl %>%
  #   dplyr::select(fips2, adm2, exp_factor)
  #
  # # Expand cl with exp_factor x gamma to grid size excluding lu_det
  # # cl cannot be larger than grid size
  #
  # # CHECK: add protected areas cannot expand
  # # CHECK: should be itterative process for each problematic adm where cl is slowly increased towards grid size till lu is reached
  # gamma <- 5
  # adm_expand <- left_join(exp_factor, cl3_df) %>%
  #   mutate(cl3 = ifelse(lu_det == "N", pmin(grid_size, cl3*exp_factor*gamma), cl3)) %>%
  #   dplyr::select(-exp_factor)
  #
  # # Check whether the expansion is sufficient to cover lu
  # adm_expand %>%
  #   group_by(adm2, fips2) %>%
  #   summarize(cl3 = plus(cl3),
  #             grid_size = sum(grid_size)) %>%
  #   left_join(lu_adm_tot %>%
  #               filter(adm_level == 2) %>%
  #               rename(adm2 = adm)) %>%
  #   mutate(diff = cl3-lu,
  #          share = lu/cl3)
  #
  # # Update cl3 of problematic adm MI04007
  # cl3_df <- bind_rows(
  #   cl3_df %>%
  #     filter(fips2 != "MI04007"),
  #   adm_expand)
  #

  ############### STEP 4: COMPARE CL AND PA FOR ADM1 ###############
  # NOTE: if sum(adm1) = sum(adm2) this step is redundant
  # adm1 where cl < lu
  adm1_check <- cl3_df %>%
    group_by(adm1_code, adm1_name) %>%
    summarize(cl3 = sum(cl3, na.rm = T)) %>%
    left_join(pa_adm_tot %>%
                filter(adm_level == 1) %>%
                rename(adm1_name = adm_name)) %>%
    mutate(diff = cl3-pa) %>%
    filter(diff < 0)

  # replace cl_area with cl_area max apart for lu_det if needed
  if(NROW(adm1_check) == 0) {
    message("No adjustments needed for cl")
    cl4_df <- cl3_df %>%
      mutate(cl4 = cl3)
  } else {
    message("cl need to be set to cl_max for following adm1s")
    print(adm1_check)
    cl4_df <- cl3_df %>%
      mutate(cl4 = ifelse(adm1_name %in% adm1_check$adm1_name & dt == "N", cl_max, cl3))

    # check again
    adm1_pa_gr_cl <- cl4_df %>%
      group_by(adm1, fips1) %>%
      summarize(cl4 = sum(cl4, na.rm = T),
                grid_size = sum(grid_size, na.rm = T)) %>%
      left_join(pa_adm_tot %>%
                  filter(adm_level == 1) %>%
                  rename(adm1_name = adm_name)) %>%
      mutate(diff = cl4-lu,
             exp_factor = lu/cl4) %>%
      filter(diff < 0)

    if(NROW(adm1_pa_gr_cl) == 0) {
      message("No further adjustments needed for adm1s")
    } else {
      message("Further adjustments needed for the following adm2s after setting to cl_max")
      print(adm1_pa_gr_cl)
    }
  }


  ############### STEP 5: COMPARE CL AND PA FOR ADM0 ###############
  # NOTE: if sum(adm0) = sum(adm1) this step is redundant
  # adm0 where cl < lu
  adm0_check <- cl4_df %>%
    group_by(adm0_name, adm0_code) %>%
    summarize(cl4 = sum(cl4, na.rm = T)) %>%
    left_join(pa_adm_tot %>%
                filter(adm_level == 0) %>%
                rename(adm0_name = adm_name)) %>%
    mutate(diff = cl4-pa) %>%
    filter(diff < 0)

  # replace cl_area with cl_area max if needed
  cl5_df <- cl4_df %>%
    mutate(cl5 = ifelse(adm0_name %in% adm0_check$adm0_name & dt == "N", cl_max, cl4))

  # check again
  adm0_pa_gr_cl <- cl5_df %>%
    group_by(adm1_name, adm1_code) %>%
    summarize(cl5 = sum(cl5, na.rm = T)) %>%
    left_join(pa_adm_tot %>%
                filter(adm_level == 1) %>%
                rename(adm1_name = adm_name)) %>%
    mutate(diff = cl5-pa) %>%
    filter(diff < 0)


  ############### STEP 6: ADD IRRIGATION INFORMATION ###############
  # Slack is x times max grid size
  slack_ir = 10*max(cl1_df$grid_size)

  # Rank irrigated grid cells till sum of cl under irrigation is slightly larger than irrigated pa
  pa_I_tot <- sum(pa_fs$pa[pa_fs$system == "I"], na.rm = T)
  cl_ir <- cl5_df %>%
    dplyr::select(gridID, grid_size, cl5, cl_max) %>%
    left_join(ir_area_raw) %>%
    ungroup() %>%
    filter(!is.na(ir_rank)) %>%
    arrange(ir_rank, desc(cl5)) %>%
    mutate(ir_tot = pa_I_tot,
           cl_ir = cl5,
           cl_cum = cumsum(cl_ir)) %>%
    filter(cl_cum <= ir_tot + slack_ir)

  if(max(cl_ir$cl_cum) > pa_I_tot){
    message("Total irrigated area is sufficient.")
  } else {
    message("Not enough irrigated area, set cropland to max(cropland, irrigated area)")
  }

  # If total irrigated area is still smaller than irrigated pa, we set cl max(max(gia, gmia), cl).
  if(max(cl_ir$cl_cum) < pa_I_tot){
    cl_ir <- cl5_df %>%
      dplyr::select(gridID, grid_size, cl5, cl_max) %>%
      left_join(ir_area_raw) %>%
      ungroup() %>%
      filter(!is.na(ir_rank)) %>%
      arrange(ir_rank, desc(cl5)) %>%
      mutate(ir_tot = pa_I_tot,
             cl_ir = pmax(cl5, ir_max, na.rm = T),
             cl_cum = cumsum(cl_ir)) %>%
      filter(cl_cum <= ir_tot + slack_ir)

    if(max(cl_ir$cl_cum) > pa_I_tot){
      message("Total irrigated area is sufficient.")
    } else {
      message("Still not enough irrigated area, set cropland to max(cropland_max, irrigated area)")
    }
  }

  if(max(cl_ir$cl_cum) < pa_I_tot){
    cl_ir <- cl5_df %>%
      dplyr::select(gridID, grid_size, cl5, cl_max) %>%
      left_join(ir_area_raw) %>%
      ungroup() %>%
      filter(!is.na(ir_rank)) %>%
      arrange(ir_rank, desc(cl5)) %>%
      mutate(ir_tot = pa_I_tot,
             cl_ir = pmax(cl_max, ir_max, na.rm = T),
             cl_cum = cumsum(cl_ir)) %>%
      filter(cl_cum <= ir_tot + slack_ir)

    if(max(cl_ir$cl_cum) > pa_I_tot){
      message("Total irrigated area is sufficient.")
    } else {
      message(glue("There is a shortage of {ir_short} ha irrigated area, which will result in model slack.
               Consider revising the irrigated area map"))}
  }

  # Add ir_area and update cl and rank
  cl6_df <- cl5_df %>%
    left_join(cl_ir %>%
                dplyr::select(gridID, cl_ir)) %>%
    mutate(cl6 = if_else(!is.na(cl_ir), cl_ir, cl5),
           cl_rank3 = ifelse(!is.na(cl_ir), 0,  cl_rank2))


  ############### STEP 7: PREPARE FINAL CL MAP BY RANKING CELLS PER ADM ###############
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

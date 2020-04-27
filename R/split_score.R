# Process_bs_py
split_score <- function(var, adm_code, param){

  cat("\nPrepare score for", adm_code)

  # Load data
  load_intermediate_data(c("pa", "pa_fs", "cl_harm", "ia_harm", "bs", "py"), adm_code, param, local = TRUE, mess = FALSE)
  load_data(c("adm_map", "adm_map_r", "grid", "population", "accessibility", "urban_mask", "crop_price"), param, local = TRUE, mess = FALSE)

  # Population map
  pop_raw <- raster(file.path(proc_path, glue("maps/population/population_{grid_sel}_{year_sel}_{iso3c_sel}.tif")))
  names(pop_raw) <- "pop"

  # Urban mask
  urban_mask <- readRDS(file.path(proc_path, glue("maps/urban_extent/urban_extent_{year_sel}_{iso3c_sel}.rds")))

  # Travel time map
  acc_raw <- raster(file.path(proc_path, glue("maps/accessibility/accessibility_{grid_sel}_{year_sel}_{iso3c_sel}.tif")))
  names(acc_raw) <- "acc"

  # GAEZ suitability
  suit <- readRDS(file.path(proc_path, glue("harmonized/suit_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

  # GAEZ potential yield
  py <- readRDS(file.path(proc_path, glue("harmonized/py_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

  # Continent level crop price in USD/ton
  price <- read_csv(file.path(proc_path, glue("agricultural_statistics/faostat_crop_prices_{year_sel}_{iso3c_sel}.csv")))

  # ifpri2crop
  spam_stat2crop <- read_excel(file.path(mappings_path, "mappings_v1.1.xlsx"), sheet = "spam_stat2crop")


  ############### PREPARATIONS ###############
  # Put statistics in long format
  pa <- pa %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level)

  pa_fs <- pa_fs %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level, -system)

  ## Create gridID and system combinations
  crop_system <- pa_fs %>%
    dplyr::filter(adm_code == adm_code, pa != 0) %>%
    dplyr::mutate(crop_system = paste(crop, system , sep = "_"))

  priors_base <- expand.grid(gridID = unique(cl_harm$gridID),
                             crop_system = unique(crop_system$crop_system), stringsAsFactors = F) %>%
    tidyr::separate(crop_system, into = c("crop", "system"), sep = "_", remove = F)

  # create gridID list
  grid_df <- as.data.frame(raster::rasterToPoints(grid))

  ## Rural population
  # Note that we normalize over adms to distribute the crops more evenly over adms.
  # If we would normalize over the whole country, crops for which we do not have adm information,
  # might be pushed to a very limited area.
  pop_rural <- mask(pop_raw, urban_mask, inverse = T) # Remove urban areas
  pop_rural <- as.data.frame(rasterToPoints(stack(grid, pop_rural))) %>%
    dplyr::select(gridID, pop) %>%
    mutate(pop = ifelse(is.na(pop), 0, pop)) %>% # We assume zero population in case data is missing
    left_join(adm_r) %>%
    rename(adm_code = glue("adm{adm_sel}_code")) %>%
    group_by(adm_code) %>%
    mutate(
      pop_norm = 100*(pop-min(pop, na.rm = T))/(max(pop, na.rm = T)-min(pop, na.rm = T))) %>%
    ungroup() %>%
    mutate(pop_norm = ifelse(is.nan(pop_norm), 0, pop_norm)) %>%
    dplyr::select(gridID, pop_norm) %>%
    filter(gridID %in% unique(cl_raw$gridID))
  summary(pop_rural)
  hist(pop_rural$pop_norm, breaks = 100)

  pop_rural_r <- rasterFromXYZ(
    left_join(grid_df, pop_rural) %>%
      dplyr::select(x, y, pop_norm), crs = crs(grid))
  pop_rural_r[pop_rural_r == 0] <- NA
  plot(pop_rural_r)

  ## Accessibility
  # NOTE that we normalize so that max = 0 and min = 1 as higher tt gives lower suitability
  # NOTE that we normalize over the whole country as some cash crops are allocated at national level.
  # We expected these crops to be located a most accessible areas from a national (not adm) perspective
  # Hence we do not normalize using adm_sel as a basis.
  acc <- as.data.frame(rasterToPoints(stack(grid, acc_raw))) %>%
    dplyr::select(gridID, acc) %>%
    left_join(adm_r) %>%
    rename(adm_code = glue("adm{adm_sel}_code")) %>%
    mutate(
      acc_norm = 100*(max(acc, na.rm = T)-acc)/(max(acc, na.rm = T)-min(acc, na.rm = T))) %>%
    mutate(acc_norm = ifelse(is.nan(acc_norm), 0, acc_norm)) %>%
    dplyr::select(gridID, acc_norm)  %>%
    filter(gridID %in% unique(cl_raw$gridID))
  summary(acc)
  hist(acc$acc_norm)

  acc_r <- rasterFromXYZ(
    left_join(grid_df, acc) %>%
      dplyr::select(x, y, acc_norm), crs = crs(grid))
  acc_r[acc_r == 0] <- NA
  plot(acc_r)


  ############### CREATE SCORE ###############
  # We calculate potential revenue by multiplying potential yield with national crop prices

  # ADD Conversion to DM!
  rev <- py %>%
    left_join(price) %>%
    mutate(rev = py*price)
  summary(rev)

  pa_fs <- pa_fs %>%
    filter(adm_code == adm_code_sel) %>%
    dplyr::select(crop, system, crop_area = pa)


  ############### SCORE FOR EACH SYSTEM ###############
  ## SUBSISTENCE
  # We use the rural population share as prior but exclude areas where suitability is zero
  # We also remove adm where crops are not allocated by definition because stat indicates zero ha.

  # crop_s
  crop_s <- unique(pa_fs$crop[pa_fs$system == "S"])

  # select adm without crop_l
  adm_code_crop_s <- bind_rows(
    pa %>%
      filter(adm_level == 1, crop %in% crop_s, pa == 0) %>%
      dplyr::select(crop, adm_code, adm_name, adm_level) %>%
      distinct(),
    pa %>%
      filter(adm_level == 2, crop %in% crop_s, pa == 0) %>%
      dplyr::select(crop, adm_code, adm_name, adm_level) %>%
      distinct()) %>%
    mutate(adm_code_crop = paste(adm_code, crop, sep = "_"))

  rur_pop_share <-priors_base %>%
    filter(system == "S") %>%
    left_join(adm_r) %>%
    mutate(adm1_code_crop = paste(adm1_code, crop, sep = "_"),
           adm2_code_crop = paste(adm2_code, crop, sep = "_")) %>%
    filter((!adm1_code_crop %in% adm_code_crop_s$adm_code_crop) &
             (!adm2_code_crop %in% adm_code_crop_s$adm_code_crop)) %>%
    left_join(pop_rural) %>%
    left_join(suit) %>%
    group_by(crop) %>%
    mutate(
      pop_norm = if_else(suit == 0, 0, pop_norm),
      rur_pop_share = pop_norm/sum(pop_norm, na.rm = T),
      crop_system = paste(crop, system, sep = "_")) %>%
    ungroup() %>%
    dplyr::select(gridID, crop_system, rur_pop_share)
  summary(rur_pop_share)


  ### LOW INPUT
  # We use suitability for only for L
  # We first remove adm where crops are not allocated by definition because stat indicates zero ha
  # Then we normalize over all crops so that an overal ranking is created.
  # This means that crops with higher suitability will get a higher score than crops with a lower suitability.
  # The argument is that if there would be competition between crops, the crop with the highest suitability
  # Will be allocated first

  # crop_l
  crop_l <- unique(pa_fs$crop[pa_fs$system == "L"])

  # select adm without crop_l
  adm_code_crop_l <- bind_rows(
    pa %>%
      filter(adm_level == 1, crop %in% crop_l, pa == 0) %>%
      dplyr::select(crop, adm_code, adm_name, adm_level) %>%
      distinct(),
    pa %>%
      filter(adm_level == 2, crop %in% crop_l, pa == 0) %>%
      dplyr::select(crop, adm_code, adm_name, adm_level) %>%
      distinct()) %>%
    mutate(adm_code_crop = paste(adm_code, crop, sep = "_"))

  # Score table.  We use suitability only for L
  score_l <- priors_base %>%
    filter(system == "L") %>%
    left_join(adm_r) %>%
    mutate(adm1_code_crop = paste(adm1_code, crop, sep = "_"),
           adm2_code_crop = paste(adm2_code, crop, sep = "_")) %>%
    filter((!adm1_code_crop %in% adm_code_crop_l$adm_code_crop) &
             (!adm2_code_crop %in% adm_code_crop_l$adm_code_crop)) %>%
    left_join(suit) %>%
    ungroup() %>%
    mutate(score = 100*(suit-min(suit, na.rm = T))/(max(suit, na.rm = T)-min(suit, na.rm = T))) %>%
    dplyr::select(gridID, crop_system, score)
  summary(score_l)


  ## HIGH INPUT
  # We use revenue and accessibility
  # We first remove adm where crops are not allocated by definition because stat indicates zero ha
  # Then we normalize rev and acessibility over all crops so that an overal ranking is created.
  # Next we use equal weight geometric average as the final ranking.
  # This means that crops with higher revenue and accessibility will get a higher score than crops with a lower rankings.
  # The argument is that if there would be competition between crops, the crop with the highest score
  # Will be allocated first
  # We rerank the combined rev and accessibility score again to it has the same scale as l and i priors.

  # crop_h
  crop_h <- unique(pa_fs$crop[pa_fs$system == "H"])

  # select adm without crop_s
  adm_code_crop_h <- bind_rows(
    pa %>%
      filter(adm_level == 1, crop %in% crop_h, pa == 0) %>%
      dplyr::select(crop, adm_code, adm_name, adm_level) %>%
      distinct(),
    pa %>%
      filter(adm_level == 2, crop %in% crop_h, pa == 0) %>%
      dplyr::select(crop, adm_code, adm_name, adm_level) %>%
      distinct()) %>%
    mutate(adm_code_crop = paste(adm_code, crop, sep = "_"))

  # Score table.  We use geometric average of rev and accessibility
  score_h <- priors_base %>%
    filter(system == "H") %>%
    left_join(adm_r) %>%
    mutate(adm1_code_crop = paste(adm1_code, crop, sep = "_"),
           adm2_code_crop = paste(adm2_code, crop, sep = "_")) %>%
    filter((!adm1_code_crop %in% adm_code_crop_h$adm_code_crop) &
             (!adm2_code_crop %in% adm_code_crop_h$adm_code_crop)) %>%
    left_join(rev) %>%
    left_join(acc) %>%
    ungroup() %>%
    mutate(rev_norm = 100*(rev-min(rev, na.rm = T))/(max(rev, na.rm = T)-min(rev, na.rm = T)),
           score = (rev_norm*acc_norm)^0.5,
           score = 100*(score-min(score, na.rm = T))/(max(score, na.rm = T)-min(score, na.rm = T))) %>%
    dplyr::select(gridID, crop_system, score)
  summary(score_h)


  ## IRRIGATION
  # We use the same score as for H
  # We select only ir gridID
  # crop_i
  crop_i <- unique(pa_fs$crop[pa_fs$system == "I"])

  # Score table.  We use geometric average of suitability and accessibility
  score_i <- priors_base %>%
    filter(system == "I") %>%
    left_join(cl_ir_raw) %>%
    filter(!is.na(cl_ir)) %>%
    left_join(rev) %>%
    left_join(acc) %>%
    ungroup() %>%
    mutate(rev_norm = 100*(rev-min(rev, na.rm = T))/(max(rev, na.rm = T)-min(rev, na.rm = T)),
           score = (rev_norm*acc_norm)^0.5,
           score = 100*(score-min(score, na.rm = T))/(max(score, na.rm = T)-min(score, na.rm = T))) %>%
    dplyr::select(gridID, crop_system, score)
  summary(score_i)


  ############### COMBINE ###############
  # score
  score_df <- bind_rows(score_l, score_h, score_i) %>%
    left_join(priors_base,.) %>%
    mutate(score = replace_na(score, 0)) %>%
    separate(crop_system, into = c("crop", "system"), sep = "_", remove = F)
  summary(score_df)


  ############### SAVE ###############
  # save
  saveRDS(rur_pop_share, file.path(proc_path, glue("harmonized/{adm_code_sel}/rur_pop_share_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.rds")))
  saveRDS(score_df, file.path(proc_path, glue("harmonized/{adm_code_sel}/score_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.rds")))
}


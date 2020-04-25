df <- cl_df


test <- data.frame(adm = c("adm0", "adm1", "adm2"),
                   adm2 = c("adm0", "adm1", "adm1"),
                   adm_tot = c(111, 110, 110),
                   adm_level = c(0, 1, 2), stringsAsFactors = F
)

test %>%
  dplyr::mutate(rank  = min_rank(adm_tot)) %>%
  dplyr::group_by(rank) %>%
  dplyr::arrange(desc(adm), .by_group = TRUE) %>%
  dplyr::slice(1)


select_cl <- function(df, param) {
  if (param$solve_level == 0) {
  grid_sel <- purrr::map_df(c(0:param$adm_level), rank_cl, df)

  adm0_tot <- sum(pa_adm_tot$pa[pa_adm_tot$adm_level==0], na.rm = T)
  adm1_tot <- sum(pa_adm_tot$pa[pa_adm_tot$adm_level==1], na.rm = T)
  adm2_tot <- sum(pa_adm_tot$pa[pa_adm_tot$adm_level==2], na.rm = T)

  if(identical(adm0_tot, adm1_tot) & identical(adm1_tot, adm2_tot)){
    message("Full adm0-2 data, use adm2 ranking")
    gridID_union <- unique(grid_sel$gridID[grid_sel$adm_level == 2])
  } else {
    if(identical(adm0_tot, adm1_tot) & !identical(adm1_tot, adm2_tot)) {
      message("Incomplete adm2 data, combine adm1 and adm2 ranking")
      gridID_union <- unique(c(cl6_adm1$gridID, cl6_adm2$gridID))
    } else {
      if(!identical(adm0_tot, adm1_tot) & identical(adm1_tot, adm2_tot)){
        message("Incomplete adm1 data but identical to adm2 data, combine adm0 and adm2 ranking")
        gridID_union <- unique(c(cl6_adm0$gridID, cl6_adm2$gridID))
      } else {
        if(!identical(adm0_tot, adm1_tot) & !identical(adm1_tot, adm2_tot)) {
          message("Incomplete adm1 and adm 2 data, combine adm0, adm1 adm2 ranking")
          gridID_union <- unique(c(cl6_adm0$gridID, cl6_adm1$gridID, cl6_adm2$gridID))
        }
      }
    }
  }


  }
  if (param$solve_level == 1) {
    x <- purrr::map_df(c(1:param$adm_level), rank_cl, df)
    }
  return(df)
}


############### STEP 4: PREPARE FINAL CL MAP BY RANKING CELLS PER ADM ###############
# We add 10 times the maximum grid_size as we need cl to equal cl or be slightly larger, not smaller. This will give a bit of slack.
# If lu_adm = 0, which happens sometimes at the adm2 level, these adms are excluded from ranking.
# TO_UPDATE/CHECK As there are many cells with  same rank, we also rank on area size from high to low so larger area is preferred over small

adm_lvl <- 2

rank_cl <- function(adm_lvl, df, slackp = 0.05, slackn = 5) {

  # Rank cropland cells till sum is at least equal to the statistics.  We add
  # the minimum of 5 grid_sell area or slack percentage to ensure this.
  rn <- paste0("adm", adm_lvl, "_code")

  # Rank adm to match lu
  adm_rank <- df %>%
    dplyr::rename(adm_code = {{rn}}) %>%
    dplyr::group_by(adm_code) %>%
    dplyr::arrange(adm_code, cl_rank, desc(cl), .by_group = TRUE) %>%
    dplyr::mutate(adm_cum = cumsum(cl)) %>%
    dplyr::left_join(pa_adm_tot %>%
                       dplyr::filter(adm_level == adm_lvl, !is.na(pa), pa != 0) %>%
                       dplyr::select(adm_code, adm_level, pa), by = "adm_code") %>%
    dplyr::mutate(slack = min(pa * slackp, min(slackn*max(df$grid_size)))) %>%
    dplyr::filter(adm_cum <= pa + slack) %>%
    dplyr::ungroup() %>%
    dplyr::select(gridID, adm_level, adm_code, pa)
  return(adm_rank)
}


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
    if(!identical(adm0_tot, adm1_tot) & identical(adm1_tot, adm2_tot)){
      message("Incomplete adm1 data but identical to adm2 data, combine adm0 and adm2 ranking")
      gridID_union <- unique(c(cl6_adm0$gridID, cl6_adm2$gridID))
    } else {
      if(!identical(adm0_tot, adm1_tot) & !identical(adm1_tot, adm2_tot)) {
        message("Incomplete adm1 and adm 2 data, combine adm0, adm1 adm2 ranking")
        gridID_union <- unique(c(cl6_adm0$gridID, cl6_adm1$gridID, cl6_adm2$gridID))
      }
    }
  }
}


cl7_df <- filter(cl6_df, gridID %in% gridID_union) %>%
  mutate(cl7 = cl6)
sum(cl7_df$cl7, na.rm = T)

# Function to merge lc and crop information
# lc = lc, crop = crop
merge_lc_crop <- function(lc, crop, lc_rp = c("OthNatLnd", "NotRel", "WetLnd", "Forest", "Grass")) {

  cat("\n############### Merge SPAM crop distribution maps with land cover information ###############")
  merge_lc_crop_simu <- function(simu_id, lc, crop, lc_rp = lc_rp) {

    cat("\n", simu_id)

    # Ensure that all SimUID globiom_lc types are present
    base_fix <- expand.grid(SimUID = unique(lc$SimUID),
                            globiom_lc_code = c("Forest", "WetLnd", "NotRel", "OthNatLnd", "Grass", "simu_area"),
                            stringsAsFactors = F)
    base_flex <- expand.grid(SimUID = unique(lc$SimUID),
                             globiom_lc_code = c("OthAgri", "CrpLnd"), stringsAsFactors = F)

    # split off rest crops
    simu_crop_ag <- crop %>%
      dplyr::mutate(globiom_lc_code = ifelse(globiom_crop == "rest", "OthAgri", "CrpLnd")) %>%
      dplyr::group_by(SimUID, globiom_lc_code) %>%
      dplyr::summarize(area = sum(value, na.rm = T)) %>%
      dplyr::left_join(base_flex, ., by = c("SimUID", "globiom_lc_code")) %>%
      dplyr::mutate(area = ifelse(is.na(area), 0, area))

    lc <- lc %>%
      dplyr::filter(!globiom_lc_code %in% c("CrpLnd")) %>%
      dplyr::left_join(base_fix,., by = c("SimUID", "globiom_lc_code")) %>%
      dplyr::mutate(area = ifelse(is.na(area), 0, area)) %>%
      dplyr::bind_rows(simu_crop_ag) %>%
      dplyr::ungroup() %>%
      dplyr::filter(SimUID == simu_id)

    diff <- lc$area[lc$globiom_lc_code %in% "simu_area"] - sum(lc$area[!lc$globiom_lc_code %in% "simu_area"])
    cat("\nTotal difference is ", diff)

    if(diff >= 0){
      lc$area[lc$globiom_lc_code == "OthNatLnd"] <- lc$area[lc$globiom_lc_code == "OthNatLnd"] + diff
      lc_upd <- lc
      cat("\nNo rebalancing needed, diff added to OthNatLnd")
    } else {
      for (i in lc_rp){
        if(lc$area[lc$globiom_lc_code == i] >= abs(diff)){
          cat("\n", abs(diff), " is subtracted from lc ", i)
          lc$area[lc$globiom_lc_code == i] <- lc$area[lc$globiom_lc_code == i] - abs(diff)
          diff <- 0
          break
        } else {
          if(lc$area[lc$globiom_lc_code == i] == 0){
            cat("\n0 is subtracted from lc", i, "because it has 0 area")
          } else {
            diff <- diff + lc$area[lc$globiom_lc_code == i]
            cat("\n", lc$area[lc$globiom_lc_code == i], "is subtracted from lc", i)
            lc$area[lc$globiom_lc_code == i] <- 0
          }
        }
      }
      lc_upd <- lc
    }
    return(lc_upd)
  }

  simu_list <- unique(lc$SimUID)
  lc_upd <- purrr::map_df(simu_list, merge_lc_crop_simu, lc, crop, lc_rp)
  return(lc_upd)
}



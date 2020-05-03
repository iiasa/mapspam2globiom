# Function to rebalance simu
#'@export

rebalance_lc <- function(simu_id, simu_lc_df, simu_crop_df,
                           lc_rp = c("OthNatLnd", "NotRel", "WetLnd", "Forest", "Grass")) {

  cat("\n", simu_id)

  # Ensure that all SimUID globiom_lc types are present
  base_fix <- expand.grid(SimUID = unique(simu_lc_df$SimUID),
                          globiom_lc_code = c("Forest", "WetLnd", "NotRel", "OthNatLnd", "Grass", "simu_area"),
                          stringsAsFactors = F)
  base_flex <- expand.grid(SimUID = unique(simu_lc_df$SimUID),
                           globiom_lc_code = c("OthAgri", "CrpLnd"), stringsAsFactors = F)

  # split off rest crops
  simu_crop_ag <- simu_crop_df %>%
    dplyr::mutate(globiom_lc_code = ifelse(globiom_crop == "rest", "OthAgri", "CrpLnd")) %>%
    dplyr::group_by(SimUID, globiom_lc_code) %>%
    dplyr::summarize(area = sum(value, na.rm = T)) %>%
    dplyr::left_join(base_flex, .) %>%
    dplyr::mutate(area = ifelse(is.na(area), 0, area))

  df <- simu_lc_df %>%
    dplyr::filter(!globiom_lc_code %in% c("CrpLnd")) %>%
    dplyr::left_join(base_fix,.) %>%
    dplyr::mutate(area = ifelse(is.na(area), 0, area)) %>%
    dplyr::bind_rows(simu_crop_ag) %>%
    dplyr::ungroup() %>%
    dplyr::filter(SimUID == simu_id)

  diff <- df$area[df$globiom_lc_code %in% "simu_area"] - sum(df$area[!df$globiom_lc_code %in% "simu_area"])
  cat("\nTotal difference is ", diff)

  if(diff >= 0){
    df_upd <- df
    df_upd$area[df$globiom_lc_code == "OthNatLnd"] <- df_upd$area[df$globiom_lc_code == "OthNatLnd"] +diff
    cat("\nNo rebalancing needed, diff added to OthNatLnd")
  } else {
    for (i in lc_rp){
      if(df$area[df$globiom_lc_code == i] >= abs(diff)){
        cat("\nFinal ", abs(diff), " is subtracted from lc ", i)
        df$area[df$globiom_lc_code == i] <- df$area[df$globiom_lc_code == i] - abs(diff)
        diff <- 0
        break
      } else {
        if(df$area[df$globiom_lc_code == i] == 0){
          print(paste0("0 is subtracted from lc ", i, " because it has 0 area."))
        } else {
          diff <- diff + df$area[df$globiom_lc_code == i]
          print(paste0(df$area[df$globiom_lc_code == i], " is subtracted from lc ", i, "."))
          df$area[df$globiom_lc_code == i] <- 0
        }
      }
    }
    df_upd <- df
  }
  return(df_upd)
}



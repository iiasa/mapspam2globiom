
# function to extract replacement crops in case of missing information
replace_gaez <- function(crp_sys, db, db_raw){

  crp <- strsplit(crp_sys, split = "_")[[1]][1]
  sys <- strsplit(crp_sys, split = "_")[[1]][2]
  crp_subs <- gaez_subs$crop_subs1[gaez_subs$crop == crp]

  if(!is.na(crp_subs)) {
    crp_sys_subs <- paste(crp_subs, sys, sep = "_")
    crp_subs_df <- as.data.frame(rasterToPoints(stack(grid, subset(db_raw, crp_sys_subs)))) %>%
      dplyr::select(-x, -y) %>%
      setNames(c("gridID", "value")) %>%
      mutate(crop_system = crp_sys) %>%
      filter(!is.na(gridID)) %>%
      mutate(value = ifelse(is.na(value), 0, value),
             value = ifelse(value < 0, 0, value))

    if(!all(crp_subs_df$value == 0)) {
      message(glue("Use {crp_sys_subs} to substitute {crp_sys}."))
      return(crp_subs_df)
    }
  }
  crp_subs <- gaez_subs$crop_subs2[gaez_subs$crop == crp]
  if(is.na(crp_subs)) {
    stop(glue("GAEZ information for {crp_sys} with no substitute crop!. Add crop in gaez_subs!"))
  } else {
    crp_sys_subs <- paste(crp_subs, sys, sep = "_")
    crp_subs_df <- as.data.frame(rasterToPoints(stack(grid, subset(db_raw, crp_sys_subs)))) %>%
      filter(gridID %in% unique(cl_raw$gridID)) %>%
      dplyr::select(-x, -y) %>%
      setNames(c("gridID", "value")) %>%
      mutate(crop_system = crp_sys) %>%
      filter(!is.na(gridID)) %>%
      mutate(value = ifelse(is.na(value), 0, value),
             value = ifelse(value < 0, 0, value))

    if(all(crp_subs_df$value != 0)) {
      message(glue("Use {crp_sys_subs} to substitute {crp_sys}."))
      return(crp_subs_df)
    }
    stop(glue("GAEZ information for {crp_sys} with no substitute crop!. Add crop in gaez_subs!"))
  }
}

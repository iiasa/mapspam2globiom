
#' Function to add irrigation information
harmonize_ia <- function(df, adm_code, param, ia_slackp) {

  # Rank irrigated grid cells till sum of cl under irrigation is at least equal
  # to the area of irrigated crops. We add the maximum of 1 grid_sell area or
  # slack percentage to ensure this.
  load_intermediate_data(c("pa_fs", "ia"), adm_code, param, local = T, mess = F)
  pa_fs <- pa_fs %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level, -system)

  pa_I_tot <- sum(pa_fs$pa[pa_fs$system == "I"], na.rm = T)
  slack <- max(max(df$grid_size), pa_I_tot*ia_slackp)
  pa_I_tot = pa_I_tot + slack

  cl_ia <- df %>%
    dplyr::select(gridID, grid_size, cl, cl_max) %>%
    dplyr::left_join(ia %>%
                       dplyr::select(gridID, ia_max, ia_rank), by = "gridID") %>%
    dplyr::filter(!is.na(ia_rank)) %>%
    dplyr::arrange(ia_rank, desc(cl)) %>%
    dplyr::mutate(ir_tot = pa_I_tot,
           cl_ia = pmin(cl, ia_max, na.rm = T),
           cl_ia2 = pmax(cl, ia_max, na.rm = T),
           cl_ia3 = pmax(cl_max, ia_max, na.rm = T),
           cl_ia_cum = cumsum(cl_ia),
           cl_ia_cum2 = cumsum(cl_ia2),
           cl_ia_cum3 = cumsum(cl_ia3))

  if (max(cl_ia$cl_ia_cum) >= pa_I_tot) {
    cat("\nIrrigated area is sufficient")
    cl_ia <- cl_ia %>%
      dplyr::filter(cl_ia_cum <= pa_I_tot)
  } else {
    if (max(cl_ia$cl_ia_cum2) >= pa_I_tot) {
      cat("\nIrrigated area is sufficient when full cl is assumed to be irrigated")
      cl_ia <- cl_ia %>%
        dplyr::filter(cl_ia_cum2 <= pa_I_tot)
    } else {
      if (max(cl_ia$cl_ia_cum3) >= pa_I_tot) {
        cat("\nIrrigated area is sufficient when full cl_max is assumed to be irrigated")
        cl_ia <- cl_ia %>%
          dplyr::filter(cl_ia_cum3 <= pa_I_tot)
      } else {
        cat("\nThere is not enough irrigated area, which will result in slack.")
      }
    }
  }

  # Update cl and rank
  df <- df %>%
    dplyr::left_join(cl_ia %>%
                       dplyr::select(gridID, cl_ia, ia_max), by = "gridID") %>%
    dplyr::mutate(cl = ifelse(!is.na(cl_ia), cl_ia, cl),
                  cl_rank = ifelse(!is.na(cl_ia), 0,  cl_rank))
  return(df)
}


df <- cl_df
#' Function to add irrigation information
update_ia <- function(df, adm_code) {

  # Slack is x times max grid size
  slack_ir = 10*max(df$grid_size)

  # Rank irrigated grid cells till sum of cl under irrigation is slightly larger than irrigated pa
  pa_I_tot <- sum(pa_fs$pa[pa_fs$system == "I"], na.rm = T)


  cl_ia <- df %>%
    dplyr::select(gridID, grid_size, cl, cl_max) %>%
    dplyr::left_join(ia %>%
                       dplyr::select(gridID, ia_max, ia_rank), by = "gridID") %>%
    dplyr::filter(!is.na(ia_rank)) %>%
    dplyr::arrange(ia_rank, desc(cl)) %>%
    dplyr::mutate(ir_tot = pa_I_tot,
           cl_ir = cl,
           cl_cum = cumsum(cl_ir)) %>%
    dplyr::filter(cl_cum <= ir_tot + slack_ir)

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


}


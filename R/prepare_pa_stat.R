#'Prepares the physical area subnational statistics so they can be used as input
#'for SPAM.
#'
#'To estimate the physical crop area for each farming system harvest area (ha)
#'statistics are combined with information on farmings sytem shares (fs) and
#'cropping intensity (ci). Depending on how the model is solved, the physical
#'area statistics are saved at the administrative unit level 0/country level
#'(model solve level 0) or at the level 1 administrative unit level (model solve
#'level 1).
#'
#'`prepare_ha_stat` combines ha, fs and ci statistics and saves two files in csv
#'format: (1) physical area (pa) and (2) physical area broken down by farming
#'systems (pa_fs). Results are saved in the subfolders that are located in the
#'the `processed_data/intermediate` folder.
#'
#'@param adm_code vector with administrative unit codes. This is either a single value
#'  equal to the country alpha-3 country code if model solve level is set to 0
#'  or a vector with all level 1 administrative unit codes if the model solve
#'  level is set to 1.
#'@param ha tbl or data.frame with harvest area statistics
#'@param fs tbl or data.frame with farming system shares
#'@param ci tbl or data.frame with cropping intensity statistics
#'@param param
#'@inheritParams create_grid
#'
#'@examples
#'
#'@export
prepare_pa_stat <- function(adm_code, ha, fs, ci, param){
    if(!adm_code %in% ha$adm_code[ha$adm_level == param$solve_level]) {
        stop("The adm codes in adm_code are not present in the list of adm codes in
             the physical area statistics.",
             call. = FALSE)
    }
    message(glue("Save pa and pa_fs statistics for {adm_code}"))

    # Select ha for top level and all lower level ADMs
    ha_adm <- bind_rows(
        ha[ha$adm_code == adm_code,],
        ha[ha$adm_code %in% adm_list$adm1_code[adm_list$adm0_code == adm_code],],
        ha[ha$adm_code %in% adm_list$adm2_code[adm_list$adm1_code == adm_code],],
        ha[ha$adm_code %in% adm_list$adm2_code[adm_list$adm0_code == adm_code],]) %>%
        unique()

    # Select fs and ci for top level ADM only. We apply these to lower levels.
    fs_adm <- bind_rows(
        fs[fs$adm_code == adm_code,]) %>%
        dplyr::select(-adm_code, -adm_name, -adm_level) %>%
        unique()

    ci_adm <- bind_rows(
        ci[ci$adm_code == adm_code,]) %>%
        dplyr::select(-adm_code, -adm_name, -adm_level) %>%
        unique()

    # Calculate physical area using cropping intensity information.
    pa_adm <- ha_adm %>%
        left_join(ci_adm, by = "crop")  %>%
        left_join(fs_adm, by = c("crop", "system")) %>%
        mutate(pa = ha*fs/ci) %>%
        group_by(adm_name, adm_code, crop, adm_level) %>%
        summarize(pa = plus(pa, na.rm = T)) %>%
        ungroup()

    # Calculate physical area broken down by farming systems
    pa_fs_adm <- pa_adm %>%
        filter(adm_code == adm_code) %>%
        left_join(fs_adm, by = "crop") %>%
        mutate(pa = pa*fs) %>%
        dplyr::select(-fs) %>%
        ungroup()

    # consistency check
    compare_adm2(pa_adm, pa_fs_adm, param$solve_level)

    # save
    temp_path <- file.path(param$spam_path,
                           glue::glue("processed_data/intermediate_output/{adm_code}"))
    dir.create(temp_path, recursive = T, showWarnings = F)

    pa_adm <- pa_adm %>%
        spread(crop, pa) %>%
        arrange(adm_code, adm_name, adm_level)

    pa_fs_adm <- pa_fs_adm %>%
        spread(crop, pa) %>%
        arrange(adm_code, adm_name, adm_level)

    write_csv(pa_adm, file.path(temp_path,
                                glue::glue("pa_{param$year}_{adm_code}_{param$iso3c}.csv")))
    write_csv(pa_fs_adm, file.path(temp_path,
                                   glue::glue("pa_fs_{param$year}_{adm_code}_{param$iso3c}.csv")))
}


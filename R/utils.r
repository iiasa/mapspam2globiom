# Helper functions that are run inside other functions.
# These functions are for internal use only and are not documented nor exported

# Function to test if gdxrrw is installed.
test_gdxrrw <- function(a, b) {
    if (!requireNamespace("gdxrrw", quietly = TRUE)) {
        stop("Package gdxrrw needed for this function to work. Please install it (see software article for help).",
             call. = FALSE)
    }
}

# Function to create a data.frame with subtotals per crop at set adm level.
sum_adm_total <- function(df, level){
    unit <- names(df)[names(df) %in% c("ha", "pa")]
    names(df)[names(df) %in% c("ha", "pa")] <- "value"
    df <- df %>%
        dplyr::filter(adm_level == level) %>%
        dplyr::group_by(crop, adm_level) %>%
        dplyr::summarize(value = plus(value, na.rm = F)) %>%
        dplyr::arrange(crop)
    return(df)
}

# Function to compare subtotals per crop at different adm levels.
compare_adm <- function(df, level_1, level_2, out = F){
    tot1 <- sum_adm_total(df, level_1) %>%
        na.omit
    tot2 <- sum_adm_total(df, level_2) %>%
        na.omit
    inter <- intersect(tot1$crop, tot2$crop)

    if(!isTRUE(all.equal(tot1$value[tot1$crop %in% inter],
                         tot2$value[tot2$crop %in% inter]))){
        message(
            glue::glue("adm{level_1} and adm{level_2} are not equal!. Did you run rebalance_stat?")
        )
    } else {
        message(glue::glue("adm{level_1} and adm{level_2} are equal"))
    }

    out_df <- dplyr::bind_rows(
        sum_adm_total(df, level_1),
        sum_adm_total(df, level_2)) %>%
        tidyr::spread(adm_level, value) %>%
        setNames(c("crop", "level_1" , "level_2")) %>%
        mutate(difference = round(level_1 - level_2, 6)) %>%
        setNames(c("crop", paste0("adm", level_1), paste0("adm", level_2),"difference"))
    if(out) return(out_df)
}

# Function to compare adm totals for two different data.frames, i.e. pa and pa_fs
compare_adm2 <- function(df1, df2, level, out = F){
    tot1 <- sum_adm_total(df1, level) %>%
        na.omit
    tot2 <- sum_adm_total(df2, level) %>%
        na.omit
    inter <- intersect(tot1$crop, tot2$crop)
    if(!isTRUE(all.equal(tot1$value[tot1$crop %in% inter],
                         tot2$value[tot2$crop %in% inter]))){
        stop(
            glue::glue("df1 and df2 are not equal!",
                       call. = FALSE)
        )
    } else {
        message(glue::glue("df1 and df2 are equal"))
    }

    out_df <- dplyr::bind_rows(
        sum_adm_total(df1, level) %>%
            mutate(source = "df1"),
        sum_adm_total(df2, level) %>%
            mutate(source = "df2")) %>%
        tidyr::spread(source, value) %>%
        mutate(difference = round(df1 - df2, 6)) %>%
        dplyr::select(-adm_level)
    if(out) return(out_df)
}

# Function to paste a vector but replace last one by and
fPaste <- function(vec) sub(",\\s+([^,]+)$", " and \\1", toString(vec))

# Function split and save ir_df and cl_df in line with solve_level
split_spatial <- function(adm_code, df, var, adm_map_r, param){
    message(glue::glue("Save {var} for {adm_code}"))
    adm_sel <- paste0("adm", param$solve_level, "_code")
    df <- dplyr::left_join(df, adm_map_r, by = c("gridID")) %>%
        na.omit()
    df <- df[df[[adm_sel]] == adm_code,]

    temp_path <- file.path(param$spam_path,
                           glue::glue("processed_data/intermediate_output/{adm_code}"))
    dir.create(temp_path, recursive = T, showWarnings = F)
    saveRDS(df, file.path(temp_path,
                          glue::glue("{var}_{param$year}_{adm_code}_{param$iso3c}.rds")))
}

# Function to combine ha, fs and ci and split
# Need to split first and then combine as in split version, adm specific fs and ci are used
split_stat <- function(adm_code, ha, fs, ci, param){
    message(glue("Save pa and pa_fs statistics for {adm_code}"))

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

    pa_adm <- pa_adm %>%
        spread(crop, pa) %>%
        arrange(adm_code, adm_name, adm_level)

    pa_fs_adm <- pa_fs_adm %>%
        spread(crop, pa) %>%
        arrange(adm_code, adm_name, adm_level)

    temp_path <- file.path(param$spam_path,
                           glue::glue("processed_data/intermediate_output/{adm_code}"))
    dir.create(temp_path, recursive = T, showWarnings = F)
    write_csv(pa_adm, file.path(temp_path,
                                glue::glue("pa_{param$year}_{adm_code}_{param$iso3c}.csv")))
    write_csv(pa_fs_adm, file.path(temp_path,
                                   glue::glue("pa_fs_{param$year}_{adm_code}_{param$iso3c}.csv")))
}


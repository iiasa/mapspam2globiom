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


compare_adm2 <- function(df1, df2, level, out = F){
    tot1 <- sum_adm_total(df1, level) %>%
        na.omit
    tot2 <- sum_adm_total(df2, level) %>%
        na.omit
    inter <- intersect(tot1$crop, tot2$crop)
    if(!isTRUE(all.equal(tot1$value[tot1$crop %in% inter],
                         tot2$value[tot2$crop %in% inter]))){
        message(
            glue::glue("df1 and df2 are not equal!")
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

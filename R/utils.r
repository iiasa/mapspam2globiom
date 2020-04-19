# Helper functions that are run inside other functions.
# These functions are for internal use only and are not documented nor exported

# plus returns the sum of all values provided as arguments but ensures NA + NA = NA when na.rm = T.
# This contrasts with sum, which returns 0.
plus <- function(x, na.rm = F){
    if(all(is.na(x))){
        c(x[0],NA)
    } else {
        if(na.rm == T){
            sum(x, na.rm = TRUE)
        } else {
            sum(x, na.rm)
        }
    }
}

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
        dplyr::summarize(value = plus(value, na.rm = F))
    return(df)
}

# Function to compare subtotals per crop at different adm levels.
compare_adm_tot <- function(df, level_1, level_2, out = F){
    check_tot <- dplyr::bind_rows(
        sum_adm_total(df, level_1),
        sum_adm_total(df, level_2)) %>%
        tidyr::spread(adm_level, value) %>%
        setNames(c("crop", "level_1", "level_2"))

    check_tot_out <- check_tot %>%
        mutate(difference = level_1 - level_2)

    check_tot_na <- check_tot %>%
        na.omit

    if(!isTRUE(all.equal(check_tot_na$level_1, check_tot_na$level_2))) {
        stop(
            glue::glue("adm{level_1} and adm{level_2} are not equal"),
            call. = FALSE
        )
    } else {
        message(glue::glue("adm{level_1} and adm{level_2} are equal"))
    }

    if(out) return(check_tot_out)
}

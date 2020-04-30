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
#'`prepare_physical_area` combines ha, fs and ci statistics and saves two files in csv
#'format: (1) physical area (pa) and (2) physical area broken down by farming
#'systems (pa_fs). Results are saved in the subfolders that are located in the
#'the `processed_data/intermediate` folder.
#'
#'@param ha tbl or data.frame with harvest area statistics
#'@param fs tbl or data.frame with farming system shares
#'@param ci tbl or data.frame with cropping intensity statistics
#'@param param
#'@inheritParams create_grid
#'
#'@examples
#'
#'@export
prepare_physical_area <- function(ha, fs, ci, param){
    load_data("adm_list", param, local = TRUE)

    # Set adm_level
    if(param$solve_level == 0) {
        adm_code_list <- unique(adm_list$adm0_code)
    } else {
        adm_code_list <- unique(adm_list$adm1_code)
    }

    # Save
    purrr::walk(adm_code_list, split_statistics, ha, fs, ci, param)
}


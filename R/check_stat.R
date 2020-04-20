#'Checks consistency of subnational statistics
#'
#'Subnational statistics must be of class `subnat`
#'
#'@param df tbl or data.frame
#'@param param
#'@inheritParams create_grid
#'@param logical
#'
#'@return data.frame `df`.
#'
#'@examples
#'
#'@export
check_stat <- function(df, param, out = F){
    if(param$adm_level == 2){
        rep <- list(
            compare_adm_tot(df, 1, 2, out = out),
            compare_adm_tot(df, 0, 2, out = out),
            compare_adm_tot(df, 0, 1, out = out)
            )
    }
    if(param$adm_level == 1){
        compare_adm_tot(df, 0, 1)
    }
    if(out) return(rep)
}


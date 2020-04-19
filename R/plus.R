#' Sum of vector elements with NA+NA=NA
#'
#' \code{plus} Returns the sum of all values provided as arguments but ensures
#' NA + NA = NA.
#'
#' This function is the same as \code{sum} but if \code{na.rm} is \code{false}
#' and all input values are \code{NA}, it will return \code{NA} instead of 0.
#'
#' @param x numeric vector.
#' @param na.rm logical. Should missing values be removed?
#'
#' @return The sum of \code{x}
#'
#' @examples
#' plus(1:10)
#' plus(c(NA, NA))
#' plus(c(NA, NA), na.rm = T)
#' @export
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


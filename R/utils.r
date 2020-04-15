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

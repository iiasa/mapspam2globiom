#'Create spam folder structure
#'
#'`create_spam_folders` creates the folder structure that is needed store raw
#'data, processed data and parameters for SPAM.
#'
#'`create_spam_folders` creates three folders in the `spam_path` set by the
#'user: parameters, processed_data and raw_data as well as a number of lower
#'level folders. This structure is essential to run SPAM. If `spam_path` is
#'missing, the folder will be created in the `path.expand("~")`.
#'
#'@param spam_path character string with the main SPAM folder. Note that R uses
#'  forward slash or double backslash to separate folder names.
#'
#'@examples
#'\dontrun{
#'create_spam_folders(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi")
#'}
#'
#'@export
create_spam_folders <- function(spam_path = NULL) {
    if (is.null(spam_path))
        spam_path <- path.expand("~")

    raw_folders <-
        c(
            "adm",
            "aquastat",
            "faostat",
            "gaez",
            "gia",
            "gmia",
            "grump",
            "sasam",
            "subnational_statistics",
            "travel_time_2000",
            "travel_time_2015",
            "worldpop"
        )

    invisible(lapply(raw_folders, function(x)
        dir.create(
            file.path(spam_path, paste0("raw_data/", x)),
            showWarnings = F,
            recursive = T
        )))

    proc_folders <- c("lists",
                      "intermediate_output",
                      "agricultural_statistics",
                      "maps/adm",
                      "maps/grid",
                      "maps/biophysical_suitability",
                      "maps/potential_yield",
                      "maps/accessibility",
                      "maps/population",
                      "maps/irrigated_area",
                      "maps/cropland",
                      "output")
    invisible(lapply(proc_folders, function(x)
        dir.create(
            file.path(spam_path, paste0("processed_data/", x)),
            showWarnings = F,
            recursive = T
        )))

    dir.create(file.path(spam_path, "parameters"),
               showWarnings = F,
               recursive = T)

    message(paste0("SPAM folder structure created in ", spam_path))
}

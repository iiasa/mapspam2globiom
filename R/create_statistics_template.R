#'Create template for raw subnational crop statistics
#'
#'To support the preparation of the subnational statistics,
#'`create_raw_statistics_template()` can create three types of data templates:
#'- ha for harvest area statistics
#'- fs for farming system share
#'- ci for cropping intensity.

#'The function requires information on how the different administrative unit
#'levels are nested. This file needs to be created first by running
#'`create_adm_list()`.
#'
#'@param type Character vector that refers to the type of template that needs to
#'  be created. See details for allowed input.
#'@param param
#'@inheritParams create_spam_folders
#'
#'@examples
#'\dontrun{
#'create_statistics_template(type = "ha", param)
#'}
#'
#'@export
create_statistics_template <- function(type, adm_list, param) {
  stopifnot(inherits(param, "spam_par"))
  load_data(c("adm_list", "crop"), param)

  if(type == "ha") {
    #
  } else {
    if(type == "fs") {
      #
    } else {
      if(type == "ci") {
        #
      }
    }
  }
}

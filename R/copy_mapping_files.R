# Function to copy mapping files
copy_mapping_files <- function(param) {
  mapping_files <- list.files(path = system.file("mappings", package = "mapspam2globiom"), full.names = TRUE)

  purrr::walk(mapping_files, function(x) {
    if(!file.exists(file.path(param$spam_path, paste0("mappings/", basename(x))))) {
      file.copy(x, file.path(param$spam_path, paste0("mappings/", basename(x))))
    }
  })

}

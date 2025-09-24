

#' Scan Folder
#'
#' @param path the path of the folder to scan
#'
#' @returns a data.frame of the metadata
#' @export
#'
#' @examples
#' scan(path = ".")

scan <- function(path){

  # -- scan folder
  files <- list.files(path, pattern = "*.CR2", full.names = TRUE)

  # -- read metadata & merge
  dplyr::bind_rows(lapply(files, read_cr2, mapping_exif = mapping_exif, mapping_canon = mapping_canon))

}

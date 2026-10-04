

#' Scan Folder
#'
#' @param path the path to scan.
#'
#' @returns a data.frame of the metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' report <- scan(path = ".")
#' }

scan <- function(path){

  # -- folder files
  files <- list.files(path, pattern = "*.CR2", full.names = TRUE, recursive = TRUE)

  # -- read metadata & merge
  dplyr::bind_rows(lapply(files, read_cr2, mapping_exif = mapping_exif, mapping_canon = mapping_canon))

}

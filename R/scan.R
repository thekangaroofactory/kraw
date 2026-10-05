

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
  cat("- number of files to extract:", length(files))

  # -- progress
  pb <- utils::txtProgressBar(min = 0, max = length(files), initial = 0, char = "=",
                       width = 50, style = 3)

  # -- read metadata & merge
  m <- dplyr::bind_rows(lapply(1:length(files), function(n) {

    x <- files[[n]]
    data <- read_cr2(x, mapping_exif = mapping_exif, mapping_canon = mapping_canon)
    utils::setTxtProgressBar(pb, value = n)
    data

    }))

  # -- close progress
  close(pb)

  # -- return
  m

}

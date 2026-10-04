

#' Most Frequent
#'
#' @description
#' Extract most frequent combination.
#'
#' @param data a data.frame (output of [scan()]).
#'
#' @details
#' The function computes the most frequent combination over:
#' camera, orientation, exposure_time, f_number, iso_speed, lens_model, focal_length
#'
#' @returns a data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' most_frequent(scan("."))
#' }

most_frequent <- function(data){

  # select columns
  data <- data |>
    dplyr::select(camera, orientation, exposure_time, f_number, iso_speed, lens_model, focal_length)

  # compute hash
  data$hash <- apply(data,
                  MARGIN = 1,
                  FUN = function(x) digest::digest(paste(x, collapse = ""), algo = "sha256", serialize = FALSE))

  # most frequent hash
  y <- data |>
    dplyr::group_by(hash) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(n == max(n))


  # extract
  head(data[data$hash == y$hash, ], 1L)

}

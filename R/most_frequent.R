

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
#' @importFrom utils head
#' @importFrom ggplot2 .data
#'
#' @examples
#' \dontrun{
#' most_frequent(scan("."))
#' }

most_frequent <- function(data){

  # select columns
  data <- data |>
    dplyr::select(.data$camera, .data$orientation, .data$exposure_time,
                  .data$f_number, .data$iso_speed, .data$lens_model, .data$focal_length)

  # compute hash
  data$hash <- apply(data,
                  MARGIN = 1,
                  FUN = function(x) digest::digest(paste(x, collapse = ""), algo = "sha256", serialize = FALSE))

  # most frequent hash
  y <- data |>
    dplyr::group_by(.data$hash) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(.data$n == max(.data$n))

  # extract
  x <- head(data[data$hash == y$hash, ], 1L)
  x$n <- y$n

  # return
  x

}

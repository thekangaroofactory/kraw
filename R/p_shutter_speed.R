

#' Shutter Speed Plot
#'
#' @param data a data.frame (see details).
#' @param bg a background color.
#' @param theme an optional theme function.
#'
#' @details
#' `data` expects the following columns:
#' - lens_model: character, the name of the lens
#' - exposure_time: character, the aperture value
#' - n: the number of images.
#'
#' @returns a 'ggplot' object.
#' @export
#' @importFrom ggplot2 .data
#'
#' @examples

p_shutter_speed <- function(data, bg = "grey", theme = p_theme()){

  ggplot2::ggplot(data) +
    ggplot2::geom_segment(ggplot2::aes(yend = .data$n,
                                       x = .data$exposure_time,
                                       group = .data$lens_model,
                                       colour = .data$lens_model),
                          y = 0,
                          lineend = "round",
                          linewidth = 4,
                          alpha = .5,
                          show.legend = FALSE) +

    # tittle
    ggplot2::ggtitle("Shutter speed") +

    # apply theme
    theme

}



#' Aperture Plot
#'
#' @param data a data.frame (see details).
#' @param bg a background color.
#' @param theme an optional theme function.
#'
#' @details
#' `data` expects the following columns:
#' - lens_model: character, the name of the lens
#' - f_number: character, the aperture value
#' - n: the number of images.
#'
#' @returns a 'ggplot' object.
#' @export
#' @importFrom ggplot2 .data
#'
#' @examples

p_aperture <- function(data, bg = "grey", theme = p_theme()){

  ggplot2::ggplot(data) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$n,
                                       y = stats::reorder(f_number, .data$n),
                                       group = .data$lens_model,
                                       colour = .data$lens_model),
                          x = 0,
                          lineend = "round",
                          linewidth = 4,
                          alpha = .5,
                          show.legend = FALSE) +

    # tittle
    ggplot2::ggtitle("Aperture") +

    # apply theme
    theme

}

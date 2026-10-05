

#' ISO Plot
#'
#' @param data a data.frame (see details).
#' @param bg a background color.
#' @param theme an optional theme function.
#'
#' @details
#' `data` expects the following columns:
#' - lens_model: character, the name of the lens
#' - iso_speed: numeric, the ISO speed
#' - n: the number of images.
#'
#' @returns a 'ggplot' object.
#' @export
#'
#' @examples
#' \dontrun{
#' p_iso(data.frame(lens_model = "foo", iso_speed = 100, n = 3))
#' }

p_iso <- function(data, bg = "grey", theme = p_theme()){

  ggplot2::ggplot(data) +

    ggplot2::geom_point(ggplot2::aes(x = .data$iso_speed,
                                     y = .data$lens_model,
                                     size = .data$n),
                      colour = bg,
                      alpha = .5,
                      show.legend = FALSE) +

    ggplot2::ggtitle("ISO Speed") +

    theme

}

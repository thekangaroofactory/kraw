

#' Camera Plot
#'
#' @param data a data.frame (see details).
#' @param fg a foreground color.
#' @param bg a background color.
#' @param theme an optional theme function.
#'
#' @details
#' `data` expects a data.frame with the following columns:
#' - camera: character, the camera name
#' - n: numeric, the number of images for the camera
#'
#' @seealso [p_theme()]
#'
#' @returns a 'ggplot' object.
#' @export
#' @importFrom ggplot2 .data
#'
#' @examples
#' p_camera(data.frame(camera = "Canon EOS 70D", n = 10))

p_camera <- function(data, fg = NA, bg = "grey", theme = p_theme()){

  # -- add rank & scale n
  data$rank <- order(data$n, decreasing = T)
  data$n <- data$n / max(data$n) / 2

  # -- init
  ggplot2::ggplot(data) +

    # -- circles
    ggforce::geom_circle(ggplot2::aes(x0 = .data$rank,
                                      y0 = 0,
                                      r = .data$n),
                         color = fg,
                         fill = bg) +

    # -- labels
    ggplot2::geom_text(ggplot2::aes(x = .data$rank,
                           label = .data$camera),
                       y = 0) +

    # ggplot2::ggtitle("Camera") +

    # -- scale
    # needed to ensure circles
    ggplot2::coord_fixed() +

    # -- theme
    theme +
    ggplot2::theme(
      axis.text = ggplot2::element_blank())

}

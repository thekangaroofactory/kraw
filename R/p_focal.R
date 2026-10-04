

#' Focal Length Plot
#'
#' @param data a data.frame (see details).
#' @param fg a foreground color.
#' @param bg a background color.
#' @param theme an optional theme function.
#'
#' @details
#' `data` expects the following columns:
#' - lens_model: character, the name of the lens
#' - focal_length: numeric, the focal length
#' - n: the number of images.
#'
#' @returns a 'ggplot' object.
#' @export
#' @importFrom ggplot2 .data
#'
#' @examples
#' \dontrun{
#' p_focal(data = metadata |>
#' dplyr::group_by(lens_model, focal_length) |>
#' dplyr::summarise(n = dplyr::n()))
#' }

p_focal <- function(data, fg = "grey", bg = "grey", theme = p_theme()){

  ggplot2::ggplot(data, aes(x = .data$focal_length,
                         y = .data$n,
                         group = .data$lens_model)) +

    geom_area(fill = bg,
              alpha = 0.5) +
    geom_line(colour = fg,
              alpha = 0.5) +

    geom_point(size = 2, alpha = 0.25) +
    geom_point(aes(
      colour = .data$lens_model),
      size = 1,
      alpha = 0.5,
      show.legend = FALSE) +

    # -- title
    ggplot2::ggtitle("Focal length") +

    # -- apply theme
    theme +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank())

}

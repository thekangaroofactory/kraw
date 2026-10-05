

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

  data_range <- data |>
    dplyr::group_by(lens_model) |>
    dplyr::summarise(min = min(focal_length),
                     max = max(focal_length))

  # -- init
  ggplot2::ggplot(data,
                  ggplot2::aes(group = lens_model)) +

    # -- background
    ggplot2::geom_rect(data = data_range,
                       ggplot2::aes(xmin = min,
                                    xmax = max,
                                    y = lens_model),
                       height = .5,
                       fill = bg,
                       alpha = .1) +

    ggplot2::geom_segment(data = data_range,
                          ggplot2::aes(x = min,
                                       xend = max,
                                       y = lens_model),
                          lineend = "round",
                          alpha = .05) +

    # -- foreground
    ggplot2::geom_point(ggplot2::aes(x = focal_length,
                                     y = lens_model,
                                     size = n),
                        fill = bg,
                        colour = fg,
                        alpha = 0.75,
                        show.legend = F) +

    # -- title
    ggplot2::ggtitle("Focal length") +

    # -- apply theme
    theme

}

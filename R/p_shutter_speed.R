

#' Shutter Speed Plot
#'
#' @param data a data.frame (see details).
#' @param fg a foreground color.
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
#' \dontrun{
#' p_shutter_speed(data)
#' }

p_shutter_speed <- function(data, fg = "grey", bg = "grey", theme = p_theme()){

  # compute sequence (order)
  ref <- names(sort(sapply(unique(data$exposure_time), function(x) eval(parse(text = x)))))

  # init
  ggplot2::ggplot(data,
                  ggplot2::aes(x = .data$lens_model,
                               y = factor(.data$exposure_time, levels = ref),
                               group = .data$lens_model)) +

    # density
    see::geom_violinhalf(colour = fg,
                         fill = bg) +

    # flip horizontal
    ggplot2::coord_flip() +
    ggplot2::scale_y_discrete(breaks = ref[c(TRUE, FALSE, FALSE, FALSE, FALSE)]) +

    # tittle
    ggplot2::ggtitle("Shutter speed") +

    # apply theme
    theme

}

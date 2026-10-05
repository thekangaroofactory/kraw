

#' Aperture Plot
#'
#' @param data a data.frame (see details).
#' @param fg a foreground color.
#' @param bg a background color.
#' @param theme an optional theme function.
#'
#' @details
#' `data` expects the following columns:
#' - lens_model: character, the name of the lens
#' - f_number: character, the aperture value
#'
#' @returns a 'ggplot' object.
#' @export
#' @importFrom ggplot2 .data
#'
#' @examples
#' \dontrun{
#' p_aperture(data)
#' }

p_aperture <- function(data, fg = "grey", bg = "grey", theme = p_theme()){

  # compute sequence (to order f_number)
  ref <- unique(data$f_number)[order(as.numeric(gsub("f/", "", unique(data$f_number))))]

  # init
  ggplot2::ggplot(data,
                  ggplot2::aes(x = lens_model,
                               y = factor(f_number, levels = ref),
                               group = lens_model)) +

    # density
    see::geom_violinhalf(colour = fg,
                         fill = bg) +

    # flip horizontal
    ggplot2::coord_flip() +

    # tittle
    ggplot2::ggtitle("Aperture") +

    # apply theme
    theme

}

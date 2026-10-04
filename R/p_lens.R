

#' Lens Plot
#'
#' @description
#' Raw images per lens type.
#'
#' @param data a data.frame (see details).
#' @param fg a foreground color.
#' @param bg a background color.
#' @param theme an optional theme function.
#'
#' @details
#' `data` expects the following columns:
#' - lens_model: a character string, the name of the lens
#' - n: numeric, the number of images for this lens
#'
#' @seealso [p_theme()]
#'
#' @returns a 'ggplot' object.
#' @export
#' @importFrom ggplot2 .data

p_lens <- function(data, fg = "#000", bg = "grey", theme = p_theme()){

  # -- reorder
  data$y <- stats::reorder(data$lens_model, data$n)

  # -- init
  ggplot2::ggplot(data) +

    # -- segment
    ggplot2::geom_segment(ggplot2::aes(xend = .data$n,
                                       y = y,
                                       colour = .data$lens_model),
                          x = 0,
                          linewidth = 4,
                          alpha = .5,
                          lineend = "round",
                          show.legend = FALSE) +

    # -- labels
    ggplot2::geom_text(ggplot2::aes(y = y,
                                    label = .data$lens_model),
                       x = 0,
                       nudge_y = 0.1,
                       hjust = 0) +

    ggplot2::geom_text(ggplot2::aes(x = .data$n + 3,
                                    y = y,
                                    label = .data$n),
                       hjust = 0) +

    # -- title
    ggplot2::ggtitle("Lens") +

    # -- scale
    ggplot2::xlim(c(0, max(data$n) + 5)) +

    # -- apply theme
    theme +
    ggplot2::theme(
      axis.text = ggplot2::element_blank())

}

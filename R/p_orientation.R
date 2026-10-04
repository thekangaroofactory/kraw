

#' Orientation Plot
#'
#' @description
#' Distribution over landscape and portrait.
#'
#' @param nl a numeric, the number of landscape.
#' @param np a numeric, the number of portraits.
#' @param fg a foreground color.
#' @param bg a background color.
#' @param theme an optional theme function.
#'
#' @seealso [p_theme()]
#'
#' @returns a 'ggplot' object.
#' @export
#'
#' @examples
#' p_orientation(nl = 42, np = 112)

p_orientation <- function(nl, np, fg = "black", bg = "grey", theme = p_theme()){

  # -- init
  ggplot2::ggplot() +

    # -- scale
    ggplot2::annotate("segment",
                      x = 0,
                      xend = 100,
                      y = 0,
                      linewidth = 4,
                      lineend = "round",
                      colour = bg) +

    # -- cursor
    ggplot2::annotate("point",
                      x = nl / (np + nl) * 100,
                      y = 0,
                      shape = 1,
                      size = 2,
                      colour = fg) +

    # -- text
    ggplot2::annotate("text",
                      x = 0,
                      y = 0.05,
                      label = "Landcape",
                      size = 9 * nl / (np + nl),
                      hjust = 0) +
    ggplot2::annotate("text",
                      x = 100,
                      y = 0.05,
                      label = "Portrait",
                      size = 9 * np / (np + nl),
                      hjust = 1) +

    # -- axis
    ggplot2::xlim(c(0, 100)) +
    ggplot2::ylim(c(-.25, .25)) +

    # -- title
    # ggplot2::ggtitle("Orientation") +

    # -- apply theme
    theme +
    ggplot2::theme(
      axis.text = ggplot2::element_blank())

}



#' Default Theme
#'
#' @details
#' The function is used as a default theme for the plots, which
#' can be replaced by a custom one.
#'
#' @returns a 'ggplot' theme.
#' @export
#'
#' @examples
#' p_theme()

p_theme <- function(){

  ggplot2::theme_minimal() +
    ggplot2::theme(

      # -- panel & background
      panel.grid = ggplot2::element_blank(),

      # -- axis
      axis.title = ggplot2::element_blank()

      )

}

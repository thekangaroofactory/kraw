

#' Overview Plot
#'
#' @param metadata a data.frame (see details).
#'
#' @details
#' `metadata` expects an output from the [scan()] function.
#'
#' @seealso [scan()]
#'
#' @returns a ggplot object.
#' @export
#' @importFrom ggplot2 .data
#'
#' @examples
#' \dontrun{
#' viz(scan(path = "."))
#' }

p_overview <- function(metadata){

  # ////////////////////////////////////////////////////////////////////////////
  # Plots

  # -- camera
  camera <- p_camera(data = metadata |>
                       dplyr::group_by(camera) |>
                       dplyr::summarise(n = dplyr::n()),
                     bg = "#D6CCC2")


  # -- lens
  lens_model <- ggplot2::ggplot(metadata |>
                                  dplyr::group_by(lens_model) |>
                                  dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$n,
                                   y = stats::reorder(lens_model, .data$n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Lens") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- orientation
  orientation <- p_orientation(nl = sum(metadata$orientation == 1),
                               np = sum(metadata$orientation == 8),
                               fg = "#000",
                               bg = "#D6CCC2")

  # -- focal length
  focal_length <- ggplot2::ggplot(metadata |>
                                    dplyr::group_by(focal_length) |>
                                    dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$n,
                                   y = stats::reorder(focal_length, .data$n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Focal Lengths") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- ISO
  iso_speed <- ggplot2::ggplot(metadata |>
                                 dplyr::group_by(iso_speed) |>
                                 dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$n,
                                   y = stats::reorder(iso_speed, .data$n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("ISO Speed") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- Shutter speed
  exposure_time <- ggplot2::ggplot(metadata |>
                                     dplyr::group_by(exposure_time) |>
                                     dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$n,
                                   y = stats::reorder(exposure_time, .data$n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Shutter Speed") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- Aperture
  f_number <- ggplot2::ggplot(metadata |>
                                dplyr::group_by(f_number) |>
                                dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$n,
                                   y = stats::reorder(f_number, .data$n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Aperture") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # ////////////////////////////////////////////////////////////////////////////
  # Legend

  label = paste(nrow(metadata), paste0("RAW Image", if(nrow(metadata) > 1) "s", "\n"),
                nrow(camera$data), paste0("Camera", if(nrow(camera$data) > 1) "s", "\n"),
                nrow(lens_model$data), paste0("Lense", if(nrow(lens_model$data) > 1) "s"), sep = "")

  legend <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       x = 0, y = 0.2, hjust = 0, vjust = 0,
                       size = 12, color = "#BEAD9D", lineheight = 0.7)


  # ////////////////////////////////////////////////////////////////////////////
  # Layout & return

  ggpubr::ggarrange(legend, camera, orientation,
                    lens_model, f_number, focal_length,
                    exposure_time, iso_speed,
                    ncol = 3, nrow = 3,
                    heights = c(1, 2, 1)) +
    ggpubr::bgcolor("#FFF")

}



#' Metadata Visualization
#'
#' @param metadata a data.frame of the metadata (output of scan function)
#'
#' @returns a ggplot object
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
#' \dontrun{
#' viz(scan(path = "."))
#' }

viz <- function(metadata){

  # ----------------------------------------------------------------------------
  # Plots
  # ----------------------------------------------------------------------------

  # -- camera
  camera <- ggplot2::ggplot(metadata %>%
                              dplyr::group_by(camera) %>%
                              dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = n,
                                   y = camera),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Camera") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- lens
  lens_model <- ggplot2::ggplot(metadata %>%
                                  dplyr::group_by(lens_model) %>%
                                  dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = n,
                                   y = stats::reorder(lens_model, n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Lens") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- orientation
  orientation <- ggplot2::ggplot(metadata %>%
                                   dplyr::group_by(orientation) %>%
                                   dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = n,
                                   y = stats::reorder(orientation, n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Orientation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- focal length
  focal_length <- ggplot2::ggplot(metadata %>%
                                    dplyr::group_by(focal_length) %>%
                                    dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = n,
                                   y = stats::reorder(focal_length, n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Focal Lengths") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- ISO
  iso_speed <- ggplot2::ggplot(metadata %>%
                                 dplyr::group_by(iso_speed) %>%
                                 dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = n,
                                   y = stats::reorder(iso_speed, n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("ISO Speed") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- Shutter speed
  exposure_time <- ggplot2::ggplot(metadata %>%
                                     dplyr::group_by(exposure_time) %>%
                                     dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = n,
                                   y = stats::reorder(exposure_time, n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Shutter Speed") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # -- Aperture
  f_number <- ggplot2::ggplot(metadata %>%
                                dplyr::group_by(f_number) %>%
                                dplyr::summarise(n = dplyr::n())) +
    ggplot2::geom_bar(ggplot2::aes(x = n,
                                   y = stats::reorder(f_number, n)),
                      stat = "identity",
                      fill = "#D6CCC2",
                      show.legend = FALSE) +
    ggplot2::ggtitle("Aperture") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  # ----------------------------------------------------------------------------
  # Legend
  # ----------------------------------------------------------------------------

  label = paste(nrow(metadata), "RAW Images\n",
                nrow(camera$data), "Cameras\n",
                nrow(lens_model$data), "Lenses", sep = "")

  legend <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       x = 0, y = 0.2, hjust = 0, vjust = 0,
                       size = 12, color = "#BEAD9D", lineheight = 0.7)


  # ----------------------------------------------------------------------------
  # Layout & return
  # ----------------------------------------------------------------------------

  ggpubr::ggarrange(legend, camera, lens_model,
                    exposure_time, f_number, focal_length, iso_speed, orientation,
                    ncol = 3, nrow = 3,
                    heights = c(1, 2, 1)) +
    ggpubr::bgcolor("#FFF")

}

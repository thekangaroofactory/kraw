


p_favorite <- function(data, theme = p_theme()){

  ggplot2::ggplot(data) +
    ggplot2::annotate("text", x = 0, y = 0, label = data$camera, hjust = 0) +
    ggplot2::annotate("text", x = 0, y = -1, label = data$lens_model, hjust = 0) +
    ggplot2::annotate("text", x = 0, y = -2, label = data$orientation, hjust = 0) +
    ggplot2::annotate("text", x = 0, y = -3, label = data$focal_length, hjust = 0) +
    ggplot2::annotate("text", x = 0, y = -4, label = data$f_number, hjust = 0) +
    ggplot2::annotate("text", x = 0, y = -5, label = data$exposure_time, hjust = 0) +
    ggplot2::annotate("text", x = 0, y = -6, label = data$iso_speed, hjust = 0) +

    theme +
    ggplot2::theme(
      axis.text = ggplot2::element_blank())

}

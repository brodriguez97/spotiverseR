
create_lr_one <- function(data, param, param2, line = "white",
                          points = "green", theme = "dark") {
  require(rlang)
  require(ggplot2)
  if (theme == "dark") {
    t <- theme_dark()
  } else {
    t <- theme_light()
  }
  ggplot(
    data = data,
    mapping = aes(
      x = !!param,
      y = !!param2
    )
  ) +
    geom_point(
      color = points,
      alpha = .7
    ) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      size = 1.5,
      color = line
    ) +
    t
}

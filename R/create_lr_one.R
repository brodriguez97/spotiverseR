#'@title Create Linear Regression Model Helper Function
#'
#'
#'@description
#'\code{create_lr_one} takes a dataframe, two arguments, a line color, a point color, and a theme. Plots a single
#'linear regression model.
#'
#'
#'@details
#'This function uses \code{ggplot2} and \code{rlang}.
#'
#'
#'@param data a dataframe of a user's playlists.
#'@param param a numeric variable in data
#'@param param another numeric variable in data
#'@param line a string with a color name 
#'@param points a string with a color name 
#'@param theme a string that has valid values "light" or "dark"
#'
#'@import rlang
#'@import ggplot2
#'@return linear regression model a user's combined playlists.
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <spham@@wesleyan.edu>
#'
#'@examples
#'create_lr_one(
#'data = christmas_playlists,
#'param = tempo,
#'param2 = valence,
#'points = "darkgreen",
#'line = "red",
#'theme = "light",
#')
create_lr_one <- function(data, param, param2, line = "white",
                          points = "green", theme = "dark") {
  if (theme == "dark") {
    t <- theme_dark()
  } else {
    t <- theme_light()
  }
  g <- ggplot(
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
  return(g)
}
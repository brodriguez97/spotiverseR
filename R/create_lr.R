#'@title Create Linear Regression Model
#'
#'
#'@description
#'\code{create_lr} takes a dataframe, two arguments, a line color, a point color, a theme,
#'and a boolean indicating if the user wants to see a multiple linear regression models per playlist.
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
#'@param multi a boolean
#'
#'@import rlang
#'@import ggplot2
#'@return linear regression model(s) for each user's playlists or combined playlists.
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <spham@@wesleyan.edu>
#'
#'@export
#'@examples
#'create_lr(
#'data = christmas_playlists,
#'param = tempo,
#'param2 = valence,
#'points = "darkgreen",
#'line = "red",
#'theme = "light",
#'multi = F
#')
create_lr <- function(data = data, param, param2, line = "white",
                      points = "green", theme = "dark",
                      multi = T) {
  require(rlang)
  require(ggplot2)
  param <- enquo(param)
  param2 <- enquo(param2)
  if (multi == F) {
    create_lr_one(data = data, param = param, param2 = param2, line = line, points = points, theme = theme)
  } else {
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
      facet_wrap(~playlist_name) +
      t
  }
}



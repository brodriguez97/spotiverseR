#'@title Create Joyplot
#'
#'
#'@description
#'\code{create_joyplot} takes a dataframe, one argument, and an optional color vector
#'to plot a joyplot containing playlists and the distributions of a numeric variable.
#'
#'
#'@details
#'This function uses \code{ggplot2},\code{rlang}, and \code{ggridges}.
#'
#'
#'@param data a dataframe of a user's playlists.
#'@param param a numeric variable in data
#'@param color a character vector of colors
#'
#'@import ggplot2
#'@import ggridges
#'@import rlang
#'@return a joyplot containing playlists and the distributions of a numeric variable.
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <spham@@wesleyan.edu>
#'
#'@export
#'@examples
#'data(christmas_playlists)
#'create_joyplot(christmas_playlists, valence, c("red","green"))
#'
create_joyplot <- function(data = data, param, color = c("blue", "green")) {
  require(ggplot2)
  require(ggridges)
  require(rlang)
  param <- enquo(param)
  ggplot(
    data,
    aes(
      x = !!param,
      y = playlist_name,
      fill = playlist_name
    )
  ) +
    geom_density_ridges() +
    scale_fill_cyclical(values = color) +
    theme_ridges() +
    ylab("playlist name") +
    theme_ridges(font_size = 13, grid = TRUE) + theme(
      legend.position = "none",
      axis.title.y = element_blank()
    )
}

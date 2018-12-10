#'@title Create a Histogram Containing Mean Variable Values
#'
#'
#'@description
#'\code{create_mean_hist} takes a dataframe, one argument, a color, and a border color.
#'
#'@details
#'This function uses \code{ggplot2} and \code{dplye}.
#'
#'
#'@param data a dataframe of a user's playlists.
#'@param param a numeric variable in data
#'@param color a string with a color name.
#'@param border a string with a color name.
#'
#'@import ggplot2
#'@import dplyr
#'@return a histogram containing mean variable values
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <spham@@wesleyan.edu>
#'
#'@export
#'@examples
#'create_mean_hist(d, energy, color = "red", border = "darkgreen")


create_mean_hist <- function(data = data, param, color = "pink", border = "darkred") {
  require(ggplot2)
  require(dplyr)
  param <- enquo(param)
  plotdata <- data %>%
    group_by(playlist_name) %>%
    summarize(mean_param = mean(!!param))
  
  # plot mean
  ggplot(
    plotdata,
    aes(
      x = playlist_name,
      y = mean_param
    )
  ) +
    geom_bar(
      stat = "identity",
      fill = color,
      color = border
    ) +
    xlab("playlist name") +
    ylab(param) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}
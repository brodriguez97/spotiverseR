#'@title Filter Data by a Substring
#'
#'
#'@description
#'\code{filter_by_ss} locates the desired substring in the column playlist_name from the Christmas playlist
#'dataframe.
#'
#'@details
#'This function takes in a character string and subsets all the playlist names in the playlist_name
#'column that contains that substring.
#'
#'@param data a dataframe of a user's playlists
#'@param col playlist_name column set as default
#'@param substring a character string
#'
#'@return a data frame
#'
#'@import rlang
#'@import dplyr
#'@import stringr
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <kpham@@wesleyan.edu>
#'
#'@export
#'@examples
#'data(christmas_playlists)
#'filter_by_ss(data = christmas_playlists, col=playlist_name, substring="Jazz")

filter_by_ss <- function(data = data, col = playlist_name, substring) {
  require(rlang)
  require(dplyr)
  require(stringr)
  col <- enquo(col)
  new_data <- filter(data, str_detect(!!col, substring))
  return(new_data)
}

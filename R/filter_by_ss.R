#'@title filter_by_ss
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
#'filter_by_ss(col=playlist_name, substring="jazz")

filter_by_ss <- function(col = playlist_name, substring) {
  require(rlang)
  require(dplyr)
  require(stringr)
  col <- enquo(col)
  new_data <- filter(data, str_detect(!!col, substring))
  return(new_data)
}
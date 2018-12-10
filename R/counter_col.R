#'@title Counter Column
#'
#'
#'@description
#'\code{counter_col} takes a dataframe and creates a new column such that tracks are numbered
#'one through n per playlist.
#'
#'
#'@details
#'This function uses \code{dplyr}.
#'
#'
#'@param df a dataframe of a user's playlists.
#'
#'@import dplyr
#'@return a dataframe with a new column called counter.
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <spham@@wesleyan.edu>
#'
#'@export
#'@examples
#'df <- counter_col(christmas_playlists)
#'View(df)
counter_col <- function(df) {
  require(dplyr)
  df <- df %>%
    group_by(playlist_name) %>%
    mutate(counter = row_number())
  return(df)
}

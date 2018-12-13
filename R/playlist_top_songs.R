#'@title Get Top Songs From a User Playlist
#'
#'
#'@description
#'\code{playlist_top_songs} produces a dataframe with the top 5 songs in a given playlist regarding the given
#'parameter.
#'
#'@details
#'This function takes in a string of a playlist and a string of parameter and returns a dataframe
#'with the top 5 songs
#'
#'@param data a dataframe of a user's playlists
#'@param playlist a character string of playlist to be analyzed
#'@param param a numeric variable in data (Spotify parameter)
#'@param top an integer indicating the number of songs for the outpit
#'@param asc a boolean value indicating whether the top songs should go or not go in ascending order within the output (according to their value)
#'@param kable a boolean value indicating whether the dataframe output should be styled according to "bg" and "text" 
#'@param bg a character string indicating the color for the background
#'@param text a character string indicating the color for the text
#'
#'@return a dataframe
#'
#'@import dplyr
#'@import knitr
#'@import rlang
#'@import kableExtra
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <kpham@@wesleyan.edu>
#'
#'@export
#'@examples
#'data(christmas_playlists)
#'playlist_top_songs(data = christmas_playlists, "Christmas Jazz", valence, asc = T, kable = F, top = 10)

playlist_top_songs <- function(data = data, playlist, param, top = 5, asc = T, kable = T, bg = "azure", text = "palevioletred") {
  require(dplyr)
  require(knitr)
  require(kableExtra)
  require(rlang)
  param <- enquo(param)
  df <- filter(data, playlist_name == playlist)
  if (asc) {
    df <- arrange(df, -!!param)
  } else {
    df <- arrange(df, !!param)
  }
  df <- select(df, track_name, !!param) %>%
    head(top)
  if (kable){
    df <- df %>% kable() %>%
      kable_styling(full_width = F, position = "left") %>%
      row_spec(row = 1:nrow(df), background = bg, color = text)
    return(df)
  } else {
    return(df)
  }
}

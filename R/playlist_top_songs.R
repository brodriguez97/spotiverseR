#'@title playlist_top_songs
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
#'@param playlist a character string
#'@param param a numeric variable in data
#'@param top an integer
#'@param asc a boolean
#'@param kable a boolean
#'@param bg a character string
#'@param text a character string
#'
#'@return a dataframe
#'
#'@import dplyr
#'@import knitr
#'@import formattable
#'@import kableExtra
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <kpham@@wesleyan.edu>
#'
#'@export
#'@examples
#'playlist_top_songs("Christmas Jazz", valence, asc = T, top = 10, bg = "lightgreen", text = "red")

# get top songs for a given variable and playlist
playlist_top_songs <- function(playlist, param, top = 5, asc = T, kable = T, bg = "azure", text = "palevioletred") {
  require(dplyr)
  require(knitr)
  require(formattable)
  require(kableExtra)
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

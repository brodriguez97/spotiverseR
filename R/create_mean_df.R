#'@title Create a Dataframe or Table Containing Mean Variable Values
#'
#'
#'@description
#'\code{create_mean_df} takes a dataframe, one argument, an ascending order boolean value, a kable table boolean value,
#'a background color, and a text color.
#'
#'
#'@details
#'This function uses \code{knitr},\code{dplyr},\code{kableExtra}, and \code{rlang}.
#'
#'
#'@param data a dataframe of a user's playlists.
#'@param param a numeric variable in data
#'@param asc a boolean to indicate the order of the result.
#'@param kable a boolean to indicate if user's wants to use kable on the result.
#'@param bg a string with a color name
#'@param text a string with a color name
#'
#'@import knitr
#'@import dplyr
#'@import kableExtra
#'@import rlang
#'@return a dataframe or table containing mean variable values
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <spham@@wesleyan.edu>
#'
#'@export
#'@examples
#'data(christmas_playlists)
#'create_mean_df(christmas_playlists, valence, asc = F, kable=F)

create_mean_df <- function(data = data, param, asc = T, kable = F, bg ="green", text = "red") {
  require(knitr)
  require(dplyr)
  require(kableExtra)
  require(rlang)
  param <- enquo(param)
  dataset <- group_by(data, playlist_name)
  df <- aggregate(dataset[, quo_name(param)], list(dataset$playlist_name), mean)
  if (asc) {
    df <- df %>% arrange(!!param)
  } else {
    df <- df %>% arrange(desc(!!param))
  }
  avg <- paste("mean (", quo_name(param), ")", sep = "")
  colnames(df) <- c("playlist", avg)
  if (!kable) {
    return(df)
  } else {
    df <- df %>% kable() %>%
      kable_styling(full_width = F, position = "left") %>%
      row_spec(row = 1:nrow(df), background = bg, color = text)
    return(df)
  }
}

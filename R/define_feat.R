#'@title define_feat
#'
#'
#'@description
#'\code{define_feat} displays the description of the Spotify parameter the user wishes to see.
#'
#'
#'@details
#'This function contains the descriptions of all parameters as they are written in the
#'Spotify API within a dataframe. The user can see the description of a parameter when they 
#'input the parameter name.
#'
#'@param feat a numeric variable in data
#'
#'@return a value in the definitions data frame
#'
#'@import dplyr
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <kpham@@wesleyan.edu>
#'
#'@export
#'@examples
#'define_feat(valence)


define_feat <- function(feat) {
  require(dplyr)
  feat <- enquo(feat)
  dfx <- feat_attributes()
  dfx <- filter(dfx, KEY == quo_name(feat))
  return(dfx$`VALUE DESCRIPTION`)
}
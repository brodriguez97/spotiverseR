#'@title Define a Feature
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
#'@import rlang
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <kpham@@wesleyan.edu>
#'
#'@export
#'@examples
#'define_feat(valence)

#feats <- c("duration_ms","key","mode","time_signature","acousticness","danceability","energy","instrumentalness","liveness","loudness","speechiness","valence","tempo")

define_feat <- function(feat) {
    require(dplyr)
    require(rlang)
  feat <- enquo(feat)
  dfx <- feat_attributes()

    tryCatch(feat <- enquo(feat),
             message = function(m) {
               stop("invalid input")
             },
             warning = function(w) {
               stop("invalid input")

             },
             error = function(e) {
               stop("invalid input")
             },
             finally = {
               stop("invalid input")
             }
             )

    dfx <- feat_attributes()
    dfx <- filter(dfx, KEY == quo_name(feat))
    return(dfx$`VALUE DESCRIPTION`)

}

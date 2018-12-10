#'@title feat_names
#'
#'
#'@description
#'\code{feat_names} produces a character vector with the names of all parameters.
#'
#'@details
#'This function does not take in any arguments, it just returns a character vector with the
#'names of all parameters when you type in feat_names().
#'
#'
#'@return a character vector
#'
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <kpham@@wesleyan.edu>
#'
#'@export
#'@examples
#'feat_names()

feat_names <- function() {
  dfx <- feat_attributes()
  return(dfx$KEY)
}
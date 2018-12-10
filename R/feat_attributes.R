#'@title Get Feature Attributes
#'
#'
#'@description
#'\code{feat_attributes} returns a description in the console with the contents of the definitions.Rda file.
#'
#'@details
#'This function does not take in any arguments. It is called on its own and returns
#'a description in the console with the contents of the definitions.Rda file.
#'
#'
#'@return description in console
#'
#'
#'@author Belen Rodriguez <brodriguez@@wesleyan.edu>
#'@author Kim Pham <kpham@@wesleyan.edu>
#'
#'@export
#'@examples
#'feat_attributes()

feat_attributes <- function() {
  data(definitions)
  return(definitions)
}

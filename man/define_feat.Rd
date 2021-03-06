% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define_feat.R
\name{define_feat}
\alias{define_feat}
\title{Define a Feature}
\usage{
define_feat(feat)
}
\arguments{
\item{feat}{a numeric variable in data}
}
\value{
a value in the definitions data frame
}
\description{
\code{define_feat} displays the description of the Spotify parameter the user wishes to see.
}
\details{
This function contains the descriptions of all parameters as they are written in the
Spotify API within a dataframe. The user can see the description of a parameter when they
input the parameter name.
}
\examples{
define_feat(valence)
}
\author{
Belen Rodriguez <brodriguez@wesleyan.edu>

Kim Pham <kpham@wesleyan.edu>
}

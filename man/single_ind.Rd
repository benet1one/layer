% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shorts.R
\name{single_ind}
\alias{single_ind}
\title{Array indices to single indices.}
\usage{
single_ind(ind, arr, .dim = dim(arr))
}
\arguments{
\item{ind}{Matrix with n columns, where n is the number of dimensions.}

\item{arr}{Optional. Array to use. Only extracts dimensions.}

\item{.dim}{Dimensions of the array.}
}
\value{
Integer vector with the single indices for the array.
}
\description{
The inverse of \link{arrayInd}
}
\examples{
si <- c(1, 4, 7, 16)
my_ind <- arrayInd(si, .dim = c(4, 6))
single_ind(my_ind, .dim = c(4, 6))
}

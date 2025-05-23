% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shorts.R
\name{\%or\%}
\alias{\%or\%}
\title{Return the first element unless it's null, in which case returns the second.}
\usage{
e1 \%or\% e2
}
\description{
Return the first element unless it's null, in which case returns the second.
}
\examples{
x <- runif(5)
y <- names(x) \%or\% seq_along(x)
print(y)
names(x) <- letters[1:5]
y <- names(x) \%or\% seq_along(x)
print(y)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functional.R
\name{list_to_function}
\alias{list_to_function}
\title{Convert a list of functions to a single function.}
\usage{
list_to_function(fun_list, simplify = TRUE)
}
\arguments{
\item{fun_list}{A list of functions.}
}
\value{
A single function that applies all the functions in the list to
it's argument \code{x}.
}
\description{
Useful for writing clean code.
}
\examples{
funs <- list(
    f1 = \(x) x + 1,
    f2 = \(x) x*4,
    f3 = \(x) x^2
)

my_fun <- list_to_function(funs)
my_fun(1:5)
}
\seealso{
[function_list()]
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strings.R
\name{str_replace_if_any}
\alias{str_replace_if_any}
\title{Avoid evaluation of replacement unless pattern is matched on string.
Efficient and elegant.}
\usage{
str_replace_if_any(
  string,
  pattern,
  replacement,
  all = FALSE,
  envir = parent.frame()
)
}
\arguments{
\item{string}{A character vector.}

\item{pattern}{Pattern to match against.}

\item{replacement}{Replacement, defused inside of function and
evaluated on \code{envir}.}

\item{all}{Whether to replace all matches (TRUE) or just the first (FALSE).}

\item{envir}{Environment where replacement will be evaluated.}
}
\value{
Character vector of the same length as \code{string}
}
\description{
Avoid evaluation of replacement unless pattern is matched on string.
Efficient and elegant.
}

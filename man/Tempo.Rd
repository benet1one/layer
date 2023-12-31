% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/times.R
\docType{data}
\name{Tempo}
\alias{Tempo}
\alias{tempo}
\title{Create a tempo object.}
\format{
An object of class \code{list} of length 6.
}
\usage{
Tempo(seconds = 0, minutes = 0, hours = 0)

tempo$hour_of(x)
tempo$minute_of(x)
tempo$second_of(x)

tempo$in_hours(x)
tempo$in_minutes(x)
tempo$in_seconds(x)
}
\description{
To be used with the tempo function family or be formatted using
\link{format.tempo}
}
\examples{
my_tempo <- Tempo(3735)
my_tempo
tempo$minute_of(my_tempo)
tempo$in_minutes(my_tempo)
}
\keyword{datasets}

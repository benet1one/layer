

#' Create a tempo object.
#' @description
#' To be used with the tempo function family or be formatted using
#' \link{format.tempo}
#'
#' @export
#'
#' @examples
#' my_tempo <- Tempo(3735)
#' my_tempo
#' tempo$minute_of(my_tempo)
#' tempo$in_minutes(my_tempo)
Tempo <- function(seconds = 0, minutes = 0, hours = 0) {
    structure(3600*hours + 60*minutes + seconds,
              class = "tempo")
}

#' Tempo family
#' @rdname Tempo
#' @export
#' @usage
#' tempo$hour_of(x)
#' tempo$minute_of(x)
#' tempo$second_of(x)
#'
#' tempo$in_hours(x)
#' tempo$in_minutes(x)
#' tempo$in_seconds(x)
tempo <- list(
    hour_of    = function(x) unclass(x) %/% 3600,
    minute_of  = function(x) (unclass(x) %% 3600) %/% 60,
    second_of  = function(x) floor(unclass(x) %% 60),

    in_hours   = function(x) unclass(x) / 3600,
    in_minutes = function(x) unclass(x) / 60,
    in_seconds = function(x) unclass(x)
)

as.numeric.tempo <- function(x) unclass(x)
print.tempo <- function(x) print(format.tempo(x), quote = FALSE)

#' Format a tempo object.
#' with a flexible syntax.
#'
#' @param x A tempo object.
#' @param format Format to use. See \link{details}.
#' @param digits Number of digits to add to \code{%S}.
#' @param ... Ignored.
#'
#' @return A formatted character vector.
#' @export
#'
#' @details
#'
#' Argument \code{format} must be specified as follows:
#'
#' \code{\%H}  Hours. Can exceed 24.
#' \code{\%M}  Clock Minutes. Can't exceed 60.
#' \code{\%S}  Clock Seconds. Can't exceed 60.
#' \code{\%h}  Hours with decimals. Can exceed 24.
#' \code{\%m}  Total Minutes with decimals. Can exceed 60.
#' \code{\%s}  Total Seconds with decimals. Can exceed 60.
#'
#'
#' The number after is the minimum width of the string,
#' with the left part filled with zeros.
#'
#' Must be between 1 and 9. If not specified it's 2 by default.
#' It can be escaped using \\.
#'
#'
#'
#' @examples
#'
#' my_tempo <- Tempo(3735)
#' format(my_tempo)
#' format(my_tempo, "%H6, %M4, %S4")
#' format(my_tempo, "It's been %m minutes")
format.tempo <- function(x, format = "%H:%M:%S", digits = 3, ...) {

    if (digits > 0) {
        dec <- (tempo$in_seconds(x) %% 1) %>%
            format(digits = digits, nsmall = digits) %>%
            substring(2L, 2L + digits)
        format <- gsub("%S", paste0("%S", dec), format)
    }

    positions <- gregexec("%[HMShms][0-9]?", format)[[1]]
    no_width <- attr(positions, "match.length", exact = TRUE) == 2

    positions <- as.integer(positions)
    no_width <- as.logical(no_width)

    letters <- substring(format, positions + 1L, positions + 1L)
    widths  <- substring(format, positions + 2L, positions + 2L)
    widths[no_width] <- "2"
    widths <- as.integer(widths)

    calculated <- numeric(6) %>% set_names(c("H", "M", "S", "h", "m", "s"))
    to_calculate <- match(unique(letters), names(calculated))

    sapply(x, function(y) {

        for (i in to_calculate) {
            calculated[i] <- tempo[[i]](y)
        }

        values <- calculated[match(letters, names(calculated))]

        replacements <- mapply(formatC, x = values, width = widths, flag = "0")
        str_replace_vectorized(string = format, pattern = "%[HMShms][0-9]?",
                               replacements = replacements)
    })
}




#' Create a tempo object.
#' @description
#' To be used with the tempo function family or be formatted using
#' \link{format.tempo}
#'
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
#' @usage
#' tempo$hour_of(x)
#' tempo$minute_of(x)
#' tempo$second_of(x)
#'
#' tempo$in_hours(x)
#' tempo$in_minutes(x)
#' tempo$in_seconds(x)
tempo <- list(
    hour_of    = function(x) as.numeric(x) %/% 3600,
    minute_of  = function(x) (as.numeric(x) %% 3600) %/% 60,
    second_of  = function(x) floor(as.numeric(x) %% 60),

    in_hours   = function(x) as.numeric(x) / 3600,
    in_minutes = function(x) as.numeric(x) / 60,
    in_seconds = function(x) as.numeric(x)
)

#' @exportS3Method
as.numeric.tempo <- function(x) unclass(x)
#' @exportS3Method 
print.tempo <- function(x) 
    print(paste0(format.tempo(x), " "), quote = FALSE)

#' Format a tempo object.
#' with a flexible syntax.
#'
#' @param x A tempo object.
#' @param format Format to use. See \link{details}.
#' @param digits Number of digits to add to \code{\%S}.
#' @param trim_zero Whether to trim the first zero. Only works if it's the
#' first character of the formatted string.
#' @param ... Ignored.
#'
#' @return A formatted character vector.
#' 
#' @details
#'
#' Argument \code{format} must be specified as follows:
#'
#' - \code{\%H} : Hours. Can exceed 24.
#' - \code{\%M} : Clock Minutes. Cannot exceed 60.
#' - \code{\%S} : Clock Seconds. Cannot exceed 60.
#' - \code{\%h} : Hours, with 3 decimals by default. Can exceed 24. 
#' - \code{\%m} : Total Minutes. Can exceed 60.
#' - \code{\%s} : Total Seconds. Can exceed 60.
#'
#' @examples
#'
#' my_tempo <- Tempo(3735)
#' format(my_tempo)
#' format(my_tempo, "%H, %M, %S")
#' format(my_tempo, "It's been %m minutes")
format.tempo <- function(x, format = "%H:%M:%S", 
                         digits = c(H = 0, M = 0, S = 0, h = 3, m = 3, s = 3), 
                         trim_zero = FALSE, ...) {
    
    d <- c(H = 0, M = 0, S = 0, h = 3, m = 3, s = 3)
    
    if (!is.null(names(digits))) {
        d[names(digits)] <- digits
        unmatched <- setdiff(names(digits), names(d))
        
        if (length(unmatched) > 0L) {
            warning("unknown keys ", paste(unmatched, collapse = ", "))
        }
        
    } else if (length(digits) == 1L) {
        d[] <- digits
        
    } else {
        stop("`digits` must be a named integer vector or a single integer value")
    }
    
    values <- list(
        H = tempo$in_hours(x) |> 
            format_tempo_value(d["H"]),
        M = tempo$minute_of(x) |> 
            format_tempo_value(d["M"]),
        S = tempo$second_of(x) |> 
            format_tempo_value(d["S"]),
        h = tempo$in_hours(x) |> 
            round_or_floor(d["h"]) |> 
            format(nsmall = d["h"]) |> 
            trimws(),
        m = tempo$in_minutes(x) |> 
            round_or_floor(d["m"]) |> 
            format(nsmall = d["m"]) |> 
            trimws(),
        s = tempo$in_seconds(x) |> 
            round_or_floor(d["s"]) |> 
            format(nsmall = d["s"]) |> 
            trimws()
    )
    
    out <- rep(format, length(x))
    
    for (v in names(values)) {
        out <- stringr::str_replace(
            out, 
            pattern = paste0("%", v), 
            replacement = values[[v]]
        )
    }
    
    if (trim_zero) {
        out <- stringr::str_remove(out, "^0")
    }
    
    out
}

round_or_floor <- function(x, digits) {
    if (digits > 0L) 
        round(x, digits)  
    else 
        floor(x)
}

format_tempo_value <- function(x, digits) {
    x <- round_or_floor(x, digits)
    x <- format(x, nsmall = digits)
    paste0("00", x) |> 
        stringr::str_extract(r"(\d{2}(\.\d+)?$)")
}

# format.tempo <- function(x, format = "%H:%M:%S", digits = 0L, 
#                          trim_zero = FALSE, ...) {
# 
#     if (digits > 0) {
#         dec <- (tempo$in_seconds(x) %% 1) %>%
#             format(digits = digits, nsmall = digits) %>%
#             substring(2L, 2L + digits)
#         format <- stringr::str_replace(format, "%S", paste0("%S", dec))
#     }
# 
#     positions <- gregexec("%[HMShms]", format)[[1]] %>% as.integer()
#     letters <- substring(format, positions + 1L, positions + 1L)
# 
#     calculated <- numeric(6) %>% set_names(c("H", "M", "S", "h", "m", "s"))
#     to_calculate <- match(unique(letters), names(calculated))
# 
#     replaced <- mapply(x, format, FUN = function(y, f) {
# 
#         for (i in to_calculate) {
#             calculated[i] <- tempo[[i]](y)
#         }
# 
#         values <- calculated[match(letters, names(calculated))]
# 
#         replacements <- mapply(formatC, x = values, width = 2, flag = "0")
#         str_replace_vectorized(string = f, pattern = "%[HMShms]",
#                                replacements = replacements)
#     })
#     
#     if (trim_zero)
#         replaced <- stringr::str_remove(replaced, "^0")
#     replaced
# }

Math.tempo <- function(x, ...) {
    fun <- match.fun(.Generic)
    x <- as.numeric(x)
    x <- fun(x, ...)
    Tempo(x)
}

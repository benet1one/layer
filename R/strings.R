
#' Split a string while keeping the pattern.
#'
#' @param string The string to split.
#' @param pattern Pattern to split with.
#' @param where Where to attach pattern. Can be 'left', 'right' or 'middle',
#' which will keep the pattern as a separate string.
#'
#' @return A list of character vectors.
#' @export
str_split_keep <- function(string, pattern, where = "left") {
    
    require(stringr)
    fix <- switch(where,
                  "left" = \(x)  x[, 2L],
                  "right" = \(x) x[, 1L] - 1L,
                  "middle" = function(x) {
                      x[, 1L] %<>% subtract(1L)
                      c(t(x))
                  },
                  stop("{where} must be 'left', 'right' or 'middle'")
    )
    
    browser()
    pos <- str_locate_all(string, pattern) %>% lapply(fix)
    
    mapply(string, pos, SIMPLIFY = FALSE, FUN = function(str, p) {
        pstart <- c(1, p + 1)
        pend   <- c(p, nchar(str))
        str_sub(str, pstart, pend)
    })
}

#' Split strings by the character index
#'
#' @param string String to be split.
#' @param index Index at which to split.
#' @param where Where to insert the character at the indexes. Possible values
#' are 'left', 'right', 'middle', and 'remove'.
#'
#' @return The split string as a vector.
#' @export
str_split_index <- function(string, index, where = "left") {
    
    switch(where,
           "left"   = NULL,
           "right"  = index %<>% add(-1L),
           "middle" = index %<>% c(index - 1L),
           "remove" = index %<>% c(index - 1L),
           stop("{where} must be 'left', 'right', 'middle' or 'remove'")
    )
    
    index %<>% sort()
    
    pstart <- c(1L, index + 1L)
    pend   <- c(index, nchar(string))
    substring(string, pstart, pend) %>%
        pipe_if(where == "remove", odds)
}


#' Replace matches in a string with a transformed match.
#'
#' @param string A character vector.
#' @param pattern Pattern to match against.
#' @param fun Function to transform matches.
#' @param ... Arguments to be passed on to fun.
#'
#' @return The transformed string.
#' @export
str_replace_fun <- Vectorize(function(string, pattern, fun, ...) {
    
    require(stringr)
    
    replacement <- function(s, p) {
        str_sub(s, p[1], p[2]) %<>% fun(...)
        s
    }
    
    matches <- str_locate_all(string, pattern)[[1]] %>%
        apply(1L, identity, simplify = FALSE)
    
    Reduce(replacement, append(list(string), matches))
    
}, vectorize.args = c("string", "pattern"))


#' Avoid evaluation of replacement unless pattern is matched on string.
#' Efficient and elegant.
#'
#' @param string A character vector.
#' @param pattern Pattern to match against.
#' @param replacement Replacement, defused inside of function and
#' evaluated on \code{envir}.
#' @param all Whether to replace all matches (TRUE) or just the first (FALSE).
#' @param envir Environment where replacement will be evaluated.
#'
#' @return Character vector of the same length as \code{string}
#' @export
str_replace_if_any <- function(string, pattern, replacement, all = FALSE,
                               envir = parent.frame()) {
    
    require(stringr)
    replacement <- substitute(replacement)
    replace <- if (all) str_replace_all  else str_replace
    
    mapply(string, pattern, FUN = function(s, p) {
        if (!str_detect(s, p))
            s
        else
            replace(s, p, eval(replacement, envir) %>% as.character)
    })
}


# For now, keep in mind it can replace an already replaced
# part of the string, as long as it matches the pattern again.
str_replace_vectorized <- function(string, pattern, replacements) {
    
    f <- as.function(list(
        s = string,
        r = substitute(),
        p = pattern,
        
        quote(sub(x = s, pattern = p, replacement = r))
    ))
    
    Reduce(f, c(string, replacements))
}



#' Alias for paste(string, collapse = "")
#' @export
collapse <- function(string, sep = "") {
    paste(string, collapse = sep)
}

#' @export
`%!in%` <- function(e1, set) {
    !is.element(e1, set)
}

#' Check if an argument is missing, NULL, or of length 0.
#'
#' @param x Usually a function argument, can also be any object.
#' @export
len0 <- function(x) {
    tryCatch(length(x) == 0, error = function(e) TRUE)
}

#' Shortcut for \code{`storage.mode<-`(x, "integer")}
#'
#' @export
as_integer <- function(x) {
    storage.mode(x) <- "integer"
    x
}

#' Clear Screen, shortcut for \code{cat("\f")}
#' @export
cls <- function() {
    cat("\f")
}

#' Alias for \code{cat(sep = "")}
#' @export cat0
cat0 <- cat
formals(cat0)$sep <- ""

#' Concatenate and print, supports line breaks from double comma ',,'
#' @export
#' @examples catln("The answer to all is", 42,, "And 2 + 3 = ", 5)
catln <- function(..., sep = "") {
    dots <- as.list(substitute(...()))
    dots[sapply(dots, identical, substitute())] <- "\n"

    envir <- parent.frame()
    dots %<>% lapply(eval, envir = envir)

    do.call(cat, list(unlist(dots), sep = sep))
}

#' Alias for ColorRampPalette
#' @description
#' Uses non named arguments as colors and named arguments are passed on to
#' \link{colorRamp}
#' @seealso \link{colorRamp}
gradient_n <- function(...) {
    dots <- list(...)
    named <- names(dots) != ""
    colors <- dots[!named]
    args <- dots[named]

    do.call(colorRampPalette, list(colors = colors, args))
}

#' Comfortable character vectors
#' @description
#' Uses a split to separate elements without having to open and close
#' "" every time. Use \\ before the split to ignore it.
#'
#' @param ... Anything that can be coerced to character
#' @param .split Where to split individual strings.
#'
#' @export
Char <- function(..., .split = "; ") {

    string <- collapse(c(...))

    .only_split <- paste0("[^\\\\]", .split)
    .remove <- paste0("\\\\", .split)

    pos <- gregexpr(.only_split, string)[[1]] %>%
        c(pos + nchar(.split))

    str_split_index(string, pos) %>%
        odds() %>%
        gsub(pattern = .remove, replacement = .split)
}

#' Use commas as a thousand separator.
#' @description
#' Simple dirty hack
#'
#' @export
#' @examples
#' Num(1,025.37)
Num <- function(...) {
    list(...) %>%
        lapply(as.numeric) %>%
        unlist() %>%
        as.character() %>%
        sapply(function(y) {
            z <- strsplit(y, "\\.")[[1]][1]
            z <- max(3L - nchar(z), 0L)
            strrep("0", z) %>% paste0(y)
        }) %>%
        paste0(collapse = "") %>%
        as.numeric()
}



#' Comfortable control flow inside the pipe
#' @description
#' Allows the use of the dot anywhere. By default, if the condition is not met
#' returns itself. Parameters \code{.condition, .if, .else}  can be functions,
#' in which case they will be evaluated with \code{.data} as a parameter.
#'
#'
#' @param .data Input object.
#' @param .condition Condition.
#' @param .if Statement if condition is TRUE.
#' @param .else Statement if condition is FALSE.
#' @export
#'
#' @examples
#' 10 %>% pipe_if(TRUE, . + 2)
#' 10 %>% pipe_if(is.numeric, as.character)
pipe_if <- function(.data, .condition, .if, .else = .data) {

    if (is.function(.condition))
        .condition <- .condition(.data)
    if (is.function(.if))
        .if <- .if(.data)
    if (is.function(.else))
        .else <- .else(.data)

    if (.condition)
        .if
    else
        .else
}

#' Call "return" from another function's execution environment.
#'
#' @param x Object to return.
#' @param n Number of generations to go back. By default, it
#' executes \code{return(x)} at the function which calls it.
#' See \link{examples}. Ignored when 'envir' is specified.
#' @param envir Alternatively, the environment where \code{return(x)} is to be
#' executed. When specified, 'n' is ignored.
#'
#' @return Invisibly returns 'x'
#' @export
#'
#' @examples
#' f <- function() {
#'     return_at(1)
#' }
#'
#' g <- function() {
#'     f()
#'     return(0)
#' }
#'
#' result <- g()
#' result
return_at <- function(x, n = 1L, envir) {

    if (missing(envir)) {
        envir <- parent.frame(n + 1L)

    } else if (n != 1L) {
        warning("When 'envir' is specified, 'n' is ignored.")
    }

    do.call("return", list(x), envir = envir)
    invisible(x)
}


#' Factor as it should work in base R
#' @description
#' Levels are ordered as they appear in the data.
#'
#' @export
factor <- function(x, levels = unique(x), labels = levels,
                   exclude = NA, ordered = is.ordered(x), nmax = NA) {
    base::factor(x, levels, labels, exclude, ordered, nmax)
}


#' Alias for within(list(), {})
#'
#' @param expr {}
#'
#' @return A list environment, with expression executed.
#' @export
within_list <- function(expr) {
    within.list(list(), expr)
}


#' Overwrites default behavior of sample to avoid silent errors.
#' @description
#' When x is a length 1 integer, it won't use sample.int. See
#' the base definition of sample.
#' @export
#' @seealso \link{sample.int}
sample <- function (x, size, replace = FALSE, prob = NULL) {
    if (missing(size))
        size <- length(x)
    x[sample.int(length(x), size, replace, prob)]
}

#' Convert a list of functions to a single function.
#' @description
#' Useful for writing clean code.
#' @param fun_list A list of functions.
#' @return A single function that applies all the functions in the list to
#' it's argument \code{x}.
#' @export
#'
#' @examples
#' funs <- list(
#'     f1 = \(x) x + 1,
#'     f2 = \(x) x*4,
#'     f3 = \(x) x^2
#' )
#'
#' my_fun <- list_to_function(funs)
#' my_fun(1:5)
list_to_function <- function(fun_list, simplify = TRUE) {

    if (missing(fun_list))
        stop("Missing fun_list")

    function(x) {
        sapply(fun_list, \(f) do.call(f, list(x)), simplify = simplify)
    }
}


#' Keep even or odd indexes of a vector.
#' @export
evens <- function(x)  x[seq(2L, length(x), 2L)]

#' @export
#' @rdname evens
odds <- function(x)  x[seq(1L, length(x), 2L)]


#' Wrap a number between a minimum and a maximum, as understood in Blender
#'
#' @description
#' Simply calculates  \code{min + (x - min) \%\% (max - min)}
#'
#' @param x Number to wrap.
#' @param min Minimum, inclusive.
#' @param max Maximum, not inclusive.
#'
#' @details
#' When wrapping integers, it is sometimes useful for the Maximum to be
#' inclusive, instead of the Minimum.
#' To achieve this, just add +0.1 to both Minimum and Maximum.
#'
#' @export
wrap <- function(x, min, max) {
    min + (x - min) %% (max - min)
}


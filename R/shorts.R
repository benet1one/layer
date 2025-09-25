
#' Alias for paste(string, collapse = "")
#' @export
collapse <- function(string, sep = "") {
    paste(string, collapse = sep)
}

#' @export
`%!in%` <- function(e1, set) {
    !is.element(e1, set)
}

#' Return the first element unless it's null, in which case returns the second.
#' @examples
#' x <- runif(5)
#' y <- names(x) %or% seq_along(x)
#' print(y)
#' names(x) <- letters[1:5]
#' y <- names(x) %or% seq_along(x)
#' print(y)
#' @export
`%or%` <- function(e1, e2) {
    if (!is.null(e1)) e1  else e2
}

#' Check if an argument is missing, NULL, or of length 0.
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
#' @examples catln(1, ". This is the first line, and",, 
#' 2, ". this is the second line.")
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
        c(. + nchar(.split))

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

#' Create a vector with trailing arguments.
#'
#' @param ... Elements.
#'
#' @returns A vector
#' @export
#'
#' @examples
#' Vec(1, 2, 3,)
#' c(1, 2, 3,)
Vec <- function(...) {
    unlist(rlang::list2(...))
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
    expr <- substitute(expr)
    expr <- substituteDirect(expr, parent.frame())
    within.list(list(), eval(expr))
}


#' Overwrites default behavior of sample to avoid silent errors.
#' @description
#' When x is a length 1 integer, it won't use sample.int. See
#' the base definition of sample.
#' @export
#' @seealso [base::sample()]
sample <- function (x, size, replace = FALSE, prob = NULL) {
    if (missing(size))
        size <- length(x)
    x[sample.int(length(x), size, replace, prob)]
}

#' Keep even or odd indexes of a vector.
#' @export
evens <- function(x)  x[seq(2L, length(x), 2L)]

#' @export
#' @rdname evens
odds <- function(x)  x[seq(1L, length(x), 2L)]


#' Wrap a numeric vector between a minimum and a maximum,
#' as understood in Blender
#'
#' @description
#' Simply calculates  \code{min + (x - min) \%\% (max - min)}
#'
#' @param x Numbers to wrap.
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


#' Map a numeric vector from one range to another
#'
#'
#' @param x Numbers to map.
#' @param from Range to map from.
#' @param to Range to map to.
#' @param clamp Whether to force numbers within the range.
#'
#' @return A vector of the same length, with numbers
#' @export
#'
#' @examples
#' map_range((1:10)^2, to = 2:8) %>% barplot()
#' map_range((1:10)^2, from = 0:50, to = 2:8, clamp = TRUE) %>% barplot()
map_range <- function(x, from = range(x), to = 0:1, clamp = FALSE) {

    if (length(from) == 2L  &&  length(to) == 2L) {
        norm_x <- (x - from[1L]) / (from[2L] - from[1L])
        y <- norm_x * (to[2L] - to[1L]) + to[1L]

    } else {
        norm_x <- (x - min(from)) / (max(from) - min(from))
        y <- norm_x * (max(to) - min(to)) + min(to)
    }

    if (clamp) {
        y <- pmax(y, min(to))
        y <- pmin(y, max(to))
    }

    y
}


#' Perform set operations elegantly.
#' @description
#' Use \code{|} for set unions, \code{&} for intersection, and \code{~} for
#' difference.
#' 
#' @param expr Expression
#'
#' @return A set.
#' @export
#'
#' @examples
#' A <- 1:10
#' B <- 5:12
#' C <- 2:3
#' set(A | B ~ C)
set <- function(expr) {
    `&` <- `&&` <- intersect
    `|` <- `||` <- union
    `~` <- setdiff

    expr2 <- substitute(expr)
    expr3 <- substituteDirect(expr2, parent.frame())
    eval(expr3)
}


#' Array indices to single indices.
#' @description
#' The inverse of \link{arrayInd}
#' 
#' @param ind Matrix with n columns, where n is the number of dimensions.
#' @param arr Optional. Array to use. Only extracts dimensions.
#' @param .dim Dimensions of the array.
#'
#' @return Integer vector with the single indices for the array.
#' @export
#'
#' @examples
#' si <- c(1, 4, 7, 16)
#' my_ind <- arrayInd(si, .dim = c(4, 6))
#' single_ind(my_ind, .dim = c(4, 6))
single_ind <- function(ind, arr, .dim = dim(arr)) {
    
    stopifnot(ncol(ind) == length(.dim))
    
    ind_array <- array(1:prod(.dim), dim = .dim)
    apply(ind, 1L, \(x) { 
        do.call(what = "[", args = append(list(ind_array), x))
    })
}

#' go_from_this To This
#' @export
format_title <- function(x, sep = "_") {
    require(stringr)
    str_replace_all(x, pattern = sep, replacement = " ") %>% str_to_title()
}

#' Cumulative Sums, Products, and Extremes for vectors with NA values.
#' @rdname cumsum_na
#' @description
#' Makes it so NA values are neutral.
#' 
#' @param x Numeric vector.
#' @param remove Whether to remove NA values altogether. 
#' The resulting vector will be of lesser or equal length to x.
#' @param replace Whether to replace original NA values in the original vector
#' with NA in the resulting vector.
#'
#' @return Numeric vector.
#' @export
#'
#' @examples
#' x <- rpois(10, 5)
#' x[c(4, 5, 8)] <- NA
#' cumsum_na(x)
#' cumprod_na(x)
#' cummax_na(x)
#' cummin_na(x)
cumsum_na <- function(x, remove = FALSE, replace = TRUE) {
    .cum_ops(x, fun = cumsum, neutral = 0, remove, replace)
}
#' @rdname cumsum_na
#' @export
cumprod_na <- function(x, remove = FALSE, replace = TRUE) {
    .cum_ops(x, fun = cumprod, neutral = 1, remove, replace)
}
#' @rdname cumsum_na
#' @export
cummax_na <- function(x, remove = FALSE, replace = TRUE) {
    .cum_ops(x, fun = cummax, neutral = -Inf, remove, replace)
}
#' @rdname cumsum_na
#' @export
cummin_na <- function(x, remove = FALSE, replace = TRUE) {
    .cum_ops(x, fun = cummin, neutral = +Inf, remove, replace)
}

.cum_ops <- function(x, fun, neutral, remove, replace) {
    nas <- is.na(x)
    if (remove)
        return(fun(x[!nas]))
    x[nas] <- neutral
    y <- fun(x)
    if (replace)
        is.na(y[nas]) <- TRUE
    return(y)
}


#' Get the levels of a factor as a factor.
#'
#' @export
levels_f <- function(x) {
    factor(levels(x), levels = levels(x))
}

#' Number of unique values.
#' 
#' If `include_na = FALSE`, always gives the same result as 
#' `length(unique(x))`, but much faster. 
#'
#' @param x Array or factor.
#' @param include_na Logical, whether to count missing values as a different unique value.
#' By default it is `TRUE` so that it gives the same result as `length(unique(x))`.
#' If `FALSE`, it is equivalent to `length(unique(x[!is.na(x)])` but much faster.
#'
#' @returns Integer indicating the number of unique values.
#' @export
#'
#' @examples
#' x <- rpois(100, 3.5)
#' length_unique(x)
#' # length(unique(x))
#' 
#' x[1:3] <- NA
#' length_unique(x)
#' # length(unique(x))
#' length_unique(x, include_na = FALSE)
#' # length(unique(x[!is.na(x)])
length_unique <- function(x, include_na = TRUE) {
    if (is.factor(x)) {
        if (include_na) {
            length(levels(x)) + anyNA(x)
        } else {
            length(levels(x))
        }
    } else {
        if (include_na) {
            length(x) - sum(duplicated(x))
        } else {
            length(x) - sum(duplicated(x)) - anyNA(x)
        }
    }
}

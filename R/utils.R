
#' Change a function's default parameters
#' 
#' @param fun A function,
#' @param ... Named values, accepts injection.
#' @param envir Where to start looking the old function with the
#' default default parameters.
#'
#' @details If you overwrite a function, you won't be able to retrieve the old
#' defaults. You can save them before overwriting by using \link{formals}
#'
#' @return The modified function.
#' @export
#'
#' @examples
#' use_names <- FALSE
#' diag <- set_defaults(diag, nrow = 3, ncol = nrow / 2, names = !!use_names)
#' old_diag <- reset_defaults(diag)
#' 
#' @import rlang
#' @rawNamespace import(magrittr, except = set_names) 
set_defaults <- function(fun, ..., reassign = TRUE, envir = globalenv(),
                         warn = TRUE) {

    require(rlang)
    fun_name <- as.character(enexpr(fun))
    fun <- match.fun(fun)
    dots <- enexprs(...)
    nams <- names(dots)
    old_formals <- formals(fun)

    if (any(is.null(nams))) {
        stop("New default values must be named with their respective arguments.")
    }

    for (k in 1:length(dots)) {
        if (warn && !(nams[k] %in% names(old_formals))) {
            warning("{", nams[k],
                    "} is not a formal argument of the function {",
                    fun_name, "}")
        }
        formals(fun)[[nams[k]]] <- dots[[k]]
    }

    if (attr(fun, "old_formals") %>% is.null()) {
        attr(fun, "old_formals") <- old_formals
    }

    if (reassign) {
        assign(fun_name, value = fun, envir = envir)
    }

    invisible(fun)
}

#' @rdname set_defaults
#' @export
reset_defaults <- function(fun, envir = parent.env(globalenv()),
                           reassign = TRUE, reassign_envir = globalenv()) {

    fun_name <- as.character(substitute(fun))
    if (exists(fun_name, envir = parent.frame(), inherits = FALSE)
        && exists(fun_name, envir = envir)) {
        rm(list = fun_name, envir = parent.frame())
        return(invisible(fun))

    } else if (!is.null(attr(fun, "old_formals"))) {
        formals(fun) <- attr(fun, "old_formals")
        attr(fun, "old_formals") <- NULL

    } else {
        fun_call <- call("match.fun", fun_name)
        fun <- eval(fun_call, envir = envir)
    }

    if (reassign) {
        assign(fun_name, value = fun, envir = reassign_envir)
    }

    invisible(fun)
}

#' Save a plot as a png.
#'
#' @param p Either a ggplot object or an expression that plots something.
#' @param filename The name of the file where the plot will be saved.
#' @param asp Aspect ratio of the plot.
#' @param quality Quality of the image. Does not affect size of elements relative
#' to the plot size.
#' @param visual_size Visual size of plot elements like text, line width...
#' @param plot_it Whether to plot it in the active graphical device after connection
#' with .png file is closed.
#' @param gg TRUE if 'p' is a ggplot object, FALSE if it's an expression.
#'
#' @export
write_png <- function(p, filename, asp = 16/9, quality = 2.5, visual_size = 1,
                      plot_it = TRUE, gg = TRUE) {

    if (stringr::str_detect(filename, "\\.png$", negate = TRUE)) {
        filename <- paste0(filename, ".png")
    }

    png(filename = filename,
        height = 480 * quality, width = 480 * quality * asp,
        res = 72 * quality * visual_size)

    if (gg) {
        plot(p)
    } else {
        eval(enexpr(p))
    }

    dev.off()

    if (plot_it) {
        if (gg) {
            plot(p)
        } else {
            eval(enexpr(p))
        }
    }
}

#' Replace matching values with respective counterparts.
#'
#' @param x The vector to be translated.
#' @param old Old values to be replaced.
#' @param new Replacement for the old values.
#'
#' @return The translated vector.
#' @export
translate <- function(x, old, new) {
    matches <- match(x, old)
    for (k in which(!is.na(matches))) {
        x[k] <- new[matches[k]]
    }
    x
}


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

    require(stringr)
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



#' Replicate an Array
#'
#' @description Stack an array or dataframe by replicating it through
#' one or more margins.
#' @param x Array object.
#' @param margin Margin to stack by. 1 means rows, 2 means columns. The order of
#' the margin matters because it is done iteratively.
#'
#' @return Object of the same class as x.
#' @export
rep_array <- function(x, margin = 1, times = 1, each = 1) {

    for (m in margin) {
        args <- rep(list(substitute()), length(dim(x)))
        args[[m]] <- rep(1:(dim(x)[m]), times = times, each = each)

        x <- do.call("[", c(list(x), args))
    }

    x
}

#' Reverse an array by one or more margins.
#'
#' @param x Array object.
#' @param margin Margin to reverse by. 1 means rows, 2 means columns. The order of
#' the margin matters because it is done iteratively.
#'
#' @return Object of the same class as x.
#' @export
rev_array <- function(x, margin = 1) {

    for (m in margin) {
        args <- rep(list(substitute()), length(dim(x)))
        args[[m]] <- dim(x)[m]:1

        x <- do.call("[", c(list(x), args))
    }

    x
}

#' Convinient matrix definition.
#' @description
#' Supports ',,' for row breaks.
#' @seealso \link[tibble]{frame_matrix}
matrix_byrow <- function(..., .fill, .nrow, .ncol) {

    require(rlang)
    dots <- enexprs(...)

    row_list <- list(NULL)
    r <- 1

    for (d in dots) {
        if (missing(d)) {
            r <- r + 1
            row_list[r] <- list(NULL)
        } else {
            row_list[[r]] %<>% append(d)
        }
    }

    row_list[sapply(row_list, is.null)] <- NULL

    if (len0(.fill)) {
        .fill <- do.call(c, row_list) %>%
            class() %>%
            vector(1)
    }
    if (len0(.nrow)) {
        .nrow <- length(row_list)
    }
    if (len0(.ncol)) {
        .ncol <- max(sapply(row_list, length))
    }

    my_matrix <- matrix(data = .fill, nrow = .nrow, ncol = .ncol)
    for (i in 1:.nrow) {
        my_matrix[i, 1:length(row_list[[i]])] <- row_list[[i]]
    }

    my_matrix
}


#' Make a list of functions
#'
#' @rdname function_list
#' @param ... Calls.
#' @param .args Arguments shared by all functions.
#'
#' @return List of functions.
#' @export
#' @seealso [list_to_function()]
#'
#' @examples
#' my_functions <- function_list(
#'     .args = c("x", "y"),
#'     x + mean(y) / z,
#'     x^2 + sd(y) / z^2,
#'     sqrt(x) + y + z
#' )
#'
#' # Is equivalent to
#' my_calls <- alist(
#'     x + mean(y) / z,
#'     x^2 + sd(y) / z^2,
#'     sqrt(x) + y + z
#' )
#' my_functions <- calls_to_functions(my_calls, c("x", "y"))
#'
#' # Is equivalent to
#' my_functions <- list(
#'     \(x, y) x + mean(y) / z,
#'     \(x, y) x^2 + sd(y) / z^2,
#'     \(x, y) sqrt(x) + y + z
#' )
function_list <- function(..., .args) {
    require(rlang)
    exprs <- enexprs(...)
    calls_to_functions(exprs, maybe_missing(.args))
}

#' @rdname function_list
#' @param exprs List of calls.
#' @param .args Arguments shared by all functions.
#' @export
calls_to_functions <- function(exprs, .args) {

    if (missing(.args))
        .args <- all.vars(exprs)

    .args <- rep(alist(name = ), length(.args)) %>%
        set_names(.args)

    fun_list <- vector("list", length(exprs))

    for (k in seq_along(exprs)) {
        function_form <- append(.args, exprs[[k]])
        fun_list[[k]] <- as.function(function_form)
    }

    names(fun_list) <- names(exprs)
    fun_list
}


#' Convert a list of functions to a single function.
#' @description
#' Useful for writing clean code.
#' @param fun_list A list of functions.
#' @return A single function that applies all the functions in the list to
#' it's argument \code{x}.
#' @export
#' @seealso [function_list()]
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


#' Negate values to make a pyramid plot.
#'
#' @param .data Data frame or tibble.
#' @param values Columns with the values to negate.
#' @param by Column, preferably a factor, to negate values by.
#' @param level Level in the factor to have it's values negated.
#'
#' @return An object of the same class as .data 
#' @export
pyramid <- function(.data, values, by, level = fact[1L]) {
    
    require(rlang)
    require(tidyselect)
    
    value_cols <- eval_select(enquo(values), data = .data)
    by_col <- eval_select(enquo(by), data = .data)
    
    fact <- .data[[ by_col ]]
    neg <- fact == level
    
    for (v in value_cols) {
        if (!is.numeric(.data[[ v ]])) {
            warning("Columns in 'values' must be numeric")
            next
        }
        .data[neg, v] <- -.data[neg, v]
    }
    
    .data
}

#' Round a vector such that their rounded sum stays intact.
#' @description
#' Useful for percentatges.
#' 
#' @param x Numeric Vector
#' @param digits Number of decimal places. To mak
#'
#' @return Rounded vector with attributes:
#' Threshold: indicating at which point numbers are rounded to their ceiling.
#' Relative Threshold: the above in relation to the digits, to be compared with 0.5.
#' Bias: mean difference between the result and the default rounding with the
#' same digits.
#' 
#' @export
#' @examples
#' x <- rexp(20)
#' x <- 100 * x / sum(x)
#' x
#' round_sum(x, 2)
#' sum(round_sum(x, 2))
round_sum <- function(x, digits = 0) {
    target <- round(sum(x), digits = digits)
    inc <- 10^-digits
    floor_x <- (x %/% inc) * inc
    differential <- (target - sum(floor_x)) / inc
    decimal_part <- x - floor_x
    nth <- -sort(-decimal_part, partial = differential)[differential]
    to_ceil <- which(decimal_part >= nth)
    
    y <- floor_x
    y[to_ceil] <- y[to_ceil] + inc
    structure(y, threshold = decimal_part[differential], 
              relative_threshold = decimal_part[differential] / inc,
              bias = mean(y) - mean(round(x, digits = digits)))
}

#' Distribute an Amount of something Between something.
#' @param amount Numeric, target sum of the values.
#' @param between Integer, number of values.
#' @param dist Optionally, an initial distribution for how the values are distributed.
#' @param digits Digits of the values.
#' @return Numeric vector of length \code{between}, 
#' the values of which add up to \code{amount}.
#' @export
distribute <- function(amount, between, dist = runif(between), digits = 0) {
    values <- dist * amount / sum(dist)
    round_sum(values, digits)
}

#' Generate the first combinations of n elements, taken m at a time
#' @description
#' Slight modification of \code{\link[utils]{combn}} to stop after
#' a set number of combinations.
#' 
#' @param x vector source for combinations, or integer n for x <- seq_len(n).
#' @param m number of elements to choose.
#' @param first maximum number of combinations to return.
#' @param warn logical, whether to warn if the actual number of combinations
#' does not reach the maximum.
#' @param FUN function to be applied to each combination. 
#' @param simplify logical indicating if the result should be simplified 
#' to an array.
#' @param ... optionally, further arguments to \code{FUN}.
#'
#' @seealso [utils::combn()]
#' @export
#' @examples
#' combn_first(10, 4, first = 5)
combn_first <- function(x, m, first, warn = FALSE, 
                        FUN = NULL, simplify = TRUE, ...) {
    stopifnot(length(m) == 1L, is.numeric(m), length(first) == 1L, is.numeric(first))
    if (m < 0) 
        stop("m < 0", domain = NA)
    if (is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == x) 
        x <- seq_len(x)
    n <- length(x)
    if (n < m) 
        stop("n < m", domain = NA)
    x0 <- x
    if (simplify) {
        if (is.factor(x)) 
            x <- as.integer(x)
    }
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- seq_len(m)
    nofun <- is.null(FUN)
    if (!nofun && !is.function(FUN)) 
        stop("'FUN' must be a function or NULL")
    len.r <- length(r <- if (nofun) x[a] else FUN(x[a], ...))
    count <- as.integer(round(choose(n, m)))
    count <- min(count, first)
    if (simplify) {
        dim.use <- if (nofun) 
            c(m, count)
        else {
            d <- dim(r)
            if (length(d) > 1L) 
                c(d, count)
            else if (len.r != 1L) 
                c(len.r, count)
            else c(d, count)
        }
    }
    if (simplify) 
        out <- matrix(r, nrow = len.r, ncol = count)
    else {
        out <- vector("list", count)
        out[[1L]] <- r
    }
    if (m > 0) {
        i <- 2L
        nmmp1 <- n - m + 1L
        while (a[1L] != nmmp1 && i <= first) {
            if (e < n - h) {
                h <- 1L
                e <- a[m]
                j <- 1L
            }
            else {
                e <- a[m - h]
                h <- h + 1L
                j <- 1L:h
            }
            a[m - h + j] <- e + j
            r <- if (nofun) 
                x[a]
            else FUN(x[a], ...)
            if (simplify) 
                out[, i] <- r
            else out[[i]] <- r
            i <- i + 1L
        }
    }
    if (simplify) {
        if (is.factor(x0)) {
            levels(out) <- levels(x0)
            class(out) <- class(x0)
        }
        dim(out) <- dim.use
    }
    if (warn && count < first) {
        warning("Number of combinations is lower than ", first)
    }
    out
}

combn_random <- function(x, m, amount, warn = FALSE, simplify = TRUE, ...) {
    stopifnot(is_integerish(m, n = 1L), is_integerish(amount, n = 1L))
    if (m < 0) 
        stop("m < 0", domain = NA)
    if (is_integerish(x, n = 1L)) 
        x <- seq_len(x)
    n <- length(x)
    if (n < m) 
        stop("n < m", domain = NA)
    
    
}

combn_recurse <- function(x, m) {
    
    n <- length(x)
    if (n == 1L || m == 1L) return(t(x))
    
    mat <- matrix(nrow = m, ncol = choose(n, m))
    k <- 1L
    j <- 1L
    
    while (j <= ncol(mat)) {
        recurse <- combn_recurse(x[-(1:k)], m-1L)
        l <- j + ncol(recurse) - 1L
        mat[1, j:l] <- x[k]
        mat[2:nrow(mat), j:l] <- recurse
        j <- l + 1L
        k <- k + 1L
    }
    
    return(mat)
}

#' Split a for loop into expressions, with the looper variable replaced
#' with each value.
#' 
#' @description
#' Useful to split a single expression into multiple, without evaluating anything
#' but the sequence.
#' 
#' @usage for_split(expr, recursive = TRUE, envir = parent.frame())
#' for (variable in sequence) expression
#' @param expr Call including all the for loop. 
#' @param recursive Whether to split for loops inside for loops.
#' @param envir Environment where to evaluate the sequence.
#'
#' @return A list of expressions. If the sequence is named, the result copies the names.
#' If \code{recursive = TRUE}, it will be lists inside of lists. 
#' @export
#'
#' @examples
#' my_loop <- quote(
#'     for(i in 1:3) for(j in 1:2)
#'         a <- a + x[i, j]
#' )
#' fsplit <- for_split(my_loop)
#' fsplit
#' unlist(fsplit)
for_split <- function(expr, recursive = TRUE, envir = parent.frame()) {
    
    if (format(expr[[1L]]) %in% c("(", "{"))
        expr <- expr[[2L]]
    
    if (recursive && expr[[1L]] != quote(`for`)) 
        return(expr)
    
    sequence <- expr[[3L]] |> eval(envir = envir)
    interior <- expr[[4L]]
    
    looper_env <- list(NA)
    names(looper_env) <- expr[[2L]] |> format()
    result <- list()
    
    for (k in seq_along(sequence)) {
        looper_env[[1L]] <- unname(sequence[k])
        result[[k]] <- substituteDirect(interior, frame = looper_env)
    }
    
    if (recursive)
        result <- lapply(result, for_split, recursive = TRUE, envir = envir)
    
    names(result) <- names(sequence)
    structure(result, variable = names(looper_env), sequence = sequence)
}



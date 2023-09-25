
library(magrittr)

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
        "left" = function(x) {
            x[, 2]
        },
        "right" = function(x) {
            x[, 1] - 1
        },
        "middle" = function(x) {
            x[, 1] %<>% subtract(1)
            c(t(x))
        },
        stop("{where} must be 'left','right' or middle")
    )
    pos <- str_locate_all(string, pattern) %>% lapply(fix)

    mapply(string, pos, SIMPLIFY = FALSE, FUN = function(str, p) {
        pstart <- c(1, p + 1)
        pend   <- c(p, nchar(str))
        str_sub(str, pstart, pend)
    })
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

#' Expand Grid with repeating support.
#'
#' @param ... Vectors or lists. Use "x \%^\% n" to have "n" columns for the vector "x"
#' @export
#'
#' @seealso See \link{expand.grid}, the base version of the function, in which this
#' one relies on.
#'
#' @examples expand_grid(1:3 %^% 2, letters[1:2])
expand_grid <- function(..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE) {

    pf <- parent.frame()
    args <- as.list(substitute(...()))
    new_args <- list()

    `%^%` <- function(x, times) {
        y <- substitute(x) %>% eval(envir = pf)
        rep(list(y), times = times)
    }

    mapply(args, names(args), FUN = function(a, name) {
        b <- eval(a)
        if (!is.list(b))
            b <- list(b)

        names(b) <- if (length(b) == 1) {
            name
        } else {
            paste0(name, "_", 1:length(b))
        }
        new_args <<- append(new_args, b)
    })

    expand.grid(new_args,
                KEEP.OUT.ATTRS = KEEP.OUT.ATTRS,
                stringsAsFactors = stringsAsFactors)
}

#' Convinient matrix definition.
#' @description
#' Supports ',,' for row breaks.
#'
#' @export
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







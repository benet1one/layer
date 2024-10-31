
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


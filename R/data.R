
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
#' @seealso [tibble::frame_matrix()]
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


#' Keep dimensions while indexing an object.
#' @rdname keep_dim
#' @description
#' By default, R will drop the dimensions of an array when a single row or 
#' column is indexed. \code{x[i, j, ..., drop = TRUE]}.
#' Objects of class "keep_dim" will override this behaviour, so you don't have
#' to add \code{drop = FALSE} at every command.
#' 
#' @param x Array.
#'
#' @return The same array, with class "keep_dim"
#' @export
#'
#' @examples
#' a <- matrix(1:16, nrow = 4)
#' a[1, ]
#' a[, 1]
#' 
#' a <- keep_dim(a)
#' a[1, ]
#' a[, 1]
#' a[, 1, drop = TRUE]
keep_dim <- function(x) {
    class(x) <- c("keep_dim", class(x))
    x
}

#' @export
`[.keep_dim` <- function(x, ..., drop = FALSE) {
    y <- `[`(unclass(x), ..., drop = drop)
    class(y) <- c("keep_dim", class(y))
    y
}

#' @rdname keep_dim
#' @description Use \code{unkeep_dim} to remove "keep_dim" class from an object.
#' @export
unkeep_dim <- function(x) {
    class(x) <- setdiff(class(x), "keep_dim")
    x
}
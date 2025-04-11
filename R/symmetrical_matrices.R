
symmetrical_matrix <- function(mat = matrix()) {
    stopifnot(nrow(mat) == ncol(mat))
    y <- vector(mode(mat), length = sum(1:nrow(mat)))
    class(y) <- "SymmetricalMatrix"
    
    for (j in 1:ncol(mat))
        for (i in 1:j)
            y[i,j] <- mat[i,j]
    
    y
}

`[.SymmetricalMatrix` <- function(x, ...) {
    browser()
    stopifnot(is_integerish(i, n = 1L), i > 0,
              is_integerish(j, n = 1L), j > 0)
    if (i > j)
        x[sum(0:(i-1)) + j, ...]
    else
        x[sum(0:(j-1)) + i, ...]
}

`[<-.SymmetricalMatrix` <- function(x, i, j, value) {
    
}
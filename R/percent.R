
#' Object of class 'percent'
#' @description
#' Usefull for probabilities
#' 
#' @param x Numeric vector or array
#' @export
#'
#' @examples
#' m <- matrix(runif(36), 6, 6) %>% percent()
#' print(m)
#' format(m, digits = 2L)
percent <- function(x) {
    stopifnot(is.numeric(x) || is.logical(x))
    class(x) <- c("percent", class(x))
    x
}

#' @exportS3Method
format.percent <- function(x, digits = 2L, spacing = 0L) {
    y <- round(100*x, digits = digits) %>% 
        format.default(nsmall = digits) %>% 
        paste0("%", strrep(" ", spacing))
    
    if (length(dim(x)) > 0)
        array(y, dim(x))
    else
        y
}

#' @exportS3Method
print.percent <- function(x, digits = 2L, spacing = 1L) {
    format(x, digits = digits, spacing = spacing) %>% print(quote = FALSE)
}
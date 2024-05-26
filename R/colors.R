
Colors <- Colours <- function(...) {
    
    require(stringr)
    
    col <- trimws(c(...))
    hex <- str_detect(col, "#")
    css <- col %in% colors()

    col[!hex & !css] <- paste0("#", col[!hex & !css])
    len <- nchar(col[!css])
    
    if (!all(len %in% c(4L, 7L, 9L))) {
        stop("Colors must either either be in colors() or in #HEX format")
    }
    structure(col, class = "color")
}

print.color <- function(col) {
    attributes(col) <- NULL
    cat(deparse1(col[], collapse = "\n"))
}

plot.color <- function(col) {
    barplot.default(
        height = rep(1, length(col)),
        names.arg = color,
        col = col,
        space = 0,
        border = 0,
    )
}
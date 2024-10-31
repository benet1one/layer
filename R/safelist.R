
safelist <- function(...) {
    list2(...) |> structure(class = "safelist")
}

`[[.safelist` <- function(x, ind) {
    out <- unclass(x)[[ind]]
    if (is.null(out))
        stop('Safelist$', ind, ' is null.')
    out
}
`[[<-.safelist` <- function(x, ind, value) {
    current <- try(x[[ind]])
    if (inherits(current, "try-error"))
        stop("Attemped assignment on null binding: Safelist$", ind)
    if (mode(x[[ind]]) != mode(value)) 
        stop("Safelist$", ind, "is mode ", mode(x[[ind]]),
             " and the value to assign is mode ", mode(value))
    y <- unclass(x)
    y[[ind]] <- value
    structure(y, class = "safelist")
}

`$.safelist` <- function(x, ind) {
    x[[ind]]
}
`$<-.safelist` <- function(x, ind, value) {
    x[[ind]] <- value
    x
}

sl <- safelist(a = 2)


safe_c <- function(..., .mode) {
    len <- ...length()
    if (len == 0L) {
        if (missing(.mode))
            stop("Must specify .mode if the vector has no elements.")
        out <- vector(.mode)
        return(structure(out, class = c("safe_vec", class(out))))
    }
    
    if (missing(.mode))
        .mode <- storage.mode(..1)

    for (d in seq_len(len)) {
        .mode_d <- storage.mode(...elt(d))
        if (.mode_d != .mode) {
            dname <- ...names()[d] %or% (d)
            message <- glue::glue("First element is storage.mode '{.mode}', ",
                                  "While element '{dname}' is storage.mode '{.mode_d}'")
            stop(message)
        }
    }

    structure(c(...), class = c("safe_vec", class(..1)))
}



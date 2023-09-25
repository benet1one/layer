
indexed <- function(x, start = 0L, step = 1L) {
    set_attributes(x, list(ind_start = start, ind_step = step,
                           class = c("indexed", class(x))))
}

unindex <- function(x) {
    set_class(x, class(x) %>% setdiff("indexed")) %>%
        set_attributes(list(ind_start = NULL, ind_step = NULL))
}

`[[.indexed` <- function(x, i) {
    i <- ((i - attr(x, "ind_start")) / attr(x, "ind_step")) + 1L
    if (any(i < 1L))
        stop("Indexed vectors don't support negative extractions.")
    unclass(x) %>% extract(i) %>% set_attributes(attributes(x))
}

seq.indexed <- function(x, length.out = length(x)) {
    attr(x, "ind_start") + attr(x, "ind_step") * 0:(length.out - 1L)
}

# print.indexed <- function(x) {
#     a
# }


pychar <- function(x) {
    if (typeof(x) != "character") stop("Must be character type.")
    set_class(x, c("pychar", class(x)))
}

`[[.pychar` <- function(x, vec_ind, char_ind, .collapse = TRUE) {

    if (missing(vec_ind))
        vec_ind <- seq_along(x)
    if (missing(char_ind))
        return(x[vec_ind])

    sapply(x[vec_ind], USE.NAMES = FALSE, function(y) {
        substring(y, char_ind, char_ind) %>%
            pipe_if(.collapse, paste(., collapse = ""))
    })
}






require(purrr)
require(cli)

super_object <- function(value = NULL, fun = \(n = 6) head(value, n), ...) {
    env <- as.environment(list(value = value, ...))
    parent.env(env) <- parent.env(environment())
    environment(fun) <- env
    
    y <- structure(fun, env = env)
    class(y) <- c("super_object", class(y))
    y
}
super <- super_object

super_fun <- function(x) {
    attributes(x) <- NULL
    x
}
`super_fun<-` <- function(x, value) {
    fun <- value
    env <- super_env(x)
    environment(fun) <- env
    
    y <- structure(fun, env = env)
    class(y) <- c("super_object", class(y))
    y
}
super_env <- function(x) {
    attr(x, "env")
}
`super_env<-` <- function(x, value) {
    env <- super_env(x)
    env <- value
    x
}

super_value <- function(x) {
    super_env(x) $ value
}
`super_value<-` <- function(x, value) {
    env <- super_env(x)
    env$value <- value
    x
}

`[.super_object` <- function(x, ...) {
    `[`(super_value(x), ...)
}
`[<-.super_object` <- function(x, ..., value) {
    new_val <- `[<-`(super_value(x), ..., value)
    super_value(x) <- new_val
    x
}
`$.super_object` <- function(x, name) {
    if (name == quote(fun))
        super_fun(x)    
    else
        getElement(super_env(x), name)
}
`$<-.super_object` <- function(x, name, value) {
    if (name == quote(fun)) {
        super_fun(x) <- value
        x
    } else {
        sym <- quote(super_env(x)$name <- value)
        sym <- substituteDirect(sym, list(name = name))
        eval(sym)
        x
    }
}

Math.super_object <- function(x, ...) {
    .Generic(x$value, ...)
}

print.super_object <- function(x) {
    cat_line("<< SUPER OBJECT >>", col = "cyan")
    cat_line("-- VALUE --", col = "yellow")
    print(super_value(x))
    cat_line("-- FUNCTION --", col = "yellow")
    print(super_fun(x))
    
    env_list <- super_env(x) |> as.list()
    env_list$value <- NULL
    if (length(env_list) > 0L) {
        cat_line("-- ENVIRONMENT --", col = "yellow")
        print(env_list)
    }
}

x <- super_object(letters, num = 5)


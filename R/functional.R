
# Function lists ---------------------------------

#' Make a list of functions
#'
#' @rdname function_list
#' @param ... Calls.
#' @param .args Arguments shared by all functions.
#'
#' @return List of functions.
#' @export
#' @seealso [list_to_function()]
#'
#' @examples
#' my_functions <- function_list(
#'     .args = c("x", "y"),
#'     x + mean(y) / z,
#'     x^2 + sd(y) / z^2,
#'     sqrt(x) + y + z
#' )
#'
#' # Is equivalent to
#' my_calls <- alist(
#'     x + mean(y) / z,
#'     x^2 + sd(y) / z^2,
#'     sqrt(x) + y + z
#' )
#' my_functions <- calls_to_functions(my_calls, c("x", "y"))
#'
#' # Is equivalent to
#' my_functions <- list(
#'     \(x, y) x + mean(y) / z,
#'     \(x, y) x^2 + sd(y) / z^2,
#'     \(x, y) sqrt(x) + y + z
#' )
function_list <- function(..., .args) {
    require(rlang)
    exprs <- enexprs(...)
    calls_to_functions(exprs, maybe_missing(.args))
}

#' @rdname function_list
#' @param exprs List of calls.
#' @param .args Arguments shared by all functions.
#' @export
calls_to_functions <- function(exprs, .args) {
    
    if (missing(.args))
        .args <- all.vars(exprs)
    
    .args <- rep(alist(name = ), length(.args)) %>%
        set_names(.args)
    
    fun_list <- vector("list", length(exprs))
    
    for (k in seq_along(exprs)) {
        function_form <- append(.args, exprs[[k]])
        fun_list[[k]] <- as.function(function_form)
    }
    
    names(fun_list) <- names(exprs)
    fun_list
}


#' Convert a list of functions to a single function.
#' @description
#' Useful for writing clean code.
#' @param fun_list A list of functions.
#' @return A single function that applies all the functions in the list to
#' it's argument \code{x}.
#' @export
#' @seealso [function_list()]
#'
#' @examples
#' funs <- list(
#'     f1 = \(x) x + 1,
#'     f2 = \(x) x*4,
#'     f3 = \(x) x^2
#' )
#'
#' my_fun <- list_to_function(funs)
#' my_fun(1:5)
list_to_function <- function(fun_list, simplify = TRUE) {
    
    if (missing(fun_list))
        stop("Missing fun_list")
    
    function(x) {
        sapply(fun_list, \(f) do.call(f, list(x)), simplify = simplify)
    }
}


# Misc --------------------------------

#' Get the expression inside an enclosure, e.g. (expr) or {expr}.
#' @param expr Quoted expression.
#' @export
inside <- function(expr) {
    if (is_symbol(expr)) return(expr)
    stopifnot(is_expression(expr))
    
    enclosure <- expr[[1L]]
    if (enclosure == quote(`(`))
        return(expr[[2]])
    if (enclosure == quote(`{`))
        if (length(expr) > 2)
            stop("Expression enclosed by `{` has more than one line.")
    else return(expr[[2]])
    
    expr
}

#' Reorder function arguments.
#' @param f Function with multiple arguments.
#' @param variables Arguments to put first and have their default values removed.
change_variable <- function(f, variables) {

    if (!is_character(variables))
        stop("'variables' must be a character vector.")
    for (v in variables)
        if (!is.element(v, formalArgs(f)))
            stop("Variable '", v, "' not an argument in function.")
    
    parameters <- formals(f)[names(formals(f)) %!in% variables]
    
    new_function(
        args = rep(list(substitute()), length(variables)) |> 
            set_names(variables) |> 
            append(parameters),
        body = body(f)
    )
}

#' Split a for loop into expressions, with the looper variable replaced
#' with each value.
#' 
#' @description
#' Useful to split a single expression into multiple, without evaluating anything
#' but the sequence.
#' 
#' @usage for_split(expr, recursive = TRUE, envir = parent.frame())
#' for (variable in sequence) expression
#' @param expr Call including all the for loop. 
#' @param recursive Whether to split for loops inside for loops.
#' @param envir Environment where to evaluate the sequence.
#'
#' @return A list of expressions. If the sequence is named, the result copies the names.
#' If \code{recursive = TRUE}, it will be lists inside of lists. 
#' @export
#'
#' @examples
#' my_loop <- quote(
#'     for(i in 1:3) for(j in 1:2)
#'         a <- a + x[i, j]
#' )
#' fsplit <- for_split(my_loop)
#' fsplit
#' unlist(fsplit)
for_split <- function(expr, recursive = TRUE, envir = parent.frame()) {
    
    expr <- inside(expr)
    
    if (recursive && expr[[1L]] != quote(`for`)) 
        return(expr)
    
    sequence <- expr[[3L]] |> eval(envir = envir)
    interior <- expr[[4L]]
    
    looper_env <- list(NA)
    names(looper_env) <- expr[[2L]] |> format()
    result <- list()
    
    for (k in seq_along(sequence)) {
        looper_env[[1L]] <- unname(sequence[k])
        result[[k]] <- substituteDirect(interior, frame = looper_env)
    }
    
    if (recursive)
        result <- lapply(result, for_split, recursive = TRUE, envir = envir)
    
    names(result) <- names(sequence)
    structure(result, variable = names(looper_env), sequence = sequence)
}


# Math -------------------------

#' Analytical and Numerical Derivative of a Function
#'
#' @rdname derivate
#' @param f Function to derivate. For \code{derivate()}, the variable
#' must be a scalar. For \code{derivate_num()}, the variable can either be
#' a scalar or a vector. 
#' @param arg String, variable to derivate by. For derivating multiple function arguments,
#' see [gradient()].
#'
#' @return A new function which takes the same arguments as \code{f}
#' and returns a scalar for \code{derivate()}, and a scalar or vector
#' for \code{derivate_num()}.
#' @export 
#' @seealso [gradient()]
derivate <- function(f, arg = formalArgs(f)[1L]) {
    
    if (!is_string(arg))
        stop("'arg' must be a string containing the variable to derivate by.")
    if (!is.element(arg, formalArgs(f)))
        warning("arg ", arg, " is not an argument in 'f'")
    
    rlang::new_function(
        args = formals(f),
        body = body(f) |> inside() |> D(arg)
    )
}

#' @rdname derivate
#' @export
derivate_num <- function(f) {
    
    rlang::check_installed("numDeriv")
    f <- rlang::as_function(f)
    
    if (rlang::is_lambda(f)) {
        return(new_function(
            args = alist(.x=),
            body = expr({
                numDeriv::grad(f, .x)
            })
        ))
    }
    
    first_arg <- formalArgs(f)[1L]
    other_args <- formalArgs(f)[-1L]
    dots_args <- parse_exprs(other_args) |> set_names(other_args)
    
    new_function(
        args = formals(f),
        body = expr({
            numDeriv::grad(f, !!parse_expr(first_arg), !!!dots_args)
        })
    )
}

#' Analytical and Numeric Gradient of a function.
#' 
#' @rdname gradient
#' @param f Function to derivate. Each argument must take a scalar. If you wish to
#' get the gradient of a function that takes a vector, use [derivate_num()].
#' 
#' @return A new function which takes the same arguments as \code{f}
#' and returns a named numeric vector.
#' @export
#' @seealso [derivate()]
gradient <- function(f) {
    
    args <- formalArgs(f)
    bdy <- inside(body(f))
    grad <- purrr::map(args, ~ D(bdy, .x)) |> set_names(args)
    rlang::new_function(
        args = formals(f),
        body = expr({
            c(!!!grad)
        })
    )
}

#' @rdname gradient
#' @export
gradient_num <- function(f) {
    
    rlang::check_installed("numDeriv")
    args <- formalArgs(f)
    
    substitution_list <- purrr::map(args, \(a) rlang::expr(.args[!!a])) |>
        rlang::set_names(args)
    
    conversion <- rlang::parse_exprs(args) |> 
        rlang::set_names(args)
    
    f2 <- rlang::new_function(
        args = alist(.args=),
        body = substituteDirect(body(f), substitution_list)
    )
    
    rlang::new_function(
        args = formals(f),
        body = rlang::expr({
            numDeriv::grad(f2, c(!!!conversion)) |> rlang::set_names(args)
        })
    )
}

#' THIS IS VERY SLOW AND NEEDS TO BE MADE TWICE AS FAST!!!
hessian <- function(f, args = formalArgs(f)) {
    if (!is_character(args))
        stop("'args' must be a character vector containing the variables to derivate by.")
    for (arg in args)
        if (!is.element(arg, formalArgs(f)))
            warning("arg ", arg, " is not an argument in 'f'")
    
    bdy <- inside(body(f))
    grid <- expand.grid(row = args, col = args, stringsAsFactors = FALSE)
    dvec <- purrr::map2(grid$row, grid$col, ~ bdy |> D(.x) |> D(.y))
    
    new_function(
        args = formals(f),
        body = expr({
            flat_hessian <- c(!!!dvec)
            matrix(flat_hessian, nrow = length(args), dimnames = list(args, args))
        })
    )
}

#' Invert a function
#'
#' @param f Function to invert.
#' @param interval Length 2 vector containing the lower and upper bound of the result.
#' @param no_root Value to return when there is no root.
#' @param arg Argument of f to invert by.
#'
#' @return Inverse function: inv(y, ...) = {x : f(x, ...) = y}
#' @export
invert <- function(f, interval, no_root = NaN, arg = formalArgs(f)[1L]) {
    stopifnot(is.numeric(interval), 
              length(interval) == 2L,
              interval[1] < interval[2])
    
    f <- change_variable(f, arg)
    
    inv <- \(y, ...) {
        if ((f(interval[1]) - y) * (f(interval[2]) - y) > 0)
            return(no_root)
        uniroot(\(x) f(x, ...) - y, interval) $ root
    }
    Vectorize(inv, "y")
}

#' Find the value of a variable such that an equality is true
#'
#' @param variable Variable to find the value of.
#' @param equality Equality expression.
#' @param interval Interval where the value can be.
#' 
#' @export
#' @returns The value of the variable if it's in the interval, an error otherwise.
#' @examples
#' x_hat <- x |> such_that(exp(x) == 5*x^2, interval = c(1, 10))
such_that <- function(variable, equality, interval) {
    variable <- enexpr(variable)
    equality <- inside(enexpr(equality))
    
    if (equality[[1L]] != quote(`==`))
        stop("Expression must be an equality.")
    
    arg <- list(substitute())
    names(arg) <- format(variable)
    lhs <- new_function(arg, body = equality[[2L]])
    rhs <- new_function(arg, body = equality[[3L]])
    
    uniroot(\(x) lhs(x) - rhs(x), interval) $ root
}

.optim <- function(f, init, maximize, ...) {
    f2 <- rlang::as_function(f)
    formals(f2) <- exprs(
        .x = , 
        .  = .x, 
        .y = stop(".y not supported in optimisation, use a length 2 vector.")
    )
    
    f3 <- if (maximize) {
         \(x) { -f2(x) }
    } else {
        f2
    }
    
    sol <- optim(fn = f3, par = init, ...)
    list(
        solution = sol$par,
        objective_value = if (maximize) -sol$value  else sol$value
    )
}

#' Optimize a multi-variate function.
#' @rdname layer_optim
#' @export
#' @param f Function to optimise.
#' @param init Initial values for the variable.
#' @param ... Arguments passed on to  \link[stats]{optim}.
#' 
#' @return List with the \code{solution} and \code{objective_value}.
minimize <- function(f, init, ...) {
    .optim(f, init, maximize = FALSE, ...)
}

#' @rdname layer_optim
#' @export
maximize <- function(f, init, ...) {
    .optim(f, init, maximize = TRUE, ...)
}




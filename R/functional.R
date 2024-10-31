
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
            stop("Variable ", v, " not an argument in function.")
    
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

#' Analytical Derivative of a Function
#'
#' @param f Function to derivate.
#' @param arg String, variable to derivate by.
#'
#' @return A new function.
#' @export 
derivate <- function(f, arg = formalArgs(f)[1L]) {
    
    f <- rlang::as_function(f)

    if (!is_string(arg))
        stop("'arg' must be a string containing the variable to derivate by.")
    if (!is.element(arg, formalArgs(f)))
        stop("'arg' is not an argument in 'f'")
    
    rlang::new_function(
        args = formals(f),
        body = body(f) |> inside() |> D(arg)
    )
}

#' Numeric Derivative of a Function
#' @param f Function to derivate.
#' @param arg String, variable to derivate by.
#' 
#' @return A new function.
#' @export
derivate_num <- function(f, arg = formalArgs(f)[1L], ...) {
    
    require(pracma)
    f <- rlang::as_function(f)
    
    if (is_lambda(f)) 
        return(function(.x) {
            pracma::fderiv(f, .x, ...)
        })
    
    f2 <- change_variable(f, arg)
    
    new_function(
        args = list(substitute()) |> set_names(arg),
        body = expr({
            pracma::fderiv(f2, !!parse_expr(arg), ...)
        })
    )
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




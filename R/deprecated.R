
#' Change a function's default parameters
#' 
#' @param fun A function,
#' @param ... Named values, accepts injection.
#' @param envir Where to start looking the old function with the
#' default default parameters.
#'
#' @details If you overwrite a function, you won't be able to retrieve the old
#' defaults. You can save them before overwriting by using \link{formals}
#'
#' @return The modified function.
#' @export
#'
#' @examples
#' use_names <- FALSE
#' diag <- set_defaults(diag, nrow = 3, ncol = nrow / 2, names = !!use_names)
#' old_diag <- reset_defaults(diag)
set_defaults <- function(fun, ..., reassign = TRUE, envir = globalenv(),
                         warn = TRUE) {
    
    require(rlang)
    fun_name <- as.character(enexpr(fun))
    fun <- match.fun(fun)
    dots <- enexprs(...)
    nams <- names(dots)
    old_formals <- formals(fun)
    
    if (any(is.null(nams))) {
        stop("New default values must be named with their respective arguments.")
    }
    
    for (k in 1:length(dots)) {
        if (warn && !(nams[k] %in% names(old_formals))) {
            warning("{", nams[k],
                    "} is not a formal argument of the function {",
                    fun_name, "}")
        }
        formals(fun)[[nams[k]]] <- dots[[k]]
    }
    
    if (attr(fun, "old_formals") %>% is.null()) {
        attr(fun, "old_formals") <- old_formals
    }
    
    if (reassign) {
        assign(fun_name, value = fun, envir = envir)
    }
    
    invisible(fun)
}

#' @rdname set_defaults
#' @export
reset_defaults <- function(fun, envir = parent.env(globalenv()),
                           reassign = TRUE, reassign_envir = globalenv()) {
    
    fun_name <- as.character(substitute(fun))
    if (exists(fun_name, envir = parent.frame(), inherits = FALSE)
        && exists(fun_name, envir = envir)) {
        rm(list = fun_name, envir = parent.frame())
        return(invisible(fun))
        
    } else if (!is.null(attr(fun, "old_formals"))) {
        formals(fun) <- attr(fun, "old_formals")
        attr(fun, "old_formals") <- NULL
        
    } else {
        fun_call <- call("match.fun", fun_name)
        fun <- eval(fun_call, envir = envir)
    }
    
    if (reassign) {
        assign(fun_name, value = fun, envir = reassign_envir)
    }
    
    invisible(fun)
}

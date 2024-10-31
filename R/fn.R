
require(rlang)

`%new%` <- function(miss, fn) {
    enexprs(miss, fn)
}

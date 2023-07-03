#' Parameters only
#'
#' Parameters to inherit from
#' @param x (`list`, `atomic`) object to compare.
#'
#' @param y (`list`, `atomic`) obejct to compare with.
#'
#' @param .expr (`language`) any syntactically valid R expression.
#' Argument is parsed, evaluated and passed recursivelly to calls run on the nested
#' elements of `x` and `y`. Other arguments (`.name`, `.depth` and `.type`) can be used
#' to influence output of the comparisons.
#' For example `if (.depth == 2) return(x)` will return `x` for the nested list elements
#' of depth equal `2`.
#' Note: If `.expr` has a `return` statement please be aware that it should be the
#' same as the type of the output.
#'
#' @param .name (`character(1)`) Private argument, please don't specify.
#' Used to identify the name of the current list element when calld resursivelly.
#' `.name` can be used with `.expr`.
#'
#' @param .depth (`integer(1)`) Private argument, please don't specify.
#' depth of the call. Increases by `1` according to depth of the lists `x` and `y`
#'
#' @param .type (`integer(1)`) Private argument, please don't specify.
#' `.type` denotes what's the result of comparison for this element in the `parent.call`.
#' - `0` if `x` and the `y` exist in the recursive path.
#' - `1` if only `x` exists in the recursive path.
#' - `2` if only `y` exists in the recursive path.
#' @keywords internal
.params <- function(x, y, .expr, .name, .depth, .type) NULL

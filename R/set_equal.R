#' Recursive `setequal` for lists
#'
#' `r lifecycle::badge("experimental")`\cr
#' Gets recursive `setequal` for lists.
#' @rdname set_equal
#' @docType methods
#'
#' @inheritParams .params
#'
#' @return object of the same class as `x` or NULL if doesn't differ from `y`.
#'
#' @examples
#' set_equal(c(1, 2), c(2, 3))
#' set_equal(list(a = 1, b = 2), list(b = 2, c = 3))
#' set_equal(letters[1:2], letters[2:3])
#' @export
setGeneric("set_equal", function(x, y, .expr = NULL, .name = NULL, .depth = 0L) {
  .expr <- substitute(.expr)
  rlang::eval_bare(expr = .expr, env = environment())

  if (identical(x, y)) return(TRUE)

  standardGeneric("set_equal")
})

#' @rdname set_equal
setMethod("set_equal", signature(x = "ANY", y = "ANY"), function(x, y, .expr = FALSE, .name = NULL, .depth = 0L) {
  base::setequal(x, y)
})

#' @rdname set_equal
setMethod("set_equal", signature(x = "list", y = "list"), function(x, y, .expr = NULL, .name = NULL, .depth = 0L) {
  if (!is.null(names(x)) && !is.null(names(y))) {
    if (!setequal(names(x), names(y))) return(FALSE)
    all(
      sapply(names(x), simplify = TRUE, FUN = function(name) {
          eval(substitute(
            set_equal(x[[name]], y[[name]], .expr = .expr, .name = name, .depth = .depth + 1L),
            list(.expr = .expr)
          ))
        }
      )
    )
  } else {
    setequal(x, y)
  }
})
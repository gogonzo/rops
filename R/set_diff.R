
#' Recursive `setdiff` for lists
#'
#' `r lifecycle::badge("experimental")`\cr
#' Gets recursive set-theoretic difference of X and Y
#' @rdname set_diff
#' @docType methods
#'
#' @inheritParams .params
#'
#' @return object of the same class as `x` or NULL if doesn't differ from `y`.
#'
#' @examples
#' set_diff(c(1, 2), c(2, 3))
#' set_diff(list(a = 1, b = 2), list(b = 2, c = 3))
#' set_diff(letters[1:2], letters[2:3])
#' @export
setGeneric("set_diff", function(x, y, .expr = NULL, .depth = 0L, .name = NULL) {
  .expr <- substitute(.expr)
  rlang::eval_bare(expr = .expr, env = environment())

  isequal <- set_equal(x, y, .depth = .depth + 1L, .name = .name)
  if (isequal) return(NULL)

  standardGeneric("set_diff")
})

#' @rdname set_diff
setMethod(
  "set_diff", signature(x = "ANY", y = "ANY"),
  function(x, y, .expr = NULL, .depth = 0L, .name = NULL) {
    base::setdiff(x, y)
  }
)

#' @rdname set_diff
setMethod(
  "set_diff", signature(x = "list", y = "ANY"),
  function(x, y, .expr = NULL, .depth = 0L, .name = NULL) {
    if (!is.null(names(x)) && !is.null(names(y))) {
      out <- Filter(
        length,
        sapply(names(x), simplify = FALSE, function(name) {
          eval(substitute(
            set_diff(x[[name]], y[[name]], .expr = .expr, .depth = .depth + 1L, .name = name),
            list(.expr = .expr)
          ))
        })
      )
      if (length(out) == 0) NULL else out
    } else {
      x
    }
  }
)
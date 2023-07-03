#' Recursive `intersect` for lists
#'
#' `r lifecycle::badge("experimental")`\cr
#' Gets recursive intersection difference of X and Y
#' @rdname set_intersect
#' @docType methods
#'
#' @inheritParams .params
#'
#' @return object of the same class as `x` or NULL if doesn't differ from `y`.
#'
#' @examples
#' set_intersect(c(1, 2), c(2, 3))
#' set_intersect(list(a = 1, b = 2), list(b = 2, c = 3))
#' set_intersect(letters[1:2], letters[2:3])
#' @export
setGeneric("set_intersect", function(x, y, .expr = NULL, .depth = 0L, .name = NULL) {
 .expr <- substitute(.expr)
  rlang::eval_bare(expr = .expr, env = environment())

  isequal <- set_equal(x, y, .depth = .depth + 1L, .name = .name)
  if (isequal) return(x)

  standardGeneric("set_intersect")
})

#' @rdname set_intersect
setMethod("set_intersect", signature(x = "ANY", y = "ANY"), function(x, y, .expr = NULL, .depth = 0L, .name = NULL) {
  base::intersect(x, y)
})

#' @rdname set_intersect
setMethod("set_intersect", signature(x = "list", y = "ANY"), function(x, y, .expr = NULL, .depth = 0L, .name = NULL) {
  if (!is.null(names(x)) && !is.null(names(y))) {
    a <- intersect(names(x), names(y))
    out <- Filter(
      length,
      sapply(a, USE.NAMES = TRUE, simplify = FALSE, function(name) {
        eval(substitute(
          set_intersect(x[[name]], y[[name]], .expr = .expr, .depth = .depth + 1L, .name = name),
          list(.expr = .expr)
        ))
        }
      )
    )
    if (length(out) == 0) NULL else out
  } else {
    NULL
  }
})

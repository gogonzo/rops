#' Recursive `union` for lists
#'
#' `r lifecycle::badge("experimental")`\cr
#' Gets recursive `union` for lists.
#' @rdname set_union
#' @docType methods
#'
#' @inheritParams .params
#'
#' @return object of the same class as `x` or NULL if doesn't differ from `y`.
#'
#' @examples
#' set_union(c(1, 2), c(2, 3))
#' set_union(list(a = 1, b = 2), list(b = 2, c = 3))
#' set_union(letters[1:2], letters[2:3])
#' @export
setGeneric("set_union", function(x, y, .expr = if (FALSE) return(x), .depth = 0L, .name = NULL) {
  .expr <- substitute(.expr)
  rlang::eval_bare(expr = .expr, env = environment())

  standardGeneric("set_union")
})
#' @rdname set_union
setMethod(
  "set_union",
  signature(x = "ANY", y = "ANY"),
  function(x, y, .expr = if (FALSE) return(x), .depth = 0L, .name = NULL) {
    c(x, y)
  }
)

#' @rdname set_union
setMethod(
  "set_union",
  signature(x = "atomic", y = "atomic"),
  function(x, y, .expr = if (FALSE) return(x), .depth = 0L, .name = NULL) {
    duplicated_y <- if (!is.null(names(x)) && !is.null(names(y))) {
      y %in% x & names(y) %in% names(x)
    } else {
      y %in% x
    }
    c(x, y[!duplicated_y])
  }
)

#' @rdname set_union
setMethod(
  "set_union",
  signature(x = "list", y = "list"),
  function(x, y, .expr = if (FALSE) return(x), .depth = 0L, .name = NULL) {
    if (!is.null(names(x)) && !is.null(names(y))) {
      all_names <- union(names(x), names(y))
      sapply(all_names, USE.NAMES = TRUE, simplify = FALSE, function(i) {
        eval(substitute(
          set_union(
            x = x[[i]], y = y[[i]], .expr = .expr, .depth = .depth + 1L, .name = i
          ),
          list(.expr = .expr)
        ))
      })
    } else {
      c(x, y)
    }
  }
)

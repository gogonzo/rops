#' Print differences between two objects
#'
#' Print differences between two objects.
#' @rdname diffs
#' @docType methods
#'
#' @inheritParams .params
#'
#' @return invisible `NULL`
#'
#'
#' @examples
#' diffs(c(1, 2), c(2, 3))
#' diffs(c(a = 1, b = 2), c(c = 2, d = 3))
#' diffs(letters[1:2], letters[2:3])
#' @export
setGeneric("diffs", function(x, y, .depth = 0L, .name = NULL, .type = 0L) {
  if (!is.null(.name) && .depth != 0L) {
    .print_type(.name, .depth = .depth - 1L, .type = .type, .is_name = TRUE)
  }
  standardGeneric("diffs")
  invisible(NULL)
})

#' @rdname diffs
setMethod("diffs", signature(x = "list", y = "NULL"), function(x, y, .depth = 0L, .name = NULL, .type = 0L) {
  lapply(names(x), function(.name) {
    diffs(x[[.name]], NULL, .depth = .depth + 1L, .name = .name, .type = .type)
  })
})

#' @rdname diffs
setMethod("diffs", signature(x = "NULL", y = "list"), function(x, y, .depth = 0L, .name = NULL, .type = 0L) {
  lapply(names(y), function(.name) {
    diffs(y[[.name]], NULL, .depth = .depth + 1L, .name = .name, .type = .type)
  })
})

#' @rdname diffs
setMethod("diffs", signature(x = "ANY", y = "NULL"), function(x, y, .depth = 0L, .name = NULL, .type = 0L) {
  .print_type(x, .depth = .depth, .type = .type)
})

#' @rdname diffs
setMethod("diffs", signature(x = "NULL", y = "ANY"), function(x, y, .depth = 0L, .name = NULL, .type = 0L) {
  .print_type(y, .depth = .depth, .type = .type)
})

#' @rdname diffs
setMethod("diffs", signature(x = "NULL", y = "NULL"), function(x, y, .depth = 0L, .name = NULL, .type = 0L) {
  .print_type(x, .depth = .depth, .type = .type)
})

#' @rdname diffs
setMethod("diffs", signature(x = "atomic", y = "atomic"), function(x, y, .depth = 0L, .name = NULL, .type = 0L) {
  if (!is.null(names(x)) || !is.null(names(y))) {
    warning("rops doesn't support comparison between named atomics")
  }
  ab <- intersect(x, y)
  a <- setdiff(x, y)
  b <- setdiff(y, x)

  if (length(a))  .print_type(a, .depth = .depth, .type = if (.type != 0L) .type else 1L)
  if (length(ab)) .print_type(ab, .depth = .depth, .type = if (.type != 0L) .type else 0L)
  if (length(b))  .print_type(b, .depth = .depth, .type = if (.type != 0L) .type else 2L)
})

#' @rdname diffs
setMethod("diffs", signature(x = "list", y = "list"), function(x, y, .depth = 0L, .name = NULL, .type = 0L) {
  out <- if (!is.null(names(x)) && !is.null(names(y))) {
    all_names <- union(names(x), names(y))
    sapply(all_names, simplify = FALSE, function(name) {
      .type <- if (.type != 0L) {
        .type
      } else if (name %in% names(x) && name %in% names(y)) {
        0L
      } else if (name %in% names(x) && !name %in% names(y)) {
        1L
      } else {
        2L
      }
      diffs(x[[name]], y[[name]], .depth = .depth + 1L, .name = name, .type = .type)
    })
  } else {
    lapply(seq_along(x), function(idx) {
      diffs(x[[idx]], NULL, .depth = .depth + 1L, .name = as.character(idx), .type = 1L)
    })
    lapply(seq_along(y), function(idx) {
      diffs(y[[idx]], NULL, .depth = .depth + 1L, .name = as.character(idx), .type = 2L)
    })
  }
})
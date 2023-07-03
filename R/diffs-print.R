.print_type <- function(x, .depth, .type, ...) {
  if (requireNamespace("crayon", quietly = TRUE) && crayon::has_color()) {
    cat(.format_crayon(x, .type, .depth,  ...))
  } else {
    cat(.format_plain(x, .type, .depth, ...))
  }
}


.format_crayon <- function(x, .type, .depth, ...) {
  x <- .format_indent(x, .depth = .depth, ...)
  if (.type == 0L) {
    crayon::style(x)
  } else if (.type == 1L) {
    crayon::style(x, "green")
  } else if (.type == 2L) {
    crayon::style(x, "red")
  }
}

.format_plain <- function(x, .type, .depth, ...) {
  fmt <- if (.type == 0L) {
    ". "
  } else if (.type == 1L) {
    "+ "
  } else if (.type == 2L) {
    "- "
  }
  .format_indent(x, .depth = .depth, .prefix = fmt, ...)
}

.format_indent <- function(x, .depth = 0L, .is_name = FALSE, .prefix = character(0)) {
  indent <- paste(rep(" ", .depth * 2L), collapse = "")
  if (length(.prefix)) {
    if (length(x) == 0) {
      "[]"
    } else if (.is_name) {
      sprintf("\n%s%s%s:", .prefix, indent, x)
    } else {
      sprintf("\n%s%s- %s", .prefix, indent, x)
    }
  } else {
    if (length(x) == 0) {
      " []"
    } else if (.is_name) {
      sprintf("\n%s%s:", indent, x)
    } else if (length(x) > 1) {
      sprintf("\n%s- %s", indent, x)
    } else {
      sprintf(" %s", x)
    }
  }
}

testthat::test_that(".format_indent returns NULL as [] no matter what", {
  testthat::expect_identical(.format_indent(NULL, .depth = 100L), "[]")
  testthat::expect_identical(.format_indent(NULL, .depth = 100L, .prefix = "."), "[]")
  testthat::expect_identical(.format_indent(NULL, .depth = 100L, .is_name = TRUE), "[]")
})

testthat::test_that(".format_indent begins name from the new line always", {
   testthat::expect_identical(.format_indent("text", .is_name = TRUE), "\ntext:")
   testthat::expect_identical(.format_indent("text", .is_name = TRUE, .prefix = "++"), "\n++text:")
})

testthat::test_that(".format_indent returns single item with or without prefix", {
  testthat::expect_identical(.format_indent("text", .depth = 2L), " text")
})

testthat::test_that(".format_indent begins single item from the new line if prefix is specified", {
  testthat::expect_identical(.format_indent("text", .depth = 2L, .prefix = "++"), "\n++    - text")
})

testthat::test_that(".format_indent adds .depth * 2 spaces", {
  testthat::expect_identical(.format_indent("text", .depth = 2L, .is_name = TRUE), "\n    text:")
})

testthat::test_that(".format_indent begins each element from new line with ", {
  testthat::expect_identical(.format_indent(letters[1:3]), c("\n- a", "\n- b", "\n- c"))
  testthat::expect_identical(.format_indent(letters[1:3], .prefix = "?"), c("\n?- a", "\n?- b", "\n?- c"))
})

testthat::test_that(".format_plain returns formatted text with . + or - in the beginning", {
  testthat::expect_identical(.format_plain("elo", .type = 0L, .depth = 1L), "\n.   - elo")
  testthat::expect_identical(.format_plain("elo", .type = 1L, .depth = 1L), "\n+   - elo")
  testthat::expect_identical(.format_plain("elo", .type = 2L, .depth = 1L), "\n-   - elo")
})

testthat::test_that(".format_plain passes .is_name correctly", {
  testthat::expect_identical(.format_plain("elo", .type = 0L, .depth = 1L, .is_name = TRUE), "\n.   elo:")
  testthat::expect_identical(.format_plain("elo", .type = 1L, .depth = 1L, .is_name = TRUE), "\n+   elo:")
  testthat::expect_identical(.format_plain("elo", .type = 2L, .depth = 1L, .is_name = TRUE), "\n-   elo:")
})

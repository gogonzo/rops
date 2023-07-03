testthat::test_that(
  "vector elements differ if their name or value is the same. Otherwise they are both included", {
  testthat::expect_identical(
    set_union(
      c(a = 1, b = 2),
      c(b = 3, c = 4),
    ),
    c(a = 1, b = 2, b = 3, c = 4)
  )
})

testthat::test_that(
  "to remove duplicated y elements by name - need to use .expr", {
  testthat::expect_identical(
    set_union(
      c(a = 1, b = 2),
      c(b = 3, c = 4),
      .expr = return(c(x, y[!names(y) %in% names(x)]))
    ),
    c(a = 1, b = 2, c = 4)
  )
})

testthat::test_that(
  "when x is unnamed it's combined with named y", {
    testthat::expect_identical(
      set_union(
        c(1, 2),
        c(b = 3, c = 4)
      ),
      c(1, 2, b = 3, c = 4)
    )
  }
)

testthat::test_that(
  "Two unnamed are combined into one with removed duplicates", {
    testthat::expect_identical(
      set_union(c(1, 2), c(2, 3)),
      c(1, 2, 3)
    )
  }
)

testthat::test_that(
  ".expr to return a list instead of vector", {
    testthat::expect_identical(
      set_union(c(1, 2), c(2, 3), .expr = return(list(a = 2))),
      list(a = 2)
    )
  }
)

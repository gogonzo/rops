testthat::test_that("set_diff of single atomic vectors works like base::setdiff", {
  testthat::expect_identical(
    set_diff(c(1L, 2L), c(2L, 3L)),
    1L
  )
})

testthat::test_that(".expr can define the match", {
  testthat::expect_identical(
    set_diff(c(1L, 2L), c(2L, 3L), .expr = if (length(x) == length(y)) return(NULL)),
    NULL
  )
})

testthat::test_that("atomic vs NULL", {
  testthat::expect_identical(
    set_diff(c(1L, 2L), NULL),
    c(1L, 2L)
  )
  testthat::expect_identical(
    set_diff(NULL, c(1L, 2L)),
    NULL
  )
})

testthat::test_that("list are differed by the named elements", {
  testthat::expect_identical(
    set_diff(list(a = 1, b = 2), list(b = 2, a = 1)),
    NULL
  )
})

testthat::test_that("list element differs if any of it's nested elements differs", {
  testthat::expect_identical(
    set_diff(
      list(a = list(x = list(y = 1), b = 2), d = 2),
      list(a = list(x = list(y = 2), b = 2), c = 3)
    ),
    list(a = list(x = list(y = 1)), d = 2)
  )
})

testthat::test_that("condition can influence match", {
  testthat::expect_identical(
    set_diff(
      list(
        a = list(b = list(y = 1), d = 2, e = 1),
        f = 2
      ),
      list(
        a = list(b = list(y = 2), c = 2),
        g = 3
      ),
       .expr = if (identical(names(x), c("b", "d", "e"))) return(list())
    ),
    list(f = 2)
  )
})

testthat::test_that("Possible to use function arguments in the .expr (.depth, .name)", {
  testthat::expect_identical(
    set_diff(
      list(a = list(x = list(y = 1), b = 2), d = 2),
      list(a = list(x = list(y = 2), b = 2), c = 3),
      .expr = if (.depth >= 3L) return(NULL)
    ),
    list(d = 2)
  )

  testthat::expect_identical(
    set_diff(
      list(a = list(x = list(y = 1), b = 2), d = 2),
      list(a = list(x = list(y = 2), b = 2), c = 3),
      .expr = if (isTRUE(.name == "a")) return(NULL)
    ),
    list(d = 2)
  )
})

testthat::test_that("identical list always NULL regardless of .expr", {
  testthat::expect_identical(
    set_diff(list(a = 1), list(a = 1), .expr = FALSE),
    NULL
  )
})


testthat::test_that("Unnamed list can't be compared - always returns x", {
  testthat::expect_identical(
    set_diff(
      list(1, 2),
      list(1, 2, 3),
      .expr = FALSE
    ),
    list(1, 2)
  )
})
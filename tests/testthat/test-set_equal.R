testthat::test_that("set_equal of different atomic vectors", {
  testthat::expect_false(set_equal(c(1L, 2L), c(2L, 3L)))
})

testthat::test_that("set_equal of the same atomic vectors but ordered differently", {
  testthat::expect_true(set_equal(c(1L, 2L), c(2L, 1L)))
})

testthat::test_that(".expr can define the match", {
  testthat::expect_true(
    set_equal(c(1L, 2L), c(2L, 3L), .expr = if (length(x) == length(y)) return(TRUE))
  )
})

testthat::test_that("they differ if one is null", {
  testthat::expect_false(set_equal(c(1L, 2L), NULL))
  testthat::expect_false(set_equal(NULL, c(1L, 2L)))
})

testthat::test_that("list are differed by the named elements", {
  testthat::expect_false(set_equal(list(a = 1, b = 2), list(b = 2, c = 3)))
})

testthat::test_that("list are considered the same if they have the same elements with different order", {
  testthat::expect_true(set_equal(list(a = 1, b = 2), list(b = 2, a = 1)))
})

testthat::test_that("list element differs if any of it's nested elements differs", {
  testthat::expect_false(
    set_equal(
      list(a = list(x = list(y = 1), b = 2), d = 2),
      list(a = list(x = list(y = 2), b = 2), c = 3),
    )
  )
})

testthat::test_that("condition can influence match", {
  testthat::expect_true(
    set_equal(
      list(
        a = list(b = list(y = 1), d = 2, e = 1),
        f = 2
      ),
      list(
        a = list(b = list(y = 2), c = 2),
        f = 3
      ),
      .expr = if (.depth > 0L) return(TRUE)
    )
  )
})

testthat::test_that(".expr is always executed first - even if the objects are identical", {
  testthat::expect_identical(
    set_equal(list(a = 1), list(a = 1), .expr = return(NULL)),
    NULL
  )
})

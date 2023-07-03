testthat::test_that("set_intersect on atomic vectors", {
  testthat::expect_identical(set_intersect(c(1L, 2L), c(2L, 3L)), 2L)
})

testthat::test_that("set_intersect of the same atomic vectors but ordered differently", {
  testthat::expect_identical(
    set_intersect(c(2L, 1L), c(1L, 2L)),
    c(2L, 1L)
  )
})

testthat::test_that(".expr can define the match", {
  testthat::expect_identical(
    set_intersect(c(1L, 2L), c(2L, 3L), .expr = if (length(x) == length(y)) return(c(x, y))),
    c(1L, 2L, 2L, 3L)
  )
})

testthat::test_that("they differ if one is null", {
  testthat::expect_identical(set_intersect(c(1L, 2L), NULL), NULL)
  testthat::expect_identical(set_intersect(NULL, c(1L, 2L)), NULL)
})

testthat::test_that("list are differed by the named elements", {
  testthat::expect_identical(
    set_intersect(list(a = 1, b = 2), list(b = 2, c = 3)),
    list(b = 2)
  )
})

testthat::test_that("list are considered the same if they have the same elements with different order", {
  testthat::expect_identical(
    set_intersect(list(a = 1, b = 2), list(b = 2, a = 1)),
    list(a = 1, b = 2)
  )
})

testthat::test_that("list element differs if any of it's nested elements differs", {
  testthat::expect_identical(
    set_intersect(
      list(a = list(x = list(y = 1), b = 2), d = 2),
      list(a = list(x = list(y = 2), b = 2), c = 3),
    ),
    list(a = list(b = 2))
  )
})

testthat::test_that("condition can influence match", {
  testthat::expect_identical(
    set_intersect(
      list(
        a = list(b = list(y = 1), d = 2, e = 1),
        f = 2
      ),
      list(
        a = list(b = list(y = 2), c = 2),
        f = 3
      ),
      .expr = if (.depth > 0L) return(x)
    ),
    list(
      a = list(b = list(y = 1), d = 2, e = 1),
      f = 2
    )
  )
})


testthat::test_that("unnamed list are not possible to compare, this NULL is returned", {
  testthat::expect_identical(set_intersect(list(1), list(2)), NULL)
})

testthat::test_that(".expr executed first regardless of the fact that x and y are identical", {
  testthat::expect_identical(
    set_intersect(list(a = 1L), list(a = 1L), .expr = return(NULL)),
    NULL
  )
})

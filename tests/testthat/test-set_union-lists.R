testthat::test_that("list are set_uniond by name - if names are unique they are appended", {
    testthat::expect_identical(
      set_union(
        list(a = 1, b = 2),
        list(c = 3, d = 4)
      ),
      list(a = 1, b = 2, c = 3, d = 4)
    )
  }
)

testthat::test_that("list are set_uniond by name - duplicated named lists are set_uniond recursively", {
  testthat::expect_identical(
    set_union(
      list(a = 1, b = list(bb = c(1, 2))),
      list(       b = list(bb = c(2, 3)), d = 4)
    ),
    list(a = 1, b = list(bb = c(1, 2, 3)), d = 4)
  )
}
)

testthat::test_that(
  "list are set_uniond by name - all.x combine arrays from duplicated lists", {
    testthat::expect_identical(
      set_union(
        list(a = 1, b = 2),
        list(       b = 3, c = 4)
      ),
      list(a = 1, b = c(2, 3), c = 4)
    )
  }
)

testthat::test_that(
  "list are set_uniond by name - all.x keeps only x from duplicated lists", {
    testthat::expect_identical(
      set_union(
        list(a = 1, b = 2),
        list(       b = 3, c = 4),
        .expr = if (is.atomic(x) && !is.null(x)) return(x)
      ),
      list(a = 1, b = 2, c = 4)
    )
  }
)

testthat::test_that("list are combined if any is unnamed", {
  testthat::expect_identical(
    set_union(
      list(1, 2),
      list(b = 3, c = 4)
    ),
    list(1, 2, b = 3, c = 4)
  )
  testthat::expect_identical(
    set_union(
      list(a = 1, b = 2),
      list(3, 4)
    ),
    list(a = 1, b = 2, 3, 4)
  )
  }
)

testthat::test_that("list are combined if any is empty", {
  testthat::expect_identical(
    set_union(
      list(),
      list(3, 4)
    ),
    list(3, 4)
  )
  testthat::expect_identical(
    set_union(
      list(a = 1, b = 2),
      list()
    ),
    list(a = 1, b = 2)
  )
})

testthat::test_that("combining list by the NULL elements", {
  testthat::expect_identical(
    set_union(
      list(a = NULL),
      list(b = NULL)
    ),
    list(a = NULL, b = NULL)
  )
  testthat::expect_identical(
    set_union(
      list(NULL),
      list(b = NULL)
    ),
    list(NULL, b = NULL)
  )
  testthat::expect_identical(
    set_union(
      list(a = NULL),
      list(NULL)
    ),
    list(a = NULL, NULL)
  )
  testthat::expect_identical(
    set_union(
      list(a = list()),
      list(b = list())
    ),
    list(a = list(), b = list())
  )
  testthat::expect_identical(
    set_union(
      list(),
      list()
    ),
    list()
  )
}
)

testthat::test_that("merging NULL with y results in y", { # for example when x[[i]] is null
  testthat::expect_identical(
    set_union(
      NULL,
      list(3, 4)
    ),
    list(3, 4)
  )
  testthat::expect_identical(
    set_union(
      NULL,
      c(3, 4)
    ),
    c(3, 4)
  )

  testthat::expect_identical(
    set_union(
      NULL,
      list()
    ),
    list()
  )
  testthat::expect_identical(
    set_union(
      NULL,
      character(0)
    ),
    character(0)
  )
}
)

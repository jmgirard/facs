# test expand_optional ---------------------------------------------------------
testthat::test_that("expansion works with separate sets", {
  # Stringify works
  t1 <- expand_optional(
    require = c(1, 3),
    optional = 2,
    stringify = TRUE
  )
  testthat::expect_type(t1, "character")
  testthat::expect_length(t1, 2)
  testthat::expect_equal(t1[[1]], "1+3")
  testthat::expect_equal(t1[[2]], "1+2+3")

  # Non-stringify works
  t2 <- expand_optional(
    require = c(1, 3),
    optional = 2,
    stringify = FALSE
  )
  testthat::expect_type(t2, "list")
  testthat::expect_length(t2, 2)
  testthat::expect_equal(t2[[1]], c(1, 3))
  testthat::expect_equal(t2[[2]], c(1, 2, 3))
})

# test get_subsets -------------------------------------------------------------
testthat::test_that("subsets works", {
  # Works with three numbers
  t1 <- get_subsets(1:3)
  testthat::expect_type(t1, "list")
  testthat::expect_length(t1, 8)
  testthat::expect_equal(t1[[1]], integer(0))
  testthat::expect_equal(t1[[8]], 1:3)

  # Works with two strings
  t2 <- get_subsets(letters[1:2])
  testthat::expect_type(t2, "list")
  testthat::expect_length(t2, 4)
  testthat::expect_equal(t2[[1]], character(0))
  testthat::expect_equal(t2[[4]], letters[1:2])

  # Works with 1 number
  t3 <- get_subsets(pi)
  testthat::expect_type(t3, "list")
  testthat::expect_length(t3, 2)
  testthat::expect_equal(t3[[1]], numeric(0))
  testthat::expect_equal(t3[[2]], pi)

  # Work with empty set
  t4 <- get_subsets(numeric(0))
  testthat::expect_type(t4, "list")
  testthat::expect_length(t4, 1)
  testthat::expect_equal(t4[[1]], numeric(0))
})
testthat::test_that("extraction functions work", {
  x <- c("12", "L12", "12B", "L12B")
  testthat::expect_equal(extract_prefixes(x), c(NA, "L", NA, "L"))
  testthat::expect_equal(extract_numcodes(x), c("12", "12", "12", "12"))
  testthat::expect_equal(extract_suffixes(x), c(NA, NA, "B", "B"))
})

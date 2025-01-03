testthat::test_that("extraction functions work", {
  x <- c("12", "L12", "12B", "L12B")
  testthat::expect_equal(extract_prefixes(x), c(NA, "L", NA, "L"))
  testthat::expect_equal(extract_numcodes(x), c("12", "12", "12", "12"))
  testthat::expect_equal(extract_suffixes(x), c(NA, NA, "B", "B"))
})

testthat::test_that("part checking works", {
  testthat::expect_true(check_prefixes(facs_prefixes))
  testthat::expect_true(check_prefixes(NA_character_))
  testthat::expect_false(check_prefixes(c("Q","L")))
  testthat::expect_true(check_numcodes(facs_codes$number))
  testthat::expect_false(check_numcodes(c("12", "300")))
})

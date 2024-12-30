testthat::test_that("clean_facs works", {
  # Check with single string out of order
  t1 <- clean_facs("1+4+2")
  testthat::expect_equal(t1, "1+2+4")
  # Check with multiple strings
  t2 <- clean_facs(c("1+4+2", "10+9+15"))
  testthat::expect_type(t2, "character")
  testthat::expect_equal(t2[[1]], "1+2+4")
  testthat::expect_equal(t2[[2]], "9+10+15")
  # Check with possible but some lowercase characters
  t3 <- clean_facs("1a+L2b+4c+5d+6e+R12C+U14B")
  testthat::expect_equal(t3, "1A+L2B+4C+5D+6E+R12C+U14B")
  # Check with impossible characters
  testthat::expect_error(clean_facs("1,2"))
  testthat::expect_error(clean_facs("@"))
  # Check with extra plus signs
  testthat::expect_error(clean_facs("+1a+2b+"))
  # Check with empty string
  testthat::expect_equal(clean_facs(""), "")
})

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

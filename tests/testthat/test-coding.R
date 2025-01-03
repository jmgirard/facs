testthat::test_that("coding works", {
  testthat::expect_true(all(check_coding(
    c("U12A", "L12", "12B", "12", "U12A+L20B", "1+2+4")
  )))
  testthat::expect_false(check_coding("Q12A"))
  testthat::expect_false(check_coding("12P"))
  testthat::expect_false(check_coding(""))
  testthat::expect_false(check_coding("A"))
  testthat::expect_false(check_coding("AU12"))
  testthat::expect_false(check_coding("1++2"))
  testthat::expect_false(check_coding("1+3+"))

  testthat::expect_equal(
    validate_coding("L12B + R2C"),
    "R2C+L12B", 
    ignore_attr = TRUE
  )
})

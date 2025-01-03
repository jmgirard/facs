#' Compare two codings on occurrence
#'
#' Compare two FACS codings on which AUs are present vs. absent. Uses the Wexler
#' formula recommended in the 2002 FACS Investigator's Guide.
#'
#' Let \eqn{P} be the number of AUs that occur in both the `x` and `y` codings
#' and let \eqn{Q} be the total number of AUs that occur in `x` and `y`
#' (counting repeated AUs twice). Wexler's formula is simply \eqn{2P/Q}.
#'
#' @param x,y A pair of coding objects created by [coding()].
#' @return A numeric vector containing the agreement scores between each
#'   corresponding element of `x` and `y`.
#' @examples
#' compare_occurrence(coding("1+4+9"), coding("1+4+10"))
#' @export
compare_occurrence <- function(x, y) {
  # Validate input
  stopifnot(class(x) == "facs_coding")
  stopifnot(class(y) == "facs_coding")
  stopifnot(length(x) == length(y))
  # Preallocate output vector
  out <- rep(NA_real_, times = length(x))
  # Tidy both sets of codes
  tidy_x <- tidy(x)
  tidy_y <- tidy(y)
  # For each code in both sets...
  for (i in seq_along(x)) {
    # Extract occurrence
    xo <- tidy_x[[i]]$occurrence
    yo <- tidy_y[[i]]$occurrence
    # Apply Welxer's formula
    out[[i]] <- (length(intersect(xo, yo)) * 2) / length(c(xo, yo))
  }
  # Return
  out
}

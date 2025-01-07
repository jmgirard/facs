#' @export
compare_proto <- function(coding, prototypes) {
  # Validate input
  stopifnot(length(coding) == 1)
  stopifnot(class(coding) == "facs_coding")
  stopifnot(class(prototypes) == "facs_coding")
  # Preallocate output vector
  out <- rep(NA_real_, times = length(prototypes))
  # Tidy both sets of codes
  co <- tidy(c)$occurrence
  tidy_p <- tidy(p)
  # For each code in both sets...
  for (i in seq_along(prototypes)) {
    # Extract occurrence
    po <- tidy_p[[i]]$occurrence
    # Calculate dice coefficient
    out[[i]] <- (length(intersect(co, po)) * 2) / length(c(co, po))
  }
  # Return
  out
}

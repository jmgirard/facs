#' @export
expand_optional <- function(required, optional, stringify = TRUE) {
  # Generate all subsets of optional elements
  optional_subsets <- get_subsets(optional)
  
  # Combine each optional subset with the required elements
  out <- lapply(
    X = optional_subsets, 
    FUN = function(opt_subset) unique(sort(c(required, opt_subset)))
  )

  # If requested, stringify the output
  if (stringify) {
    out <- lapply(
      X = out,
      FUN = function(x) paste(x, collapse = "+")
    )
    out <- unlist(out, recursive = FALSE)
  }

  # Return 
  out
}

get_subsets <- function(vec) {
  # Validate input
  stopifnot(is.vector(vec))
  n <- length(vec)
  if (n == 1) {
    out <- list(vector(mode = typeof(vec), length = 0), vec)
  } else if (n > 1) {
    out <- lapply(
      X = 0:n,
      FUN = function(m) combn(vec, m, simplify = FALSE)
    )
    out <- unlist(out, recursive = FALSE)
  } else {
    out <- list(vector(mode = typeof(vec), length = 0))
  }
  out
}

#' @export
expand_optional <- function(required, optional, stringify = TRUE) {
  # Validate input
  stopifnot(is.vector(required))
  stopifnot(is.vector(optional))
  stopifnot(rlang::is_bool(stringify))
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
  # Generate all subsets of vec including empty and full sets
  n <- length(vec)
  if (n == 1) {
    # Generate manually if n == 1 due to how combn(x) handles single integers
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
  # Return
  out
}

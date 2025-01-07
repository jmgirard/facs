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
    FUN = function(opt_subset) unique(sort_by_numeric(c(required, opt_subset)))
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

expand_alternatives <- function(required, alternatives, stringify = TRUE) {
  # Validate input
  stopifnot(is.vector(required))
  stopifnot(is.vector(alternatives))
  stopifnot(rlang::is_bool(stringify))
  # Generate all subsets of alternative elements
  alternative_subsets <- get_subsets(alternatives, drop_empty = TRUE)
  if (length(alternative_subsets) == 0) {
    out <- list(unique(sort_by_numeric(required)))
  } else {
    out <- lapply(
      X = alternative_subsets,
      FUN = function(alt) unique(sort_by_numeric(c(required, alt)))
    )
  }
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

get_subsets <- function(vec, drop_empty = FALSE, drop_full = FALSE) {
  # Validate input
  stopifnot(is.vector(vec))
  # Coerce to character to prevent issues with combn(x) when x is a scalar
  cvec <- as.character(vec)
  # Find all combinations including empty (0) and full (n)
  out <- lapply(
    X = 0:length(cvec),
    function(m) combn(cvec, m, simplify = FALSE)
  )
  out <- unlist(out, recursive = FALSE)
  # Drop empty and/or full sets if requested
  if (drop_empty) out <- out[-1]
  if (drop_full) out <- out[-length(out)]
  # Return
  out
}

expand_strings <- function(chr, delim = "+") {
  strsplit(chr, split = delim, fixed = TRUE)
}

sort_by_numeric <- function(chr) {
  # Extract numeric parts from each element
  numeric_parts <- sapply(chr, extract_number)
  # Order the input vector based on the numeric parts
  out <- chr[order(numeric_parts, na.last = TRUE)]
  # Return
  out
}

extract_number <- function(str) {
  # Use a regular expression to extract the numeric part
  matches <- regmatches(str, regexpr("\\d{1,2}", str))
  # Convert to numeric for sorting
  as.numeric(matches)
}

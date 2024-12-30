new_facs <- function(x = character()) {
  stopifnot(rlang::is_character(x, n = 1))
  y <- clean_facs(x)
  d <- extract_details(y)
  structure(
    y,
    sym = d$prefixes,
    num = d$numcodes,
    int = d$suffixes,
    class = "facs"
  )
}

#' @export
clean_facs <- function(x) {
  # Validate input
  stopifnot(is.character(x))
  # Remove whitespace
  y <- gsub("\\s+", "", x)
  # Capitalize all letters (except the i in Bi)
  y <- toupper(y)
  y <- gsub("BI", "Bi", y)
  # Check for unallowed characters
  unallowed <- paste0("[^", facs_characters, "]")
  if (any(grepl(unallowed, y))) stop("Unallowed characters found in `x`")
  # Split on plus signs
  y <- strsplit(y, split = "+", fixed = TRUE)
  # Check for empty parts
  if ("" %in% unlist(y)) stop("Misplaced plus signs found in `x`")
  # Sort by numeric components
  y <- lapply(y, sort_by_numeric)
  # Recombine into string
  y <- lapply(y, function(z) paste(z, collapse = "+"))
  y <- unlist(y)
  # Return
  y
}

extract_details <- function(x) {
  stopifnot(rlang::is_character(x, n = 1))
  # Split on pluses
  y <- strsplit(x, split = "+", fixed = TRUE)[[1]]
  # Extract each part
  prefixes <- extract_prefixes(y)
  numcodes <- extract_numcodes(y)
  suffixes <- extract_suffixes(y)
  # Check for unallowed parts
  stopifnot(check_prefixes(prefixes))
  stopifnot(check_numcodes(numcodes))
  stopifnot(check_suffixes(suffixes))
  # Return
  list(
    prefixes = prefixes,
    numcodes = numcodes,
    suffixes = suffixes
  )
}

extract_prefixes <- function(x) {
  find_matches(x, pattern = "^[A-Z]+")
}

extract_numcodes <- function(x) {
  find_matches(x, pattern = "\\d+")
}

extract_suffixes <- function(x) {
  find_matches(x, pattern = "[A-Z]+$")
}

find_matches <- function(x, pattern) {
  matches <- regexpr(pattern, x, perl = TRUE)
  result <- rep(NA_character_, length(x)) 
  result[matches > 0] <- regmatches(x, matches)
  result
}

check_prefixes <- function(x) {
  all(is.na(x) | x %in% facs_prefixes)
}

check_numcodes <- function(x) {
  all(x %in% facs_codes$number)
}

check_suffixes <- function(x) {
  all(is.na(x) | x %in% facs_suffixes)
}

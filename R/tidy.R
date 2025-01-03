parse_coding <- function(x) {
  stopifnot(rlang::is_character(x, n = 1))
  # Split on pluses
  y <- strsplit(x, split = "+", fixed = TRUE)[[1]]
  # Extract each part
  prefixes <- extract_prefixes(y)
  numcodes <- extract_numcodes(y)
  suffixes <- extract_suffixes(y)
  # Check for unallowed parts
  check_prefixes(prefixes)
  check_numcodes(numcodes)
  check_suffixes(suffixes)
  # Return
  data.frame(
    occ = numcodes,
    int = suffixes,
    sym = prefixes
  )
}

occ <- function(x) {
  stopifnot(class(x) == "facs_coding")
  info <- attributes(x)$info
  lapply(info, function(y) y$occ)
}

int <- function(x) {
  stopifnot(class(x) == "facs_coding")
  info <- attributes(x)$info
  lapply(info, function(y) setNames(object = y$int, nm = y$occ))
}

sym <- function(x) {
  stopifnot(class(x) == "facs_coding")
  info <- attributes(x)$info
  lapply(info, function(y) setNames(object = y$sym, nm = y$occ))
}


extract_prefixes <- function(x) {
  find_matches(
    x, 
    pattern = paste0(
      "^[", 
      paste(facs_prefixes, collapse = ""),
      "](?=\\d{1,2})"
    )
  )
}

extract_numcodes <- function(x) {
  find_matches(x, pattern = "\\d{1,2}")
}

extract_suffixes <- function(x) {
  find_matches(
    x, 
    pattern = paste0(
      "(?<=\\d{1,2})[", 
      paste(facs_suffixes, collapse = ""), 
      "]$"
    )
  )
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

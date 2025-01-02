# coding -----------------------------------------------------------------------
new_coding <- function(x = character()) {
  stopifnot(is.character(x))
  out <- structure(y, class = "facs_coding")
  out
}

validate_coding <- function(x) {
  # Extract attributes
  scheme <- attr(x, "scheme")
  # Remove all whitespace characters
  x <- gsub("\\s+", "", x)
  # Capitalize all letters
  x <- toupper(x)
  # Check for unallowed characters
  ccx <- check_coding(x)
  if (!all(ccx)) {
    cli::cli_abort(
      paste0(
        "The following elements of `x` are not valid FACS codes: ", 
        paste(paste0('(', which(!ccx), ') "', x[!ccx], '"'), collapse = ", ")
      )
    )
  }
  # Split on plus signs
  x <- strsplit(x, split = "+", fixed = TRUE)
  # Sort by numeric components
  x <- lapply(x, sort_by_numeric)
  # Recombine into string
  x <- lapply(x, function(z) paste(z, collapse = "+"))
  x <- unlist(x)
  # Return
  x
}

#' @export
coding <- function(x) {
  validate_coding(new_coding(x))
}

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

check_coding <- function(x) {
  pattern <- paste0(
    "^(?:[", 
    paste(facs_prefixes, collapse = ""), 
    "]?\\d{1,2}[", 
    paste(facs_suffixes, collapse = ""),
    "]?)(?:\\+[", 
    paste(facs_prefixes, collapse = ""),
    "]?\\d{1,2}[", 
    paste(facs_suffixes, collapse = ""),
    "]?)*$"
  )
  grepl(pattern, x)
}

extract_prefixes <- function(x) {
  find_matches(x, pattern = "^[A-Z](?=\\d)")
}

extract_numcodes <- function(x) {
  find_matches(x, pattern = "\\d+")
}

extract_suffixes <- function(x) {
  find_matches(x, pattern = "(?<=\\d)[A-Z]$")
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

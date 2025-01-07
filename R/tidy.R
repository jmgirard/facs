#' @method tidy facs_coding
#' @export
tidy.facs_coding <- function(x, ...) {
  lapply(x, parse_coding)
}

parse_coding <- function(x) {
  stopifnot(rlang::is_character(x, n = 1))
  # Split on pluses
  y <- strsplit(x, split = "+", fixed = TRUE)[[1]]
  # Extract each part
  prefixes <- extract_prefixes(y)
  numcodes <- extract_numcodes(y)
  suffixes <- extract_suffixes(y)
  # Return
  data.frame(
    occurrence = numcodes,
    intensity = suffixes,
    asymmetry = prefixes
  )
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
  if (all(is.na(x))) return(NA_character_)
  matches <- regexpr(pattern, x, perl = TRUE)
  result <- rep(NA_character_, length(x))
  result[matches > 0] <- regmatches(x, matches)
  result
}

#' @export
occurrence <- function(x, scheme) {
  # Validate input
  stopifnot(class(x) == "facs_coding")
  stopifnot(class(scheme) == "facs_scheme")
  stopifnot(length(scheme$occurrence) > 0)
  # Initialize the output matrix
  out <- matrix(0, nrow = length(x), ncol = length(scheme$occurrence))
  colnames(out) <- autopad(scheme$occurrence, prefix = "O")
  rownames(out) <- seq_along(x)
  # Populate the matrix
  tidy_x <- tidy(x)
  for (i in seq_along(tidy_x)) {
    df <- tidy_x[[i]]
    # TODO: Enforce partial NAs when using obscurance codes
    if (all(is.na(df$occurrence))) {
      out[i, ] <- rep(NA_integer_, times = length(scheme$occurrence))
    } else {
      out[i, ] <- as.integer(scheme$occurrence %in% df$occurrence)
    }
  }
  # Return
  out
}

#' @export
intensity <- function(x, scheme, type = "numerical") {
  # Validate input
  stopifnot(class(x) == "facs_coding")
  stopifnot(class(scheme) == "facs_scheme")
  stopifnot(length(scheme$intensity) > 0)
  type <- match.arg(type, choices = c("numerical", "character"))
  # Initialize the output matrix
  out <- matrix(0, nrow = length(x), ncol = length(scheme$intensity))
  colnames(out) <- autopad(scheme$intensity, prefix = "I")
  rownames(out) <- seq_along(x)
  # Populate the matrix
  tidy_x <- tidy(x)
  for (i in seq_along(tidy_x)) {
    df <- tidy_x[[i]]
    # Match occurrences and intensities
    for (j in seq_len(nrow(df))) {
      coded_occ <- df$occurrence[[j]]
      coded_int <- df$intensity[[j]]
      if (all(is.na(coded_occ))) {
        out[i, ] <- rep(
          ifelse(type == "numerical", NA_integer_, NA_character_),
          times = length(scheme$intensity)
        )
      } else {
        # Find the column index in the output matrix
        if (coded_occ %in% scheme$intensity) {
          if (type == "numerical") {
            coded_int <- match(coded_int, c("A", "B", "C", "D", "E"))
          }
          out[i, which(scheme$intensity == coded_occ)] <- coded_int
        }
      }
    }
  }
  # Return
  out
}

#' @export
asymmetry <- function(x, scheme) {
  # Validate input
  stopifnot(class(x) == "facs_coding")
  stopifnot(class(scheme) == "facs_scheme")
  stopifnot(length(scheme$asymmetry) > 0)
  # Initialize the output matrix
  out <- matrix(
    NA_character_,
    nrow = length(x),
    ncol = length(scheme$asymmetry)
  )
  colnames(out) <- autopad(scheme$asymmetry, prefix = "A")
  rownames(out) <- seq_along(x)
  # Populate the matrix
  tidy_x <- tidy(x)
  for (i in seq_along(tidy_x)) {
    df <- tidy_x[[i]]
    # Match occurrences and asymmetries
    for (j in seq_len(nrow(df))) {
      coded_occ <- df$occurrence[[j]]
      coded_sym <- df$asymmetry[[j]]
      if (all(is.na(coded_occ))) {
        out[i, ] <- rep(NA_character_, times = length(scheme$asymmetry))
      } else {
        # Find the column index in the output matrix
        if (coded_occ %in% scheme$asymmetry) {
          coded_sym <- ifelse(is.na(coded_sym), "S", coded_sym)
          out[i, which(scheme$asymmetry == coded_occ)] <- coded_sym
        }
      }
    }
  }
  # Return
  out
}

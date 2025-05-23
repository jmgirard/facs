# coding -----------------------------------------------------------------------
new_coding <- function(x = character()) {
  stopifnot(is.character(x))
  out <- structure(x, class = "facs_coding")
  out
}

validate_coding <- function(x, error = TRUE) {
  # Remove all whitespace characters
  x <- gsub("\\s+", "", x)
  # Capitalize all letters
  x <- toupper(x)
  # Check for unallowed characters
  ccx <- check_coding(x)
  if (!all(ccx)) {
    msg <- paste0(
      "The following elements of `x` are not valid FACS codes: ",
      paste(paste0('(', which(!ccx), ') "', x[!ccx], '"'), collapse = ", ")
    )
    if (error) {
      cli::cli_abort(msg)
    } else {
      cli::cli_alert_danger(msg)
      x[!ccx] <- NA_character_
    }
  }
  # Split on plus signs
  x <- strsplit(x, split = "+", fixed = TRUE)
  # Check for duplicated codes
  cdx <- check_duplicates(x)
  if (!all(cdx)) {
    msg <- paste0(
      "The following elements of `x` contain repeated FACS codes: ",
      paste(
        paste0(
          '(', which(!cdx), ') "', paste(x[!cdx][[1]], collapse = "+"), '"'
        ),
        collapse = ", "
      )
    )
    if (error) {
      cli::cli_abort(msg)
    } else {
      cli::cli_alert_danger(msg)
      x[!cdx] <- NA_character_
    }
  }
  # Check for exclusive codes
  cex <- check_exclusives(x)
  if (!all(cex)) {
    msg <- paste0(
      "The following elements of `x` contain mutually-exclusive FACS codes: ",
      paste(
        paste0(
          '(', which(!cex), ') "',
          sapply(x[!cex], function(x) paste(x, collapse = "+")),
          '"'
        ),
        collapse = ", "
      ),
      ". Check `facs_exclusives` for the full list of exclusive codes."
    )
    if (error) {
      cli::cli_abort(msg)
    } else {
      cli::cli_alert_danger(msg)
      x[!cex] <- NA_character_
    }
  }
  # Sort by numeric components
  x <- lapply(x, sort_by_numeric)
  # Recombine into string
  x <- lapply(x, function(z) paste(z, collapse = "+"))
  x <- unlist(x)
  x[x == "NA"] <- NA_character_
  # Return
  new_coding(x)
}

#' @export
coding <- function(x, error = TRUE) {
  validate_coding(x, error = error)
}

#' @export
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
  grepl(pattern, x) | is.na(x)
}

check_duplicates <- function(x) {
  sapply(x, function(x) {
    # Remove all letters from the string
    x <- gsub("[A-Z]", "", x)
    # Check for duplicates
    length(x) == length(unique(x))
  })
}

check_exclusives <- function(x) {
  sapply(x, function(vec) {
    !any(apply(facs_exclusives, MARGIN = 1, function(row) all(row %in% vec)))
  })
}

#' @method print facs_coding
#' @export
print.facs_coding <- function(x, ...) {
  print.default(unclass(x))
  cat("FACS Coding\n")
}

#' @method `[` facs_coding
#' @export
`[.facs_coding` <- function(x, i, ...) {
  result <- NextMethod("[")
  structure(result, class = class(x))
}

#' @method `[[` facs_coding
#' @export
`[[.facs_coding` <- function(x, i, ...) {
  result <- NextMethod("[[")
  structure(result, class = class(x))
}

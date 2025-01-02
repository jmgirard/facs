# scheme -----------------------------------------------------------------------
new_scheme <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "facs_scheme")
}

validate_scheme <- function(x) {
  # Check if x is a list
  if (!is.list(x)) {
    cli::cli_abort("`x` must be a list.")
  }
  # Check if the required elements exist in the list
  required_elements <- c("occurrence", "intensity", "asymmetry")
  missing_elements <- setdiff(required_elements, names(x))
  if (length(missing_elements) > 0) {
    cli::cli_abort(
      paste(
        "`x` is missing the following elements:", 
        paste(missing_elements, collapse = ", ")
      )
    )
  }
  # Check if the elements are numeric vectors
  for (element in required_elements) {
    if (!is.numeric(x[[element]]) || !is.vector(x[[element]])) {
      cli::cli_abort(paste0("`", element, "` must be a numeric vector."))
    }
  }
  # Check for any non-standard codes
  observed_codes <- unique(c(x$occurrence, x$intensity, x$asymmetry))
  unexpected_codes <- setdiff(observed_codes, facs_codes$number)
  if (length(unexpected_codes) > 0) {
    cli::cli_warn(
      paste(
        "The following codes are not part of the official FACS:",
        paste(unexpected_codes, collapse = ", ")
      )
    )
  }
  x
}


#' @export
scheme <- function(x = list()) {
  validate_scheme(new_scheme(x))
}

#' @method print facs_scheme
#' @export 
print.facs_scheme <- function(x, ...) {
  format_output <- function(y) {
    v <- x[[y]]
    if (length(v) > 0) {
      paste(v, collapse = ", ")
    } else {
      "None"
    }
  }
  cat(
    "A scheme object from the {facs} package:\n",
    "  Occurrence Coded = [", format_output("occurrence"), "]\n",
    "  Intensity Coded =  [", format_output("intensity"), "]\n",
    "  Asymmetry Coded =  [", format_output("asymmetry"), "]\n",
    sep = ""
  )
}

#' @method summary facs_scheme
#' @export
summary.facs_scheme <- function(object, ...) {
  # Get all unique values across all vectors
  unique_values <- unique(unlist(object))
  # Create a data frame with a "Code" column for unique values
  out <- data.frame(code = unique_values)
  # Add columns for each vector
  for (col in names(object)) {
    new <- col
    out[[col]] <- unique_values %in% object[[col]]
  }
  # Return
  print(out, row.names = FALSE)
}

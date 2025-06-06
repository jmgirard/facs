#' @export
compare_to_prototypes <- function(
  x,
  scheme,
  proto = prototypes,
  uncoded = c("warn", "error", "omit")
) {
  # Validate input
  stopifnot(length(x) == 1)
  stopifnot(class(x) == "facs_coding")
  stopifnot(class(scheme) == "facs_scheme")
  uncoded <- match.arg(uncoded, c("warn", "error", "omit"), FALSE)
  # Filter down to selected sources
  # Preallocate output vector
  out <- rep(NA_real_, times = nrow(proto))
  if (length(unique(proto$source) > 1)) {
    names(out) <- paste(
      proto$source,
      proto$emotion,
      proto$config_num,
      sep = "_"
    )
  } else {
    names(out) <- paste(
      proto$emotion,
      proto$config_num,
      sep = "_"
    )
  }
  # Tidy both sets of codes
  xo <- tidy(x)[[1]]$occurrence
  tidy_p <- tidy(coding(proto$code))
  # Check for prototype codes not in scheme
  proto_codes <- unlist(lapply(tidy_p, function(y) y$occurrence))
  proto_codes <- sort(as.integer(unique(proto_codes)))
  scheme_codes <- scheme$occurrence
  outside <- setdiff(proto_codes, scheme_codes)
  if (uncoded == "warn") {
    if (length(outside) > 0) {
      cli::cli_warn(
        paste0(
          "The following codes are present in one or more of the selected ",
          "prototypes but were not included in your coding scheme: [",
          paste(outside, collapse = ","),
          "]. This will cause some prototype match scores to be NA.\n"
        )
      )
    }
  } else if (uncoded == "error") {
    if (length(outside) > 0) {
      cli::cli_abort(
        paste0(
          "The following codes are present in one or more of the selected ",
          "prototypes but were not included in your coding scheme: [",
          paste(outside, collapse = ","),
          "].\n"
        )
      )
    }
  }
  # For each code in both sets...
  for (i in seq_along(tidy_p)) {
    # Extract occurrence
    po <- tidy_p[[i]]$occurrence
    if (uncoded == "omit") {
      po <- setdiff(po, as.character(outside))
    }
    if (all(po %in% scheme$occurrence)) {
      # Calculate dice coefficient
      out[[i]] <- (length(intersect(xo, po)) * 2) / length(c(xo, po))
    }
  }
  # Return
  out
}

#' @export
find_max <- function(x) {
  x[x == max(x, na.rm = TRUE)]
}

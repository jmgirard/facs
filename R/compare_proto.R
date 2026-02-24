#' Compare FACS Coding Objects to Emotion Prototypes
#'
#' How similar is this coding to each emotion prototype?
#'
#' @param x An object created by `coding()` to contain one or more FACS codings.
#' @param scheme An object created by `facs_scheme()` to indicate which AUs were
#'   coded and in which ways (i.e., occurrence, intensity, asymmetry).
#' @param proto A FUNCTION TO CREATE PROTOTYPES NEEDS TO BE MADE.
#' @param uncoded A string indicating how to handle the situation where a
#'   prototype or coding contains an AU that wasn't included in the scheme.
#'   "warn" provides a warning and an NA value, "error" provides an error and 
#'   stops, and "omit" drops the uncoded AU before calculating the Dice coefficient.
#' @param output A string indicating what to return. "all" returns the Dice
#'   coefficients for all prototypes, "exact" returns only the prototypes where
#'   the Dice coefficient equals 1.0, and "closest" returns only the prototypes
#'   with the maximum observed Dice coefficient.
#' @return A list the same length as `x` wherein each element contains a named
#'   vector containing zero or more prototypes and Dice coefficients quantifying
#'   their match to the coding in `x`.
#' @export
compare_to_prototypes <- function(
  x,
  scheme,
  proto = prototypes,
  uncoded = c("warn", "error", "omit"),
  output = c("all", "exact", "closest")
) {
  # Validate input
  stopifnot(class(x) == "facs_coding")
  stopifnot(class(scheme) == "facs_scheme")
  uncoded <- match.arg(uncoded, c("warn", "error", "omit"), FALSE)
  output <- match.arg(output, c("all", "exact", "closest"), FALSE)
  
  # Preallocate output vector
  out <- rep(NA_real_, times = nrow(proto))
  if (length(unique(proto$source)) > 1) {
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
  xlist <- occ_list(tidy(x))
  plist <- occ_list(tidy(coding(proto$code)))
  names(plist) <- names(out)
  
  # Check for codes not in scheme
  scheme_codes <- scheme$occurrence
  
  proto_codes <- sort(unique(unlist(plist)))
  outside_proto <- setdiff(proto_codes, scheme_codes)
  
  x_codes <- sort(unique(unlist(xlist)))
  outside_x <- setdiff(x_codes, scheme_codes)
  
  if (uncoded == "warn") {
    if (length(outside_proto) > 0) {
      cli::cli_warn(
        paste0(
          "The following codes are present in one or more of the selected ",
          "prototypes but were not included in your coding scheme: [",
          paste(outside_proto, collapse = ", "),
          "]. This will cause some prototype match scores to be NA.\n"
        )
      )
    }
    if (length(outside_x) > 0) {
      cli::cli_warn(
        paste0(
          "The following codes are present in `x` but were not included ",
          "in your coding scheme: [",
          paste(outside_x, collapse = ", "),
          "]. This will cause some prototype match scores to be NA.\n"
        )
      )
    }
  } else if (uncoded == "error") {
    if (length(outside_proto) > 0) {
      cli::cli_abort(
        paste0(
          "The following codes are present in one or more of the selected ",
          "prototypes but were not included in your coding scheme: [",
          paste(outside_proto, collapse = ", "),
          "].\n"
        )
      )
    }
    if (length(outside_x) > 0) {
      cli::cli_abort(
        paste0(
          "The following codes are present in `x` but were not included ",
          "in your coding scheme: [",
          paste(outside_x, collapse = ", "),
          "].\n"
        )
      )
    }
  } else if (uncoded == "omit") {
    plist <- lapply(plist, function(p) p[p %in% scheme_codes])
    xlist <- lapply(xlist, function(xi) xi[xi %in% scheme_codes])
  }
  
  # Loop through xlist...
  out <- lapply(xlist, function(xi) {
    # Loop through plist...
    sapply(plist, function(pi) {
      # Calculate Dice coefficient
      (length(intersect(xi, pi)) * 2) / length(c(xi, pi))
    })
  })
  
  # Reduce output if requested
  if (output == "exact") {
    out <- lapply(out, function(xi) xi[xi == 1])
  } else if (output == "closest") {
    out <- lapply(out, function(xi) xi[xi == max(xi, na.rm = TRUE)])
  }
  
  # Return
  out
}

# Convert
occ_list <- function(x) {
  lapply(x, function(df) as.integer(df[["occurrence"]]))
}

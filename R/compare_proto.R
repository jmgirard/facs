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
  stopifnot(class(x) == "facs_coding")
  stopifnot(class(scheme) == "facs_scheme")
  uncoded <- match.arg(uncoded, c("warn", "error", "omit"), FALSE)
  output  <- match.arg(output,  c("all", "exact", "closest"), FALSE)

  # Tidy both sets of codes
  xlist <- occ_list(tidy(x))
  plist <- occ_list(tidy(coding(proto$code)))
  names(plist) <- if (length(unique(proto$source)) > 1) {
    paste(proto$source, proto$emotion, proto$config_num, sep = "_")
  } else {
    paste(proto$emotion, proto$config_num, sep = "_")
  }

  # Handle prototype codes not in the scheme
  scheme_codes <- scheme$occurrence
  outside <- setdiff(sort(unique(unlist(plist))), scheme_codes)
  if (uncoded == "warn" && length(outside) > 0) {
    cli::cli_warn(paste0(
      "These codes appear in prototypes but not your scheme: [",
      paste(outside, collapse = ","), "]. Affected match scores will be NA.\n"
    ))
  } else if (uncoded == "error" && length(outside) > 0) {
    cli::cli_abort(paste0(
      "These codes appear in prototypes but not your scheme: [",
      paste(outside, collapse = ","), "].\n"
    ))
  } else if (uncoded == "omit") {
    plist <- lapply(plist, function(p) p[p %in% scheme_codes])
    empty <- lengths(plist) == 0
    if (any(empty)) {
      cli::cli_inform(paste0(
        "Dropped ", sum(empty), " prototype(s) with no in-scheme AUs ",
        "(unassessable in this scheme): [",
        paste(names(plist)[empty], collapse = ", "), "].\n"
      ))
      plist <- plist[!empty]
    }
  }

  # Dice coefficient of each observed coding vs each prototype
  out <- lapply(xlist, function(xi) {
    vapply(plist, function(pi) {
      denom <- length(xi) + length(pi)
      if (denom == 0) NA_real_ else (length(intersect(xi, pi)) * 2) / denom
    }, numeric(1))
  })

  # Reduce output if requested
  if (output == "exact") {
    out <- lapply(out, function(xi) xi[!is.na(xi) & xi == 1])
  } else if (output == "closest") {
    out <- lapply(out, function(xi) {
      m <- suppressWarnings(max(xi, na.rm = TRUE))
      if (!is.finite(m) || m == 0) xi[0] else xi[!is.na(xi) & xi == m]
    })
  }
  out
}

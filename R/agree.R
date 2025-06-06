#' @export
agree_description <- function(
  ...,
  scheme,
  event_names = NULL,
  coder_names = NULL
) {
  occ_rel <- prep_occ_reliability(
    ...,
    scheme = scheme,
    event_names = event_names,
    coder_names = coder_names
  )
  new_agree(
    overall = agree_overall(occ_rel),
    per_event = agree_per_event(occ_rel),
    per_code = agree_per_code(occ_rel),
    per_pair = agree_per_pair(occ_rel),
    drop_one = agree_drop_one(occ_rel),
    scheme = scheme
  )
}

prep_occ_reliability <- function(
  ...,
  scheme,
  event_names = NULL,
  coder_names = NULL
) {
  inputs <- list(...)
  if (is.null(event_names)) {
    event_names <- autopad(seq_along(inputs[[1]]), prefix = "E")
  }
  if (is.null(coder_names)) {
    coder_names <- autopad(seq_along(inputs), prefix = "C")
  }
  allocc <- lapply(inputs, function(x) occurrence(x, scheme))
  out <- simplify2array(allocc)
  dimnames(out)[c(1, 3)] <- list(event_names, coder_names)
  out
}

agree_per_event <- function(occ_rel) {
  n_events <- dim(occ_rel)[[1]]
  out <- vector(mode = "double", length = n_events)
  names(out) <- dimnames(occ_rel)[[1]]
  for (i in seq_len(n_events)) {
    # Create analysis matrix explicitly to avoid drop errors
    code_coder_matrix <- matrix(
      occ_rel[i, , , drop = FALSE],
      nrow = dim(occ_rel)[[2]],
      ncol = dim(occ_rel)[[3]],
      dimnames = list(
        dimnames(occ_rel)[[2]],
        dimnames(occ_rel)[[3]]
      )
    )
    out[[i]] <- calc_specific_agreement(code_coder_matrix, category = 1)
  }
  out
}

agree_per_code <- function(occ_rel) {
  n_codes <- dim(occ_rel)[[2]]
  out <- vector(mode = "double", length = n_codes)
  names(out) <- dimnames(occ_rel)[[2]]
  for (i in seq_along(out)) {
    # Create analysis matrix explicitly to avoid drop errors
    event_coder_matrix <- matrix(
      occ_rel[, i, , drop = FALSE],
      nrow = dim(occ_rel)[[1]],
      ncol = dim(occ_rel)[[3]],
      dimnames = list(
        dimnames(occ_rel)[[1]],
        dimnames(occ_rel)[[3]]
      )
    )
    out[[i]] <- calc_specific_agreement(event_coder_matrix, category = 1)
  }
  out[is.nan(out)] <- NA_real_
  out
}

agree_per_pair <- function(occ_rel) {
  pairs <- utils::combn(dimnames(occ_rel)[[3]], m = 2)
  n_pairs <- ncol(pairs)
  out <- vector(mode = "double", length = n_pairs)
  names(out) <- apply(pairs, MARGIN = 2, function(x) paste(x, collapse = "_"))
  for (i in seq_along(out)) {
    out[[i]] <- agree_overall(occ_rel[, , pairs[, i], drop = FALSE])
  }
  out
}

agree_drop_one <- function(occ_rel) {
  n_coders <- dim(occ_rel)[[3]]
  out <- rep(NA_real_, length = n_coders)
  names(out) <- paste0("drop_", dimnames(occ_rel)[[3]])
  if (n_coders < 3) return(out)
  for (i in seq_along(out)) {
    out[[i]] <- agree_overall(occ_rel[, , -i, drop = FALSE])
  }
  out
}

agree_overall <- function(occ_rel) {
  mean(agree_per_event(occ_rel), na.rm = TRUE)
}

# agree ------------------------------------------------------------------------
new_agree <- function(
  overall = numeric(),
  per_event = numeric(),
  per_code = numeric(),
  per_pair = numeric(),
  drop_one = numeric(),
  scheme = list()
) {
  stopifnot(is.numeric(overall))
  stopifnot(is.numeric(per_event))
  stopifnot(is.numeric(per_code))
  stopifnot(is.numeric(per_pair))
  stopifnot(is.numeric(drop_one))
  stopifnot(is.list(scheme), class(scheme) == "facs_scheme")
  out <- structure(
    list(
      overall = overall,
      per_event = per_event,
      per_code = per_code,
      per_pair = per_pair,
      drop_one = drop_one,
      scheme = scheme
    ),
    class = "facs_agree"
  )
  out
}

#' @method print facs_agree
#' @export
print.facs_agree <- function(x, ...) {
  print.default(x$overall)
}

#' @method summary facs_agree
#' @export
summary.facs_agree <- function(object, digits = 3, ...) {
  cat(
    "# Counts\n",
    "Events = ", length(object$per_event), "\n",
    "Codes  = ", length(object$per_code), "\n",
    "Coders = ", length(object$drop_one), "\n",
    sep = ""
  )
  cat("\n# Overall", round(object$overall, digits), sep = "\n")
  cat("\n# Per Event\n")
  print(object$per_event, digits = digits)
  cat("\n# Per Code\n")
  print(object$per_code[!is.na(object$per_code)], digits = digits)
  cat("\n# Per Pair\n")
  print(object$per_pair, digits = digits)
  cat("\n# Drop One\n")
  print(object$drop_one, digits = digits)
}

#' Calculate specific agreement
#'
#' Calculate category-specific agreement using a generalized formula that can
#' accommodate missing data and any number of coders. With two raters, the
#' interpretation of specific agreement for any category is the probability of
#' one rater assigning an item to that category given that the other rater has
#' also assigned that item to that category. With more than two raters, the
#' interpretation becomes the probability of a randomly chosen rater assigning
#' an item to that category given that another randomly chosen rater has also
#' assigned that item to that category.
#'
#' \deqn{A_k=\frac{\sum_{i=1}^{n'}r_{ik}(r_{ik}-1)}{\sum_{i=1}^{n'}r_{ik}(r_i-1)}}
#' \eqn{n'} is the number of objects that were assigned to any category by
#'   two or more coders<br>
#' \eqn{r_{ik}} is the number of coders that assigned object \eqn{i} to category
#'   \eqn{k}<br>
#' \eqn{r_i} is the number of coders that assigned object \eqn{i} to any category
#'
#' @param mat A matrix of coding where each row is an object of coding (i.e., an
#'   action unit when doing description coding or a temporal bin when doing
#'   location coding) and each column is a source of coding (e.g., human coder).
#' @param category A scalar (e.g., number, string, or boolean) that represents
#'   which of the possible coding categories agreement should be calculated on.
#'   Defaults to `1` for positive agreement during occurrence coding.
#' @return A scalar double that is the specific agreement estimate from 0 to 1.
#' @export
calc_specific_agreement <- function(mat, category = 1) {
  # Validate inputs
  stopifnot(is.matrix(mat))
  stopifnot(length(category) == 1 && !is.na(category))
  # Select those objects with two or more codings
  mat_p <- mat[rowSums(!is.na(mat)) >= 2, , drop = FALSE]
  # Count the number of coders assigning each object to the selected category
  r_ik <- rowSums(mat_p == category, na.rm = TRUE)
  # Count the number of coders assigning each object to any category
  r_i <- rowSums(!is.na(mat_p))
  # Calculate specific agreement
  sum(r_ik * (r_ik - 1)) / sum(r_ik * (r_i - 1))
}

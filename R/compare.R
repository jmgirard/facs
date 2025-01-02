#' @export
compare_occurrence <- function(x, y) {
  length(intersect(occ(x), occ(y))) * 2 / length(c(occ(x), occ(y)))
}

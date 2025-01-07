autopad <- function(x, prefix = "") {
  sprintf(paste0(prefix, "%0", max(nchar(x)), "d"), x)
}

## code to prepare `fig2002` dataset goes here
x <- read.csv("data-raw/fig2002_raw.csv")
fig2002 <- dplyr::mutate(x, .before = 1, source = "fig2002")
usethis::use_data(fig2002, overwrite = TRUE)

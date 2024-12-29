## code to prepare `keltner2019` dataset goes here
x <- read.csv("data-raw/keltner2019_raw.csv")
keltner2019 <- dplyr::mutate(x, .before = 1, source = "keltner2019")
usethis::use_data(keltner2019, overwrite = TRUE)

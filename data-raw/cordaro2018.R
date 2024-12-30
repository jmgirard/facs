## code to prepare `cordaro2018` dataset goes here
x <- read.csv("data-raw/cordaro2018_raw.csv")
cordaro2018 <- dplyr::mutate(x, .before = 1, source = "cordaro2018")
usethis::use_data(cordaro2018, overwrite = TRUE)

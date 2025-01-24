## code to prepare `keltner2019` dataset goes here
keltner2019 <-
  read.csv("data-raw/keltner2019_raw.csv") |>
  dplyr::rename(code = required) |>
  dplyr::mutate(source = "keltner2019", .before = 1) |>
  dplyr::mutate(code = coding(code, error = FALSE)) |> 
  tidyr::drop_na(code) |> 
  dplyr::mutate(
    .by = emotion,
    .after = code,
    config_num = dplyr::row_number(),
    config_type = "prototype"
  )
usethis::use_data(keltner2019, overwrite = TRUE)

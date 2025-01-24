## code to prepare `cordaro2018` dataset goes here
cordaro2018 <-
  read.csv("data-raw/cordaro2018_raw.csv") |>
  dplyr::mutate(
    required2 = expand_strings(required, delim = "+"),
    alternatives2 = expand_strings(alternatives, delim = ","),
    code = purrr::map2(
      .x = required2,
      .y = alternatives2,
      expand_alternatives
    )
  ) |>
  tidyr::unnest(cols = code) |>
  dplyr::select(emotion, code, config_type, special) |>
  dplyr::mutate(source = "cordaro2018", .before = 1) |>
  dplyr::mutate(code = coding(code, error = FALSE)) |> 
  tidyr::drop_na(code) |> 
  dplyr::mutate(
    .by = emotion,
    .before = config_type,
    config_num = dplyr::row_number()
  )
usethis::use_data(cordaro2018, overwrite = TRUE)

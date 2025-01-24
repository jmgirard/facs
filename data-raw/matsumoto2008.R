## code to prepare `matsumoto2008` dataset goes here
matsumoto2008 <-
  read.csv("data-raw/matsumoto2008_raw.csv") |>
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
  dplyr::select(emotion, code, special) |>
  dplyr::mutate(source = "matsumoto2008", .before = 1) |>
  dplyr::mutate(code = coding(code, error = FALSE)) |> 
  tidyr::drop_na(code) |> 
  dplyr::mutate(
    .by = emotion,
    .after = code,
    config_num = dplyr::row_number(),
    config_type = "prototype"
  )
usethis::use_data(matsumoto2008, overwrite = TRUE)

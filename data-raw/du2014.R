## code to prepare `du2014` dataset goes here
du2014 <-
  read.csv("data-raw/du2014_raw.csv") |>
  dplyr::mutate(
    required2 = expand_strings(required, delim = ","),
    optional2 = expand_strings(optional, delim = ","),
    code = purrr::map2(
      .x = required2,
      .y = optional2,
      expand_optional
    )
  ) |>
  tidyr::unnest(cols = code) |>
  dplyr::select(emotion, emo_type, code) |>
  dplyr::mutate(source = "du2014", .before = 1) |>
  dplyr::mutate(
    .by = emotion,
    config_num = dplyr::row_number(),
    config_type = ifelse(config_num == 1, "prototype", "variant")
  )
usethis::use_data(du2014, overwrite = TRUE)

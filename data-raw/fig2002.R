## code to prepare `fig2002` dataset goes here
fig2002 <-
  read.csv("data-raw/fig2002_raw.csv") |>
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
  dplyr::mutate(source = "fig2002", .before = 1) |>
  dplyr::mutate(
    .by = emotion,
    .before = config_type,
    config_num = dplyr::row_number()
  )
usethis::use_data(fig2002, overwrite = TRUE)

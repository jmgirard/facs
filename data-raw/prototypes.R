## code to prepare `prototypes` dataset goes here
prototypes <-
  dplyr::bind_rows(
    cordaro2018,
    du2014,
    fig2002,
    keltner2019,
    matsumoto2008
  ) |>
  dplyr::select(
    source,
    emotion,
    emotion_type,
    config_num,
    config_type,
    code,
    special
  ) |>
  dplyr::mutate(special = dplyr::na_if(special, ""))
usethis::use_data(prototypes, overwrite = TRUE)

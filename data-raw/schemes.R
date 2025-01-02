# All codes, all information
scheme_all_oia <- 
  list(
    occurrence = facs_codes$number,
    intensity  = facs_codes$number,
    asymmetry  = facs_codes$number
  ) |> 
  new_scheme() |> 
  validate_scheme()
usethis::use_data(scheme_all_oia, overwrite = TRUE)
scheme_full <- scheme_all_oia
usethis::use_data(scheme_full, overwrite = TRUE)


# All codes, occurrence and intensity only
scheme_all_oi <- 
  list(
    occurrence = facs_codes$number,
    intensity  = facs_codes$number,
    asymmetry  = numeric()
  ) |> 
  new_scheme() |> 
  validate_scheme()
usethis::use_data(scheme_all_oi, overwrite = TRUE)

# All codes, occurrence only
scheme_all_o <- 
  list(
    occurrence = facs_codes$number,
    intensity  = numeric(),
    asymmetry  = numeric()
  ) |> 
  new_scheme() |> 
  validate_scheme()
usethis::use_data(scheme_occ, overwrite = TRUE)

# AUs only, all information
scheme_aus_oia <- 
  list(
    occurrence = facs_codes$number[facs_codes$type == "AU"],
    intensity  = facs_codes$number[facs_codes$type == "AU"],
    asymmetry  = facs_codes$number[facs_codes$type == "AU"]
  ) |> 
  new_scheme() |> 
  validate_scheme()
usethis::use_data(scheme_aus_oia, overwrite = TRUE)

# AUs only, occurrence and intensity only
scheme_aus_oi <- 
  list(
    occurrence = facs_codes$number[facs_codes$type == "AU"],
    intensity  = facs_codes$number[facs_codes$type == "AU"],
    asymmetry  = numeric()
  ) |> 
  new_scheme() |> 
  validate_scheme()
usethis::use_data(scheme_aus_oi, overwrite = TRUE)

# AUs only, occurrence only
scheme_aus_o <- 
  list(
    occurrence = facs_codes$number[facs_codes$type == "AU"],
    intensity  = numeric(),
    asymmetry  = numeric()
  ) |> 
  new_scheme() |> 
  validate_scheme()
usethis::use_data(scheme_aus_o, overwrite = TRUE)

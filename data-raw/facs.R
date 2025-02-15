facs_codes <- read.csv("data-raw/facs_codes.csv")
usethis::use_data(facs_codes, overwrite = TRUE)

facs_prefixes <- c(
  "U",  # unilateral (generic)
  "L",  # unilateral: left only
  "R",  # unilateral: right only
  "T",  # unilateral: top only
  "B",  # unilateral: bottom only
  "S",  # symmetrical
  "A",  # asymmetrical (generic)
  "V",  # asymmetrical: left greater by 1
  "W",  # asymmetrical: left greater by 2
  "X",  # asymmetrical: left greater by 3
  "Y",  # asymmetrical: left greater by 4
  "Z",  # asymmetrical: left greater by 5
  "G",  # asymmetrical: right greater by 1
  "H",  # asymmetrical: right greater by 2
  "I",  # asymmetrical: right greater by 3
  "J",  # asymmetrical: right greater by 4
  "K",  # asymmetrical: right greater by 5
  "M"   # movement
)
usethis::use_data(facs_prefixes, overwrite = TRUE)

facs_insides <- c(
  facs_codes$number, # numerical codes
  "\\+" # combination delimiter
)
usethis::use_data(facs_insides, overwrite = TRUE)

facs_suffixes <- c(
  "A", # intensity 1: trace
  "B", # intensity 2: slight
  "C", # intensity 3: marked or pronounced
  "D", # intensity 4: severe or extreme
  "E"  # intensity 5: maximum
)
usethis::use_data(facs_suffixes, overwrite = TRUE)

facs_characters <- paste0(
  c(sort(unique(c(facs_prefixes, facs_suffixes))), "0-9\\+"), collapse = ""
)
usethis::use_data(facs_characters, overwrite = TRUE)

facs_exclusives <- read.csv("data-raw/facs_exclusives.csv")
usethis::use_data(facs_exclusives, overwrite = TRUE)

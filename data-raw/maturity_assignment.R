file <- system.file("extdata", "maturity_assignment.csv",
  package = "gfplot"
)

mat_df <- readr::read_csv(file,
  col_types = readr::cols(
    maturity_convention_code = readr::col_integer(),
    maturity_convention_desc = readr::col_character(),
    sex = readr::col_integer(),
    mature_at = readr::col_integer()
  )
)

maturity_assignment <- mat_df
usethis::use_data(maturity_assignment, overwrite = TRUE)

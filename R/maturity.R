library(dplyr)
dbio <- readRDS("data-cache/all-survey-bio.rds")
dbio <- dbio[!duplicated(dbio$specimen_id), ]
dbio <- dbio %>%
  select(species_common_name, species_science_name,
    year, age, length, weight,
    maturity_code, sex, survey_series_desc,
    maturity_convention_desc, maturity_convention_maxvalue)

mat_df <- tibble::tribble(
  ~maturity_convention_desc, ~mature_at,
  "ROCKFISH (1977+)",        3,
  "DOGFISH",                 77,
  "FLATFISH (1978+)",        3
)

dbio <- left_join(dbio, mat_df)
dbio <- mutate(dbio, mature = maturity_code >= mature_at)

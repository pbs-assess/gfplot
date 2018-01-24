library(dplyr)
library(rio)

dbio <- readRDS("data-cache/all-survey-bio.rds")
dbio <- dbio[!duplicated(dbio$specimen_id), ]
dbio <- dbio %>%
  select(species_common_name, species_science_name,
         year, age, length, weight,
         maturity_code, sex, survey_series_desc,
         maturity_convention_desc, maturity_convention_maxvalue)

mat_df <- import("data/maturity_assignment.csv") %>%
  rename(sex = specimen_sex_code, maturity_convention_desc = maturity_convention_description)

dbio <- left_join(dbio, mat_df)
dbio <- mutate(dbio, mature = maturity_code >= mature_at)

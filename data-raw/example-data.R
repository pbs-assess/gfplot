library(dplyr)
# d <- get_survey_samples("pacific ocean perch")
d <- readRDS("../gfsynopsis/report/data-cache2/pbs-survey-samples.rds")
d <- dplyr::filter(d, species_common_name == "pacific ocean perch",
  survey_abbrev == "SYN QCS", year > 2010, year < 2018)
d <- d[!duplicated(select(d, year, specimen_id)), , drop = FALSE]
d <- filter(
  d, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$weight),
  !is.na(.data$age), !is.na(maturity_code)
)
# d <- select(
#   d, survey_series_desc, sex, age, length, weight, maturity_code,
#   species_common_name, year, sample_id, maturity_convention_maxvalue,
#   maturity_code, maturity_convention_desc, specimen_id, trip_start_date
# )
set.seed(42)
# d <- d[sample(seq_len(nrow(d)), 3000L), , drop = FALSE]
names(d)
nrow(d)
pop_samples <- d
usethis::use_data(pop_samples, overwrite = TRUE)

d <- readRDS("../gfsynopsis/report/data-cache2/pbs-survey-sets.rds")
d <- dplyr::filter(d, species_common_name == "pacific ocean perch",
  survey_abbrev == "SYN QCS", year == 2015)
pop_surv <- d
usethis::use_data(pop_surv, overwrite = TRUE)

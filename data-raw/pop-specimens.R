# d <- get_survsamples("pacific ocean perch")
d <- readRDS("data-cache/pbs-survey-specimens.rds")
d <- dplyr::filter(d, species_common_name == "pacific ocean perch")
d <- filter(d, year > 2010, year < 2012)
d <- d[!duplicated(d), , drop = FALSE]
d <- filter(d, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$weight),
  !is.na(.data$age))
d <- select(d, survey_series_desc, sex, age, length, weight, maturity_code,
  species_common_name, year, sample_id, maturity_convention_maxvalue,
  maturity_code)
set.seed(42)
d <- d[sample(seq_len(nrow(d)), 1000L), , drop = FALSE]
names(d)
nrow(d)
pop_samples <- d
usethis::use_data(pop_samples, overwrite = TRUE)

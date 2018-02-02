prep_pbs_ageing_precision <- function(dat, species) {
  # dbio <- dat
  # dbio <- readRDS(file.path(path, "pbs-aging-precision.rds"))

  # remove specimen id's for which there is no precision reading
  dbio <- group_by(dbio, specimen_id, species_code) %>%
    mutate(has_precision = 3 %in% age_reading_type_code) %>%
    filter(has_precision) %>% select(-has_precision)

  # organize dataframe with one record for each specimen id, age reading type and age parameter
  dbio <- gather(dbio, ageing_param, age, -(specimen_id:ageing_method_desc),
      -employee_id, -age_reading_id) %>%
      arrange(age_reading_id) %>%
    group_by(specimen_id, age_reading_type_code, year, species_code,
      ageing_param) %>%
    summarize(age = age[[1]], employee_id = employee_id[[1]])

  # remove bad data
  precision_aged_by_same <- group_by(dbio, specimen_id) %>%
    summarize(n_employee = length(unique(employee_id))) %>%
    filter(n_employee < 2) %>%
    pull(specimen_id)
  dbio <- dbio %>%
    filter(!specimen_id %in% precision_aged_by_same)

  # organize data into individual columns for aging parameter + age reading type
  dbio <- dbio %>%
    unite(temp, age_reading_type_code, ageing_param) %>%
    reshape2::dcast(specimen_id + year + species_code ~ temp, value.var = "age")
  names(dbio5) <- sub("2", "primary", names(dbio5))
  names(dbio5) <- sub("3", "precision", names(dbio5))
}




#' Prepare PBS ageing precision data
#'
#' @param dat A data frame from \code{\link{get_pbs_ageing_precision}}
#' @export
prep_pbs_ageing_precision <- function(dat) {

  # remove specimen id's for which there is no precision reading
  dbio <- group_by(dbio, specimen_id, species_code) %>%
    mutate(has_precision = 3 %in% age_reading_type_code) %>%
    filter(has_precision) %>% select(-has_precision)

  # organize dataframe with one record for each specimen id, age reading type and age parameter
  dbio <- tidyr::gather(dbio, ageing_param, age, -(specimen_id:ageing_method_desc),
      -employee_id, -age_reading_id) %>%
      arrange(age_reading_id) %>%
    group_by(specimen_id, age_reading_type_code, year, species_code,
      ageing_param) %>%
    summarise(age = age[[1]], employee_id = employee_id[[1]])

  # remove bad data
  precision_aged_by_same <- group_by(dbio, specimen_id) %>%
    summarise(n_employee = length(unique(employee_id))) %>%
    filter(n_employee < 2) %>%
    dplyr::pull(specimen_id)
  dbio <- dbio %>%
    filter(!specimen_id %in% precision_aged_by_same)

  # organize data into individual columns for aging parameter + age reading type
  dbio <- dbio %>%
    mutate(temp = paste(age_reading_type_code, ageing_param, sep = "_")) %>%
    reshape2::dcast(specimen_id + year + species_code ~ temp, value.var = "age")
  names(dbio) <- sub("2", "primary", names(dbio))
  names(dbio) <- sub("3", "precision", names(dbio))
  dbio
}


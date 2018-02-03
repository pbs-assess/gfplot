#' Prepare PBS ageing precision data
#'
#' @param dat A data frame from \code{\link{get_pbs_ageing_precision}}
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_pbs_ageing_precision("pacific ocean perch")
#' prep_pbs_ageing_precision(d)
#' }
prep_pbs_ageing_precision <- function(dat) {

  # dbio <- readRDS(file.path(path, "all-survey-bio.rds"))

  # remove specimen id's for which there is no precision reading
  dbio <- group_by(dat, specimen_id, species_code) %>%
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
  ageing_prec <- dbio %>%
    mutate(temp = paste(age_reading_type_code, ageing_param, sep = "_")) %>%
    reshape2::dcast(specimen_id + year + species_code ~ temp, value.var = "age")
  names(ageing_prec) <- sub("2", "prim", names(ageing_prec))
  names(ageing_prec) <- sub("3", "prec", names(ageing_prec))
  names(ageing_prec) <- sub("maximum", "max", names(ageing_prec))
  names(ageing_prec) <- sub("minimum", "min", names(ageing_prec))
  names(ageing_prec) <- sub("specimen_", "", names(ageing_prec))

  ageing_prec
}

#' Plot pbs ageing precision data
#'
#' @param dat TODO
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_pbs_ageing_precision("pacific ocean perch")
#' d <- prep_pbs_ageing_precision(d)
#' plot_ageing_precision(d)
#' }
plot_ageing_precision <- function(dat) {
    ggplot(dat, aes_string("prim_age", "prec_age")) +
    geom_point(pch = 19, colour = "grey10", size = 1.2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, col = "grey50", lty = 2) +
    ggplot2::geom_segment(aes_string(x = "prim_min_age", xend = "prim_min_age",
      y = "prec_age", yend = "prec_age"), alpha = 0.6) +
    ggplot2::geom_segment(aes_string(x = "prim_age", xend = "prim_age",
      y = "prec_min_age", yend = "prec_max_age"), alpha = 0.6) +
    labs(title = "Ageing Precision", x = "Primary Age", y = "Precision Age") +
    theme_pbs()
}

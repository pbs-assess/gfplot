# Bring in ageing precision data
# source("R/make-spp-list.R")
# species <- get_spp_names()$species_common_name
#


# to test

dbio <- get_pbs_ageing_precision("pacific ocean perch")
b <- prep_pbs_ageing_precision(dbio)

#' Prepare PBS ageing precision data
#'
#' @param dat A data frame from \code{\link{get_pbs_ageing_precision}}
#' @export
prep_pbs_ageing_precision <- function(dat) {

  # dbio <- readRDS(file.path(path, "all-survey-bio.rds"))

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
  ageing_prec <- dbio %>%
    mutate(temp = paste(age_reading_type_code, ageing_param, sep = "_")) %>%
    reshape2::dcast(specimen_id + year + species_code ~ temp, value.var = "age")
  names(ageing_prec) <- sub("2", "prim", names(ageing_prec))
  names(ageing_prec) <- sub("3", "prec", names(ageing_prec))
  names(ageing_prec) <- sub("maximum", "max", names(ageing_prec))
  names(ageing_prec) <- sub("minimum", "min", names(ageing_prec))
  names(ageing_prec) <- sub("specimen_", "", names(ageing_prec))
}



#' Plot pbs ageing precision data
#'
#' @param dat
#'
#' @export
#'
plot_ageing_prec <- function(dat) {
# [ageing_prec$species_code == common2codes(species),]

a <- ageing_prec
a %>%
  ggplot(aes(prim_age, prec_age)) +
  geom_point(pch = 19, colour = "grey10", size = 1.2) +
  stat_smooth(method="lm", color = "grey10", se = FALSE, size = 0.72) +
  theme_pbs() +
  geom_linerange(ymin = a$prec_min_age, ymax = a$prec_max_age) +
  ggstance::geom_linerangeh(xmin = a$prim_min_age, xmax = a$prim_max_age) +
  labs(title = "Ageing Precision", x = "Primary Age", y = "Precision Age") +
  theme(plot.title = element_text(hjust = 0.5))
}





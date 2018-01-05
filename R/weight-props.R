join_comps_commercial <- function(dat, catch_dat) {
  library(dplyr)

  dat <- mutate(dat, month = lubridate::month(trip_start_date),
    quarter = case_when(
      month %in% seq(1, 3) ~ 1,
      month %in% seq(4, 6) ~ 2,
      month %in% seq(7, 9) ~ 3,
      month %in% seq(10, 12) ~ 4
    )) %>% select(-month)

  catch_dat <- filter(all_catch, !is.na(fe_end_date)) %>%
    mutate(month = lubridate::month(fe_end_date),
      quarter = case_when(
        month %in% seq(1, 3) ~ 1,
        month %in% seq(4, 6) ~ 2,
        month %in% seq(7, 9) ~ 3,
        month %in% seq(10, 12) ~ 4
      )) %>% select(-month)

  sampled_trip_id_catch <-
    unique(select(dat, year, quarter, trip_id, catch_weight)) %>%
    group_by(year, quarter, trip_id) %>%
    summarise(samp_trip_catch_weight = sum(catch_weight))

  quarter_sampled_catch <- sampled_trip_id_catch %>%
    group_by(year, quarter) %>%
    summarise(samp_catch_weight_quarter = sum(samp_trip_catch_weight))

  freq_and_catch_by_trip <- dat %>% group_by(year, trip_id, quarter, age) %>%
    summarise(freq = n()) %>%
    inner_join(sampled_trip_id_catch, by = c("year", "trip_id", "quarter"))

  species_catch_by_quarter <- group_by(catch_dat, year, quarter) %>%
    summarise(landed_kg_quarter = sum(landed_kg)) %>%
    group_by(year) %>%
    mutate(landed_kg_year = sum(landed_kg_quarter))

  inner_join(freq_and_catch_by_trip,
    species_catch_by_quarter, by = c("year", "quarter")) %>%
    inner_join(quarter_sampled_catch, by = c("year", "quarter")) %>%
    select(year, trip_id, quarter, age, freq,
      samp_trip_catch_weight, samp_catch_weight_quarter,
      landed_kg_quarter, landed_kg_year) %>% # re-order
    arrange(year, trip_id, age)
}


join_comps_strata <- function(dat, strat_dat) {
  library(dplyr)

  raw_comp <- dat %>%
    select(year, sample_id, age, weight, grouping_code) %>%
    group_by(year, sample_id, grouping_code, age) %>%
    summarise(freq = n())

  strat_areas <- select(strat_dat, year, grouping_code, area_km2) %>%
    unique() %>%
    group_by(year) %>%
    mutate(total_area_km2 = sum(area_km2))

  strat_dens <- select(strat_dat, year, fishing_event_id,
    grouping_code, density_kgpm2) %>%
    unique() %>%
    group_by(year, grouping_code) %>%
    summarise(total_density = sum(density_kgpm2*1e6))

  sample_dens <- select(dat, -area_km2) %>%
    inner_join(strat_dat,
      by = c("year", "survey_id", "sample_id", "grouping_code")) %>%
    group_by(year, grouping_code, sample_id) %>%
    summarise(density = mean(density_kgpm2*1e6))

  inner_join(raw_comp, sample_dens,
    by = c("year", "sample_id", "grouping_code")) %>%
    inner_join(strat_dens, by = c("year", "grouping_code")) %>%
    inner_join(strat_areas, by = c("year", "grouping_code")) %>%
    ungroup()
}

weight_comps <- function(dat) {
  library(dplyr)
  library(rlang)

  group_by(dat, year, grouping_code) %>% # pre D.4 / # quarter

    # first level (within quarters by catch or within strata by survey catch):
    mutate(density_prop = density/total_density) %>% # D.4
    # re-weight:
    group_by(year, grouping_code, area_km2, total_area_km2, age) %>% # pre D.5
    summarise(weighted_freq1 = sum(freq * density_prop), # D.5
      sum_freq = sum(freq)) %>% # needed for D.6

    # re-standardize:
    mutate(weighted_freq1_scaled =
        weighted_freq1 * sum(sum_freq)/sum(weighted_freq1)) %>%  # D.6
    group_by(year) %>% # pre D.7

    # second level (within years by catch or within survey-years by area):
    mutate(area_annual_prop = area_km2/sum(total_area_km2)) %>% # D.7
    group_by(year, age) %>% # pre D.8

    # re-weight:
    summarise(
      weighted_freq2 = sum(weighted_freq1_scaled * area_annual_prop), # D.8
      sum_weighted_freq1 = sum(weighted_freq1_scaled)) %>% # needed for D.9
    group_by(year) %>% # pre D.9

    # re-standardize:
    mutate(weighted_freq2_scaled = weighted_freq2 *
        (sum(sum_weighted_freq1) / sum(weighted_freq2))) %>% # D.9
    group_by(year) %>%

    # calculate proportions:
    mutate(weighted_age_prop =
        weighted_freq2_scaled / sum(weighted_freq2_scaled)) %>% # D.10
    select(-contains("freq"))
}



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


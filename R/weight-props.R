
weight_proportions <- function(dat, strat_dat) {

  # Plan:
  # 1. generate the catch per year-quarter / density per year-stratum data.frame
  # 2. generate the catch per year / area per stratum data.frame
  # 3. join these data.frames with the main data.frame at the 2 weighting stages

  browser()
  library(magrittr)

  dd <- left_join(dat, strat_dat, by = c("year", "grouping_code")) %>%
    select(year, sample_id, age, weight, grouping_code,
      total_density, area) %>%
    filter(!is.na(age)) %>%
    group_by(year, grouping_code, age, area, total_density) %>%
    summarise(n = n())

  strat_areas <- group_by(strat_dat, year) %>%
    summarise(total_area = sum(area)) %>% ungroup()
  strat_dens <- group_by(strat_dat, year) %>%
    summarise(total_density = sum(density)) %>% ungroup()

  dplyr::group_by(dat, year, quarter) %>% # pre D.4

    # first level (within quarters by catch or within strata by survey catch):
    dplyr::mutate(catch_quarter_prop = catch/sum(catch)) %>% # D.4
    dplyr::group_by(year, quarter, age) %>% # pre D.5

    # re-weight:
    dplyr::summarise(weighted_age_freq1 = sum(freq * catch_quarter_prop), # D.5
      sum_catch = sum(catch), sum_freq = sum(freq)) %>% # needed for D.6 and D.7

    # re-standardize:
    dplyr::mutate(weighted_age_freq1_scaled =
        weighted_age_freq1 * sum(sum_freq)/sum(weighted_age_freq1)) %>%  # D.6
    dplyr::group_by(year) %>% # pre D.7

    # second level (within years by catch or within survey-years by area):
    dplyr::mutate(catch_annual_prop = sum_catch/sum(sum_catch)) %>% # D.7
    dplyr::group_by(year, age) %>% # pre D.8

    # re-weight:
    dplyr::summarise(
      weighted_age_freq2 = sum(weighted_age_freq1_scaled * catch_annual_prop), # D.8
      sum_weighted_age_freq1 = sum(weighted_age_freq1_scaled)) %>% # needed for D.9
    dplyr::group_by(year) %>% # pre D.9

    # re-standardize:
    dplyr::mutate(weighted_age_freq2_scaled = weighted_age_freq2 *
        (sum(sum_weighted_age_freq1) / sum(weighted_age_freq2))) %>% # D.9
    dplyr::group_by(year) %>%

    # calculate proportions:
    dplyr::mutate(weighted_age_prop =
        weighted_age_freq2_scaled / sum(weighted_age_freq2_scaled)) %>% # D.10
    dplyr::select(-contains("freq"))
}

library(dplyr)
d <- readRDS("data-cache/all-survey-bio.rds") %>% as_tibble() %>%
  filter(species_common_name %in% "canary rockfish") %>%
  filter(survey_series_id %in% 1)

strat <- readRDS("data-cache/all-survey-strat-dens.rds") %>% as_tibble() %>%
  filter(species_common_name %in% "canary rockfish") %>%
  filter(survey_series_id %in% 1)

s <- group_by(strat, year, grouping_code) %>%
  summarise(density = sum(mean_per_strat), area = unique(area))

spa <- readRDS("data-cache/all-survey-spatial-tows.rds") %>%
  filter(species_common_name %in% "canary rockfish") %>%
  filter(survey_series_id %in% 1)
lu <- readRDS("data-cache/sample_trip_id_lookup.rds")
spa <- left_join(spa, lu, by = "fishing_event_id") %>%
  select(year, fishing_event_id, sample_id, grouping_code, density_kgpm2)

weight_proportions(d, s)

# d <- mutate(d, month = lubridate::month(trip_start_date),
#   quarter = case_when(
#     month %in% seq(1, 3) ~ 1,
#     month %in% seq(4, 6) ~ 2,
#     month %in% seq(7, 9) ~ 3,
#     month %in% seq(10, 12) ~ 4
#   )) %>% select(-month)

dplyr::mutate(year = lubridate::year(trip_start_date)) %>%
  dplyr::group_by(year, sample_id) %>%
  dplyr::mutate(weight = sum(weight)) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::group_by(year, sample_id, age) %>%
  dplyr::summarise(freq = n(), weight = weight[1]) %>%


dplyr::rename(d, quarter = grouping_code, catch = weight) %>%

  weight_proportions()

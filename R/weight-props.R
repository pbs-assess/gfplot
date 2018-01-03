
join_comps_strata <- function(dat, strat_dat) {
  library(dplyr)

  raw_comp <- dat %>%
    select(year, sample_id, age, weight, grouping_code) %>%
    filter(!is.na(age)) %>%
    group_by(year, sample_id, grouping_code, age) %>%
    summarise(freq = n()) %>% ungroup()

  strat_areas <- select(strat_dat, year, grouping_code, area_km2) %>%
    unique() %>%
    group_by(year) %>%
    mutate(total_area_km2 = sum(area_km2)) %>% ungroup()

  strat_dens <- select(strat_dat, year, fishing_event_id,
    grouping_code, density_kgpm2) %>%
    unique() %>%
    group_by(year, grouping_code) %>%
    summarise(total_density = sum(density_kgpm2*1e6)) %>% ungroup()

  sample_dens <- select(dat, -area_km2) %>%
    inner_join(strat_dat,
      by = c("year", "survey_id", "sample_id", "grouping_code")) %>%
    group_by(year, grouping_code, sample_id) %>%
    summarise(density = mean(density_kgpm2*1e6)) %>% ungroup()

  inner_join(raw_comp, sample_dens,
    by = c("year", "sample_id", "grouping_code")) %>%
    inner_join(strat_dens, by = c("year", "grouping_code")) %>%
    inner_join(strat_areas, by = c("year", "grouping_code"))
}

weight_proportions <- function(dat) {
  library(dplyr)

  dplyr::group_by(dat, year, grouping_code) %>% # pre D.4 / # quarter

    # first level (within quarters by catch or within strata by survey catch):
    dplyr::mutate(density_prop = density/total_density) %>% # D.4
    # re-weight:
    dplyr::group_by(year, grouping_code, area_km2, total_area_km2, age) %>% # pre D.5
    dplyr::summarise(weighted_age_freq1 = sum(freq * density_prop), # D.5
      sum_freq = sum(freq)) %>% # needed for D.6

    # re-standardize:
    dplyr::mutate(weighted_age_freq1_scaled =
        weighted_age_freq1 * sum(sum_freq)/sum(weighted_age_freq1)) %>%  # D.6
    dplyr::group_by(year) %>% # pre D.7

    # second level (within years by catch or within survey-years by area):
    dplyr::mutate(area_annual_prop = area_km2/sum(total_area_km2)) %>% # D.7
    dplyr::group_by(year, age) %>% # pre D.8

    # re-weight:
    dplyr::summarise(
      weighted_age_freq2 = sum(weighted_age_freq1_scaled * area_annual_prop), # D.8
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
  filter(species_common_name %in% "pacific ocean perch") %>%
  filter(survey_series_id %in% 1)

spa <- readRDS("data-cache/all-survey-spatial-tows.rds") %>%
  filter(species_common_name %in% "pacific ocean perch") %>%
  filter(survey_series_id %in% 1)
lu <- readRDS("data-cache/sample-trip-id-lookup.rds")
s <- left_join(spa, lu, by = "fishing_event_id") %>%
  select(year, survey_id, fishing_event_id, sample_id, grouping_code, density_kgpm2)
area <- readRDS("data-cache/stratum-areas.rds")
s <- left_join(s, area, by = c("survey_id", "grouping_code"))

comp <- join_comps_strata(d, s) %>% weight_proportions()

a1 <- group_by(comp, year) %>%
  summarise(total = sum(weighted_age_prop)) %>% round(9)
assertthat::assert_that(all(a1$total == 1))
head(comp)

raw <- d %>%
  select(year, age, weight) %>%
  filter(!is.na(age)) %>%
  group_by(year, age) %>%
  summarise(freq = n()) %>%
  group_by(year) %>%
  mutate(age_prop = freq / sum(freq)) %>%
  select(-freq) %>%
  left_join(comp, by = c("year", "age")) %>%
  filter(!is.na(weighted_age_prop))

tidyr::gather(raw, proportion_type, age_prop, -age, -year) %>%
  ggplot(aes(year, age, size = age_prop)) +
  # geom_vline(xintercept = seq(min(raw$year), max(raw$year)), alpha = 0.2) +
  geom_point(alpha = 0.8, pch = 21) + scale_size(range = c(0.2, 8)) +
  facet_wrap(~proportion_type) + ggsidekick::theme_sleek() +
  scale_x_continuous(breaks = seq(min(raw$year), max(raw$year), 2))

ggplot(raw, aes(year, age, colour = weighted_age_prop / age_prop,
  size = abs(log(weighted_age_prop / age_prop)))) +
  # geom_vline(xintercept = seq(min(raw$year), max(raw$year)), alpha = 0.2) +
  geom_point(pch = 21) +
  scale_size(range = c(0.2, 8)) +
  scale_color_gradient2(midpoint = 1.0) + ggsidekick::theme_sleek() +
  scale_x_continuous(breaks = seq(min(raw$year), max(raw$year), 2))


# #####################################################################
# # d <- mutate(d, month = lubridate::month(trip_start_date),
# #   quarter = case_when(
# #     month %in% seq(1, 3) ~ 1,
# #     month %in% seq(4, 6) ~ 2,
# #     month %in% seq(7, 9) ~ 3,
# #     month %in% seq(10, 12) ~ 4
# #   )) %>% select(-month)
#
# dplyr::mutate(year = lubridate::year(trip_start_date)) %>%
#   dplyr::group_by(year, sample_id) %>%
#   dplyr::mutate(weight = sum(weight)) %>%
#   dplyr::filter(!is.na(age)) %>%
#   dplyr::group_by(year, sample_id, age) %>%
#   dplyr::summarise(freq = n(), weight = weight[1]) %>%
#
#
#   dplyr::rename(d, quarter = grouping_code, catch = weight) %>%
#
#   weight_proportions()

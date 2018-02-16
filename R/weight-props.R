#' Age and length frequency weighting
#'
#' These functions weight age or length frequencies.
#' `tidy_comps_commercial()` and `tidy_comps_survey()` join the necessary data
#' for commercial or survey samples, respectively, and format it for
#' weighting. `weight_comps()` does the actual weighting based on the output
#' from the `tidy_*` functions.
#'
#' @param specimen_dat Specimen data. E.g. from [get_commsamples()] for
#' commercial data or [get_survsamples()] for survey data.
#' @param catch_dat Catch data. E.g. from [get_catch()].
#' @param survey_tows Survey tow data. E.g. from [get_sample_trips()].
#' @param value The **unquoted** column name with the values to re-weight
#' (e.g. `age` or `length`).
#' @param bin_size The binning size (likely only used for lengths).
#' @param dat A properly formatted data frame such the output from
#' `tidy_comps_survey()` or `tidy_comps_commercial()`. See details.
#'
#' @details The input data frame to `weight_comps()` must have columns in the
#' following order:
#' 1. year
#' 1. weighting unit ID (e.g. trip ID or sample ID)
#' 1. grouping variable for first weighting (e.g. quarter or stratum)
#' 1. value of weighting variable (e.g. age or length bin)
#' 1. frequency of that weighting variable (e.g. frequency of that age or length bin)
#' 1. numerator in first weighting (e.g. sample catch weight or density)
#' 2. denominator in first weighting (e.g. quarter catch weight or total stratum density)
#' 1. numerator in second weighting (e.g. catch that quarter or stratum area)
#' 1. denominator in second weighting (e.g. catch that year or total survey area)
#'
#' The names of the columns do not matter, only their contents. The data frame
#' should contain these columns and only these columns.
#'
#' @references
#' Page 161 of Kendra R. Holt, Paul J. Starr, Rowan Haigh, and Brian Krishka.
#' 2016. Stock Assessment and Harvest Advice for Rock Sole (Lepidopsetta spp.)
#' in British Columbia. CSAS Res. Doc. 2016/009.
#' [Link](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2016/2016_009-eng.html)
#' [PDF](http://waves-vagues.dfo-mpo.gc.ca/Library/363948.pdf)
#'
#' @family age- and length-frequency functions
#' @examples
#' \dontrun{
#' species <- "pacific ocean perch"
#' survey <- "Queen Charlotte Sound Synoptic Survey"
#'
#' ## Surveys:
#' survey_samples <- get_survsamples(species) %>%
#'   dplyr::filter(survey_series_desc == survey)
#' survey_trips <- get_sample_trips() %>%
#'   dplyr::filter(survey_series_desc == survey)
#'
#' surv_lengths <- tidy_comps_survey(survey_samples, survey_trips,
#'   value = length, bin_size = 2)
#' surv_lengths
#' weight_comps(surv_lengths)
#'
#' surv_ages <- tidy_comps_survey(survey_samples, survey_trips, value = age)
#' surv_ages
#' weight_comps(surv_ages)
#'
#' ## Commercial:
#' comm_samples <- get_commsamples(species) %>%
#'   dplyr::filter(survey_series_desc == survey)
#' comm_catch <- get_catch(species) %>%
#'   dplyr::filter(survey_series_desc == survey)
#'
#' comm_lengths <- tidy_comps_commercial(comm_samples, comm_catch,
#'   value = length, bin_size = 2)
#' comm_lengths
#' weight_comps(comm_lengths)
#'
#' comm_ages <- tidy_comps_commercial(comm_samples, comm_catch,
#'   value = age)
#' comm_ages
#' weight_comps(comm_ages)
#'
#' ## These functions are pipe (%>%) friendly. E.g.:
#' tidy_comps_survey(survey_samples, survey_trips, value = age) %>%
#'   weight_comps()
#' }
#' @name weight_comps
NULL

bin_lengths <- function(dat, value, bin_size) {
  value <- enquo(value)
  bin_range <- dat %>% select(!!value) %>% pull() %>% range()
  bins <- seq(min(bin_range), max(bin_range), by = bin_size)
  mutate(dat, !!quo_name(value) :=
    bins[findInterval(!!value, bins)] + bin_size / 2)
}

#' @export
#' @rdname weight_comps
tidy_comps_commercial <- function(specimen_dat, catch_dat, value,
  bin_size = NULL) {

  value <- enquo(value)
  specimen_dat <- specimen_dat %>% filter(!is.na(!!value))

  if (!is.null(bin_size))
    specimen_dat <- bin_lengths(specimen_dat, !!value, bin_size = bin_size)

  dat <- mutate(specimen_dat, month = lubridate::month(trip_start_date),
    quarter = case_when(
      month %in% seq(1, 3) ~ 1,
      month %in% seq(4, 6) ~ 2,
      month %in% seq(7, 9) ~ 3,
      month %in% seq(10, 12) ~ 4
    )) %>% select(-month)

  catch_dat <- filter(catch_dat, !is.na(fe_end_date)) %>%
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

  freq_and_catch_by_trip <- group_by(dat, year, trip_id, quarter, !!value) %>%
    summarise(freq = n()) %>%
    inner_join(sampled_trip_id_catch, by = c("year", "trip_id", "quarter"))

  species_catch_by_quarter <- group_by(catch_dat, year, quarter) %>%
    summarise(landed_kg_quarter = sum(landed_kg)) %>%
    group_by(year) %>%
    mutate(landed_kg_year = sum(landed_kg_quarter))

  inner_join(freq_and_catch_by_trip,
    species_catch_by_quarter, by = c("year", "quarter")) %>%
    inner_join(quarter_sampled_catch, by = c("year", "quarter")) %>%
    select(year, trip_id, quarter, !!value, freq,
      samp_trip_catch_weight, samp_catch_weight_quarter,
      landed_kg_quarter, landed_kg_year) %>% # re-order columns
    arrange(year, trip_id, !!value) %>%
    ungroup()
}

#' @export
#' @rdname weight_comps
tidy_comps_survey <- function(specimen_dat, survey_tows, value,
  bin_size = NULL) {

  value <- enquo(value)
  specimen_dat <- specimen_dat %>% filter(!is.na(!!value))

  if (!is.null(bin_size))
    specimen_dat <- bin_lengths(specimen_dat, !!value, bin_size = bin_size)

  sample_trip_ids <- readRDS("data-cache/sample-trip-id-lookup.rds")
  areas <- readRDS("data-cache/stratum-areas.rds")

  strat_dat <- survey_tows %>%
    left_join(sample_trip_ids, by = "fishing_event_id") %>%
    select(year, survey_id, fishing_event_id, sample_id, grouping_code,
      density_kgpm2)
  strat_dat <- left_join(strat_dat, areas, by = c("survey_id", "grouping_code"))

  raw_comp <- specimen_dat %>%
    select(year, sample_id, !!value, weight, grouping_code) %>%
    group_by(year, sample_id, grouping_code, !!value) %>%
    summarise(freq = n())

  strat_areas <- select(strat_dat, year, grouping_code, area_km2) %>%
    unique() %>%
    group_by(year) %>%
    mutate(total_area_km2 = sum(area_km2))

  strat_dens <- select(strat_dat, year, fishing_event_id,
    grouping_code, density_kgpm2) %>%
    unique() %>%
    group_by(year, grouping_code) %>%
    summarise(total_density = sum(density_kgpm2 * 1e6))

  sample_dens <- select(specimen_dat, -area_km2) %>%
    inner_join(strat_dat,
      by = c("year", "survey_id", "sample_id", "grouping_code")) %>%
    group_by(year, grouping_code, sample_id) %>%
    summarise(density = mean(density_kgpm2 * 1e6)) # should be one unique value

  inner_join(raw_comp, sample_dens,
    by = c("year", "sample_id", "grouping_code")) %>%
    inner_join(strat_dens, by = c("year", "grouping_code")) %>%
    inner_join(strat_areas, by = c("year", "grouping_code")) %>%
    ungroup() %>%
    arrange(year, sample_id, !!value)
}

weight_comps_base <- function(dat) {
  # Internal function that actually does the weighting
  #
  # value (age or length)
  #
  # grouping1 (quarter or grouping_code)
  #
  # weighting1 (samp_trip_catch_weight or density)
  # weighting1_total (samp_catch_weight_quarter or total_density)
  #
  # weighting2 (landed_kg_quarter or area_km2)
  # weighting2_total (landed_kg_year or total_area_km2)
  #
  # Equation numbers refer to p161 of: Stock Assessment and Harvest Advice for
  # Rock Sole (Lepidopsetta spp.) in British Columbia Kendra R. Holt, Paul J.
  # Starr, Rowan Haigh, and Brian Krishka using dplyr and pipes. PDF:
  # http://waves-vagues.dfo-mpo.gc.ca/Library/363948.pdf

  group_by(dat, year, grouping1) %>% # pre D.4

    # first level (within quarters by catch or within strata by survey catch):
    mutate(prop = weighting1 / weighting1_total) %>% # D.4
    # re-weight:
    group_by(year, grouping1, weighting2, weighting2_total, value) %>% # pre D.5
    summarise(weighted_freq1 = sum(freq * prop), # D.5
      sum_freq = sum(freq)) %>% # needed for D.6

    # re-standardize:
    mutate(weighted_freq1_scaled =
        weighted_freq1 * sum(sum_freq) / sum(weighted_freq1)) %>%  # D.6
    group_by(year) %>% # pre D.7

    # second level (within years by catch or within survey-years by area):
    mutate(annual_prop = weighting2 / sum(weighting2_total)) %>% # D.7
    group_by(year, value) %>% # pre D.8

    # re-weight:
    summarise(
      weighted_freq2 = sum(weighted_freq1_scaled * annual_prop), # D.8
      sum_weighted_freq1 = sum(weighted_freq1_scaled)) %>% # needed for D.9
    group_by(year) %>% # pre D.9

    # re-standardize:
    mutate(weighted_freq2_scaled = weighted_freq2 *
        (sum(sum_weighted_freq1) / sum(weighted_freq2))) %>% # D.9
    group_by(year) %>%

    # calculate proportions:
    mutate(weighted_prop =
        weighted_freq2_scaled / sum(weighted_freq2_scaled)) %>% # D.10
    select(-contains("freq"))
}

#' @export
#' @rdname weight_comps
weight_comps <- function(dat) {
  names(dat) <- c("year", "id", "grouping1", "value", "freq",
    "weighting1", "weighting1_total",
    "weighting2", "weighting2_total")
  weight_comps_base(dat)
}

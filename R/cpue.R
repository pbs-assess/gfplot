#' Prepare commercial PBS CPUE data
#'
#' @param dat An input data frame from TODO
#' @param species_common The species common name
#' @param year_range The range of years to include
#' @param lat_range The range of latitudes to include
#' @param min_positive_tows The minimum number of positive tows over all years
#' @param min_annual_positive_trips The minimum number of annual positive trips
#' @param min_years_with_min_positive_trips The number of years in which the
#'   \code{min_annual_positive_trips} criteria needs to be met
#' @param area_grep_pattern A regular expression to extract the management areas
#'   of interest
#' @param lat_bands A sequence of latitude bans
#' @param depth_bands A sequence of depth bands
#' @param anonymous_vessels Should the vessel names be anonymized?
#'
#' @export
#'
# @examples
# # catch <- readRDS("~/Dropbox/dfo/selected-data/gf_merged_catch_1996_onwards.rds")
# prep_pbs_cpue(catch, species = "arrowtooth flounder")
prep_pbs_cpue <- function(dat, species_common,
  year_range = c(1996, 2015),
  lat_range = c(48, Inf),
  min_positive_tows = 100,
  min_annual_positive_trips = 4,
  min_years_with_min_positive_trips = 4,
  area_grep_pattern = "5[CDE]+",
  lat_bands = seq(47, 62, 0.1),
  depth_bands = seq(0, 900, 25),
  anonymous_vessels = TRUE) {

  # areas and species are package data
  pbs_areas <- pbs_areas[grep(area_grep_pattern, pbs_areas$MAJOR_STAT_AREA_DESCRIPTION), ]
  names(catch) <- tolower(names(catch))
  catch <- inner_join(catch, pbs_species, by = "species_code")

  d <- inner_join(catch, pbs_areas, by = "major_stat_area_code") %>%
    mutate(year = lubridate::year(best_date)) %>%
    filter(year >= year_range[[1]] & year <= year_range[[2]]) %>%
    filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
    filter(gear %in% toupper(gear)) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    filter(latitude >= lat_range[[1]] & latitude <= lat_range[[2]]) %>%
    mutate(month = lubridate::month(best_date)) %>%
    mutate(hours_fished =
        as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))) %>%
    filter(hours_fished > 0) %>%
    mutate(catch = landed_kg + discarded_kg)

  d_fe <- group_by(d, fishing_event_id, month, locality_code,
    vessel_name, year, hours_fished, trip_id) %>%
    mutate(
      spp_in_fe = toupper(species_common) %in% species_common_name,
      spp_in_row = species_common_name == toupper(species_common)) %>%
    summarise(pos_catch = ifelse(spp_in_fe[[1]], 1, 0),
      spp_catch = sum(ifelse(spp_in_row, catch, 0), na.rm = TRUE),
      best_depth = mean(best_depth, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE)) %>%
    ungroup()

  caught_something <- d_fe %>%
    group_by(vessel_name) %>%
    mutate(total_positive_tows = sum(pos_catch)) %>%
    filter(total_positive_tows >= min_positive_tows) %>%
    filter(spp_catch > 0) %>%
    group_by(year, vessel_name, trip_id) %>%
    summarise(sum_catch = sum(spp_catch, na.rm = TRUE)) %>%
    filter(sum_catch > 0) %>%
    group_by(vessel_name) %>%
    mutate(n_years = length(unique(year))) %>%
    # for speed (filter early known cases):
    filter(n_years >= min_years_with_min_positive_trips) %>%
    group_by(year, vessel_name) %>%
    summarise(n_trips_per_year = length(unique(trip_id))) %>%
    mutate(trips_over_treshold_this_year =
        n_trips_per_year >= min_annual_positive_trips) %>%
    group_by(vessel_name) %>%
    summarise(trips_over_thresh = sum(trips_over_treshold_this_year)) %>%
    filter(trips_over_thresh >= min_years_with_min_positive_trips) %>%
    ungroup()

  d_retained <- dplyr::semi_join(d_fe, caught_something, by = "vessel_name") %>%
    filter(best_depth >= min(depth_bands) & best_depth <= max(depth_bands)) %>%
    mutate(
      depth_band = as.factor(depth_bands[findInterval(best_depth, depth_bands)]),
      vessel_name = as.factor(vessel_name),
      latitude_band = as.factor(lat_bands[findInterval(latitude, lat_bands)]),
      dfo_locality = as.factor(locality_code)) %>%
    mutate(pos_catch = ifelse(spp_catch > 0, 1, 0))

  if (anonymous_vessels) {
    vessel_df <- dplyr::tibble(vessel_name = unique(d_retained$vessel_name),
      scrambled_vessel = as.factor(seq_along(vessel_name)))
    d_retained <- left_join(d_retained, vessel_df, by = "vessel_name") %>%
      select(-vessel_name) %>%
      dplyr::rename(vessel_name = scrambled_vessel)
  }

  dplyr::as_tibble(d_retained)
}

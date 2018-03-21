#' Tidy commercial PBS CPUE data
#'
#' @param dat An input data frame from \code{\link{get_cpue_index}}
#' @param species_common The species common name
#' @param year_range The range of years to include
#' @param lat_range The range of latitudes to include
#' @param min_positive_tows The minimum number of positive tows over all years
#' @param min_positive_trips The minimum number of annual positive trips
#' @param min_yrs_with_trips The number of years in which the
#'   \code{min_positive_trips} criteria needs to be met
#' @param area_grep_pattern A regular expression to extract the management areas
#'   of interest
#' @param lat_bands A sequence of latitude bans
#' @param depth_bands A sequence of depth bands
#' @param gear Gear types
#' @family tidy data functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_cpue_index(gear = "bottom trawl")
#' walleye <- tidy_cpue_index(d, "walleye pollock",
#'   area_grep_pattern = "5[CDE]+")
#' }

tidy_cpue_index <- function(dat, species_common,
  year_range = c(1996, Inf),
  lat_range = c(48, Inf),
  min_positive_tows = 100,
  min_positive_trips = 4,
  min_yrs_with_trips = 4,
  area_grep_pattern = "5[CDE]+",
  lat_bands = seq(48, 59, 0.1),
  depth_bands = seq(50, 450, 25),
  gear = "BOTTOM TRAWL") {
  pbs_areas <- gfplot::pbs_areas[grep(
    area_grep_pattern,
    gfplot::pbs_areas$major_stat_area_description
  ), ]
  names(dat) <- tolower(names(dat))
  dat <- inner_join(dat, gfplot::pbs_species, by = "species_code")

  # basic filtering:
  catch <- dat %>%
    inner_join(pbs_areas, by = "major_stat_area_code") %>%
    mutate(year = lubridate::year(best_date)) %>%
    filter(year >= year_range[[1]] & year <= year_range[[2]]) %>%
    filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    filter(gear %in% toupper(gear)) %>%
    filter(latitude >= lat_range[[1]] & latitude <= lat_range[[2]]) %>%
    mutate(month = lubridate::month(best_date)) %>%
    mutate(
      hours_fished =
        as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))
    ) %>%
    filter(hours_fished > 0) %>%
    mutate(catch = landed_kg + discarded_kg) %>%
    group_by(fishing_event_id) %>%
    mutate(n_date = length(unique(best_date))) %>%
    filter(n_date < Inf) %>%
    select(-n_date) %>% # remove FE_IDs with multiple dates
    ungroup()

  # catch for target spp:
  catch <- group_by(
    catch, fishing_event_id, best_date, month, locality_code,
    vessel_name, year, trip_id, hours_fished
  ) %>%
    mutate(
      spp_in_fe = toupper(species_common) %in% species_common_name,
      spp_in_row = species_common_name == toupper(species_common)
    ) %>%
    summarise(
      pos_catch = ifelse(spp_in_fe[[1]], 1, 0),
      # hours_fished = mean(hours_fished, na.rm = TRUE),
      spp_catch = sum(ifelse(spp_in_row, catch, 0), na.rm = TRUE),
      best_depth = mean(best_depth, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE)
    ) %>%
    ungroup()

  # figure out which vessels should be considered part of the spp. fleet
  fleet <- catch %>%
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
    filter(n_years >= min_yrs_with_trips) %>%
    group_by(year, vessel_name) %>%
    summarise(n_trips_per_year = length(unique(trip_id))) %>%
    mutate(
      trips_over_treshold_this_year =
        n_trips_per_year >= min_positive_trips
    ) %>%
    group_by(vessel_name) %>%
    summarise(trips_over_thresh = sum(trips_over_treshold_this_year)) %>%
    filter(trips_over_thresh >= min_yrs_with_trips) %>%
    ungroup()

  # retain the data from our "fleet"
  d_retained <- semi_join(catch, fleet, by = "vessel_name") %>%
    filter(best_depth >= min(depth_bands) & best_depth <= max(depth_bands)) %>%
    mutate(
      depth = factor_bin_clean(best_depth, depth_bands),
      vessel = as.factor(vessel_name),
      latitude = factor_bin_clean(latitude, lat_bands),
      locality = factor_clean(locality_code),
      year_factor = factor_clean(year),
      month = factor_clean(month)
    ) %>%
    mutate(pos_catch = ifelse(spp_catch > 0, 1, 0))

  # anonymize the vessels
  vessel_df <- dplyr::tibble(
    vessel = unique(d_retained$vessel),
    scrambled_vessel = factor_clean(seq_along(vessel))
  )
  d_retained <- left_join(d_retained, vessel_df, by = "vessel") %>%
    select(-vessel) %>%
    rename(vessel = scrambled_vessel)

  arrange(d_retained, .data$year, .data$vessel) %>%
    dplyr::as_tibble()
}

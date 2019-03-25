#' Tidy commercial PBS CPUE data
#'
#' This function determines the qualifying "fleet" for CPUE analyses. It is
#' meant to be used with modern data available at the fishing event level.
#'
#' @param dat An input data frame from [get_cpue_index()].
#' @param species_common The species common name.
#' @param year_range The range of years to include.
#' @param alt_year_start_date Alternative year starting date specified as a
#'   month-day combination. E.g. "03-01" for March 1st. Can be used to create
#'   'fishing years'.
#' @param use_alt_year Should the alternate year (e.g. fishing year) column
#'   `alt_year` be used? If `FALSE` then the calendar year will be used. If this
#'   is set to `TRUE` then the `year` column will be replaced with the
#'   `alt_year` column.
#' @param lat_range The range of latitudes to include.
#' @param min_positive_tows The minimum number of positive tows over all years.
#' @param min_positive_trips The minimum number of annual positive trips.
#' @param min_yrs_with_trips The number of years in which the
#'   `min_positive_trips` criteria needs to be met.
#' @param area_grep_pattern A regular expression to extract the management areas
#'   of interest.
#' @param lat_band_width The latitude bandwidths in degrees.
#' @param depth_band_width The depth band widths in m.
#' @param clean_bins Logical. Should the depth and latitude bands be rounded to
#'   the nearest clean value as defined by `lat_band_width` or
#'   `depth_band_width`? Internally, these use, for example:
#'   `gfplot:::round_down_even(lat_range[1], lat_band_width)`.
#' @param depth_bin_quantiles Quantiles for the depth bands. If the cumulative
#'   proportion of positive fishing events within a given depth bin is less then
#'   the lower amount or greater than the upper amount then that depth bin will
#'   be dropped.
#' @param min_bin_prop If the proportion of fishing events for any given factor
#'   level is less than this value then that factor level will be dropped.
#' @param lat_bin_quantiles Quantiles for the latitude bands. Values above and
#'   below these quantiles will be discarded.
#' @param gear One or more gear types as a character vector.
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
                            year_range = c(1996, as.numeric(format(Sys.Date(), "%Y")) - 1),
                            alt_year_start_date = "04-01",
                            use_alt_year = FALSE,
                            lat_range = c(48, Inf),
                            min_positive_tows = 100,
                            min_positive_trips = 5,
                            min_yrs_with_trips = 5,
                            area_grep_pattern = "5[CDE]+",
                            lat_band_width = 0.1,
                            depth_band_width = 25,
                            clean_bins = TRUE,
                            depth_bin_quantiles = c(0.001, 0.999),
                            min_bin_prop = 0.001,
                            lat_bin_quantiles = c(0, 1),
                            gear = "bottom trawl") {

  pbs_areas <- gfplot::pbs_areas[grep(
    area_grep_pattern,
    gfplot::pbs_areas$major_stat_area_description
  ), ]
  names(dat) <- tolower(names(dat))
  dat <- inner_join(dat, gfplot::pbs_species, by = "species_code")

  dat <- dat %>% mutate(year = lubridate::year(best_date))

  # create possibly alternate starting date:
  if (alt_year_start_date != "01-01") {
    dat <- dplyr::mutate(dat, .year_start_date =
        lubridate::ymd_hms(paste0(year, "-", alt_year_start_date, " 00:00:00")))
    dat <- dplyr::mutate(dat, .time_diff = best_date - .year_start_date)
    dat <- dplyr::mutate(dat, alt_year = ifelse(.time_diff > 0, year, year - 1L))
    dat <- dplyr::select(dat, -.time_diff, -.year_start_date)
  }

  if (use_alt_year) {
    dat$year <- NULL
    dat$year <- dat$alt_year
    dat$alt_year <- NULL
  }

  dat$locality_code <- paste(dat$major_stat_area_code,
    dat$minor_stat_area_code, dat$locality_code, sep = "-")

  # basic filtering:
  catch <- dat %>%
    inner_join(pbs_areas, by = "major_stat_area_code") %>%
    filter(year >= year_range[[1]] & year <= year_range[[2]]) %>%
    filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(best_depth)) %>%
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
  d_retained <- semi_join(catch, fleet, by = "vessel_name")

  if (nrow(d_retained) == 0) {
    warning("No vessels passed the fleet conditions.")
    return(NA)
  }

  lat_range <- stats::quantile(d_retained$latitude,
    probs = c(min(lat_bin_quantiles), max(lat_bin_quantiles))
  )
  # depth_range <- stats::quantile(d_retained$best_depth,
  #   probs = c(min(depth_bin_quantiles), max(depth_bin_quantiles))
  # )
  depth_range <- vector(mode = "numeric", length = 2L)

  if (clean_bins) {
    lat_range[1] <- round_down_even(lat_range[1], lat_band_width)
    lat_range[2] <- round_up_even(lat_range[2], lat_band_width)
    depth_range[1] <- 0
    depth_range[2] <- 10000
  }

  d_retained <- filter(
    d_retained,
    latitude > lat_range[[1]],
    latitude < lat_range[[2]],
    best_depth > depth_range[[1]],
    best_depth < depth_range[[2]]
  )
  lat_bands <- seq(lat_range[[1]], lat_range[[2]], lat_band_width)
  depth_bands <- seq(depth_range[[1]], depth_range[[2]], depth_band_width)

  d_retained <- d_retained %>%
    filter(best_depth >= min(depth_bands) & best_depth <= max(depth_bands)) %>%
    mutate(
      depth = factor_bin_clean(best_depth, depth_bands),
      vessel = as.factor(vessel_name),
      latitude = factor_bin_clean(latitude, lat_bands),
      locality = as.factor(locality_code),
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

  pos_catch_fleet <- filter(d_retained, .data$pos_catch == 1)

  # retain depth bins with enough data:
  tb <- table(pos_catch_fleet$depth)
  too_deep <- names(tb)[cumsum(tb)/sum(tb) > depth_bin_quantiles[2]]
  too_shallow <- names(tb)[cumsum(tb)/sum(tb) < depth_bin_quantiles[1]]
  d_retained <- filter(d_retained,
    !.data$depth %in% union(too_deep, too_shallow))

  too_few <- function(x) {
    tb <- table(pos_catch_fleet[[x]])
    names(tb)[tb/sum(tb) < min_bin_prop]
  }

  # not enough pos. data in bins?
  d_retained <- filter(d_retained, !.data$latitude %in% too_few("latitude"))
  d_retained <- filter(d_retained, !.data$month %in% too_few("month"))
  d_retained <- filter(d_retained, !.data$depth %in% too_few("depth"))
  d_retained <- filter(d_retained, !.data$locality %in% too_few("locality"))
  # vessel already known

  base_month      <- get_most_common_level(pos_catch_fleet$month)
  base_depth      <- get_most_common_level(pos_catch_fleet$depth)
  base_lat        <- get_most_common_level(pos_catch_fleet$latitude)
  base_vessel     <- get_most_common_level(pos_catch_fleet$vessel)
  base_locality   <- get_most_common_level(pos_catch_fleet$locality)

  d_retained$locality  <- stats::relevel(as.factor(d_retained$locality),
    ref = base_locality)
  d_retained$depth     <- stats::relevel(as.factor(d_retained$depth),
    ref = base_depth)
  d_retained$latitude  <- stats::relevel(as.factor(d_retained$latitude),
    ref = base_lat)
  d_retained$vessel    <- stats::relevel(as.factor(d_retained$vessel),
    ref = base_vessel)
  d_retained$month     <- stats::relevel(as.factor(d_retained$month),
    ref = base_month)

  d_retained <- droplevels(d_retained)

  d_retained <- mutate(d_retained, cpue = .data$spp_catch / .data$hours_fished)

  # make sure unique:
  d_retained <- mutate(d_retained,
    fishing_event_id = paste(year, trip_id, fishing_event_id, sep = "-"))

  arrange(d_retained, .data$year, .data$vessel) %>%
    dplyr::as_tibble()
}

get_most_common_level <- function(x) {
  rev(names(sort(table(x))))[[1]]
}

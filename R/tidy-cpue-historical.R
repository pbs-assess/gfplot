clean_localities <- function(x) {
  gsub(" |\\/|'|>|\\(|\\)|\\-|\\.|[0-9]+", "", x)
}

#' Tidy historical CPUE data
#'
#' The function can create trip level historical CPUE for index standardization
#' or an "arithmetic" CPUE where the catch and effort are summed each year and
#' divided.
#'
#' @param dat Data from [get_cpue_historical()].
#' @param species_common A species common name in lowercase.
#' @param year_range A range of years to include. Can go up to current year but
#'   note the management changes, particularly in 1996.
#' @param area_grep_pattern A major statistical area as a regular expression.
#' @param use_alt_year Should the alternate year (e.g. fishing year) column
#'   `alt_year` be used? If `FALSE` then the calendar year will be used. If this
#'   is set to `TRUE` then the `year` column will be replaced with the
#'   `alt_year` column.
#' @param depth_bin_quantiles Quantiles for the depth bands. If the cumulative
#'   proportion of positive fishing events within a given depth bin is less then
#'   the lower amount or greater than the upper amount then that depth bin will
#'   be dropped.
#' @param depth_band_width The depth band widths in m.
#' @param min_bin_prop If the proportion of fishing events for any given factor
#'   level is less than this value then that factor level will be dropped.
#' @param type Should the trip-level data be returned (for possible future index
#'   standardization) or should the arithmetic CPUE be returned (species
#'   specific catch and all effort is summed each year and then divided.)
#' @param max_fe_hours The maximum number of allowable hours per fishing event
#'   (e.g. per trawl). Fishing events that are longer than this will be removed.
#'   This can be used to remove erroneous fishing events. Only affects data in
#'   1991 and after when the data are recorded as fishing events.
#'
#' @details
#' Note that fishing events prior to 1991 are 'rolled up' in the databases to
#' locality-specific trips. This function well aggregate fishing events into
#' trips in 1991 and after to create a consistent form of data.
#' @return
#' A (tibble) data frame.
#'
#' @examples
#' \dontrun{
#' get_cpue_historical(end_year = 2016) %>%
#'   tidy_cpue_historical(species_common = "pacific cod", area_grep_pattern = "5[CD]+")
#' }
#' @export
tidy_cpue_historical <- function(dat,
                            species_common,
                            year_range = c(1956, 1995),
                            area_grep_pattern = c("3[CD]+", "5[ABCDE]+"),
                            use_alt_year = FALSE,
                            depth_band_width = 25,
                            depth_bin_quantiles = c(0.001, 0.999),
                            min_bin_prop = 0.001,
                            type = c("trip-level-data", "arithmetic-cpue"),
                            max_fe_hours = 5) {
  type <- match.arg(type)

  if (use_alt_year) {
    dat$year <- NULL
    dat$year <- dat$alt_year
    dat$alt_year <- NULL
  }

  # basic filtering:
  dat <- dat %>% filter(
    !is.na(hours_fished),
    !is.na(total), !is.na(best_depth_m),
    best_depth_m > 0, hours_fished > 0, total > 0,
    year >= year_range[[1]], year <= year_range[[2]]
  )

  dat$area <- assign_areas(dat$major_stat_area_description, area_grep_pattern)
  dat <- dat[!is.na(dat$area), , drop = FALSE]

  fe_dat <- dat %>%
    group_by(year, area, trip_id, fishing_event_id) %>%
    summarise(
      spp_catch = sum(ifelse(tolower(species_common_name) == tolower(species_common), total, 0)),
      hours_fished = mean(hours_fished, na.rm = TRUE), # should be 1
      depth = mean(best_depth_m, na.rm = TRUE), # should be 1
      month = month[[1]], # should be 1
      # - is for formatting for easy coefficient plotting:
      locality = paste0("-", clean_localities(locality_description[[1]]))
    ) %>%
    ungroup()

  fe_dat <- fe_dat[fe_dat$year < 1991 |
      (fe_dat$year >= 1991 & fe_dat$hours_fished <= max_fe_hours), , drop = FALSE]

  # arith. cpue for target spp:
  cpue_arith <- fe_dat %>%
    group_by(area, year) %>%
    summarise(spp_catch = sum(spp_catch, na.rm = TRUE),
      hours_fished = sum(hours_fished, na.rm = TRUE)) %>%
    mutate(cpue_arith = spp_catch / hours_fished) %>%
    ungroup()

  if (type == "trip-level-data") {
    trip_dat <- fe_dat %>%
      group_by(year, area, trip_id, locality) %>%
      summarise(
        spp_catch = sum(spp_catch, na.rm = TRUE),
        hours_fished = sum(hours_fished, na.rm = TRUE),
        mean_depth = mean(depth, na.rm = TRUE),
        month = month[[1]]
      ) %>%
      ungroup()

    # fleet cleaning:
    pos_catch_fleet <- filter(trip_dat, .data$spp_catch > 0)
    tb <- table(pos_catch_fleet$mean_depth)
    too_deep <- names(tb)[cumsum(tb)/sum(tb) > depth_bin_quantiles[2]]
    too_shallow <- names(tb)[cumsum(tb)/sum(tb) < depth_bin_quantiles[1]]
    trip_dat <- filter(trip_dat,
      !.data$mean_depth %in% union(too_deep, too_shallow))

    depth_range <- c(0, 10000)
    depth_bands <- seq(depth_range[[1]], depth_range[[2]], depth_band_width)
    trip_dat <- trip_dat %>%
      filter(mean_depth >= min(depth_bands) & mean_depth <= max(depth_bands)) %>%
      mutate(
        depth = factor_bin_clean(mean_depth, depth_bands),
        locality = as.factor(locality),
        year_factor = factor_clean(year),
        month = factor_clean(month)
      ) %>%
      mutate(pos_catch = ifelse(spp_catch > 0, 1, 0))
  }

  too_few <- function(x) {
    tb <- table(pos_catch_fleet[[x]])
    names(tb)[tb/sum(tb) < min_bin_prop]
  }

  pos_catch_fleet <- filter(trip_dat, .data$spp_catch > 0)
  # not enough pos. data in bins?
  trip_dat <- filter(trip_dat, !.data$month %in% too_few("month"))
  trip_dat <- filter(trip_dat, !.data$depth %in% too_few("depth"))
  trip_dat <- filter(trip_dat, !.data$locality %in% too_few("locality"))

  base_month      <- get_most_common_level(pos_catch_fleet$month)
  base_depth      <- get_most_common_level(pos_catch_fleet$depth)
  base_locality   <- get_most_common_level(pos_catch_fleet$locality)

  trip_dat$locality  <- stats::relevel(as.factor(trip_dat$locality),
    ref = base_locality)
  trip_dat$depth     <- stats::relevel(as.factor(trip_dat$depth),
    ref = base_depth)
  trip_dat$month     <- stats::relevel(as.factor(trip_dat$month),
    ref = base_month)

  trip_dat <- droplevels(trip_dat)

  trip_dat <- mutate(trip_dat, cpue = .data$spp_catch / .data$hours_fished)
  trip_dat <- rename(trip_dat, best_depth = .data$mean_depth)

  if (type == "trip-level-data") {
    dplyr::as_tibble(trip_dat)
  } else {
    dplyr::as_tibble(cpue_arith)
  }
}

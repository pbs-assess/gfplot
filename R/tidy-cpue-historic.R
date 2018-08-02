clean_localities <- function(x) {
  gsub(" |\\/|'|>|\\(|\\)|\\-|\\.|[0-9]+", "", x)
}

#' Tidy historical CPUE data
#'
#' The function can create trip level historical CPUE for index standardization
#' or an "arithmetic" CPUE where the catch and effort are summed each year and
#' divided.
#'
#' @param dat Data from [get_cpue_historic()].
#' @param species_common A species common name in lowercase.
#' @param year_range A range of years to include. Can go up to current year but
#'   note the management changes, particularly in 1996.
#' @param area A major statistical area as a regular expression.
#' @param use_alt_year Should the alternate year (e.g. fishing year) column
#'   `alt_year` be used? If `FALSE` then the calendar year will be used. If this
#'   is set to `TRUE` then the `year` column will be replaced with the
#'   `alt_year` column.
#' @param depth_bin_quantiles Quantiles for the depth bands. Values above and
#'   below these quantiles will be discarded.
#' @param type Should the trip-level data be returned (for possible future index
#'   standardization) or should the arithmetic CPUE be returned (species
#'   specific catch and all effort is summed each year and then divided.)
#'
#' @details
#' Note that fishing events prior to 1991 are 'rolled up' in the databases to
#' locality-specific trips. This function well aggregate fishing events into
#' trips in 1991 and after to create a consistent form of data.
#' @return
#' A (tibble) data frame.
#' @export
#'
#' @examples
#' \donttest{
#' get_cpue_historic(end_year = 2016) %>%
#'   tidy_cpue_historic(species_common = "pacific cod", area = "5[CD]+")
#' }
tidy_cpue_historic <- function(dat,
                            species_common,
                            year_range = c(1956, 1996),
                            area = "5[CDE]+",
                            depth_bins = seq(0, 350, 25),
                            use_alt_year = FALSE,
                            depth_bin_quantiles = c(0, 1),
                            type = c("trip-level-data", "arithmetic-cpue")) {
  type <- match.arg(type)

  dat <- dat[grepl(area, dat$specific_area), , drop = FALSE]
  dat$area <- gsub("\\[|\\]|\\+", "", area)

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

  fe_dat <- dat %>%
    group_by(year, area, trip_id, fishing_event_id) %>%
    summarise(
      spp_catch = sum(ifelse(species_common_name == species_common, total, 0)),
      hours_fished = mean(hours_fished), # should be 1
      depth = mean(best_depth_m), # should be 1
      month = month[[1]], # should be 1
      # - is for formatting for easy coefficient plotting:
      locality = paste0("-", clean_localities(locality_description[[1]]))
    ) %>%
    ungroup()

  # arith. cpue for target spp:
  cpue_arith <- fe_dat %>%
    group_by(area, year) %>%
    summarise(spp_catch = sum(spp_catch), hours_fished = sum(hours_fished)) %>%
    mutate(cpue_arith = spp_catch / hours_fished) %>%
    ungroup()

  if (type == "trip-level-data") {
    trip_dat <- fe_dat %>%
      group_by(year, area, trip_id, locality) %>%
      summarise(
        spp_catch = sum(spp_catch),
        hours_fished = sum(hours_fished),
        mean_depth = mean(depth),
        geo_mean_depth = exp(mean(log(depth))),
        month = month[[1]]
      ) %>%
      ungroup()

    depth_range <- stats::quantile(trip_dat$mean_depth,
      probs = c(min(depth_bin_quantiles), max(depth_bin_quantiles))
    )

    trip_dat <- trip_dat %>%
      filter(mean_depth >= min(depth_bands) & mean_depth <= max(depth_bands)) %>%
      mutate(
        mean_depth = factor_bin_clean(mean_depth, depth_bands),
        locality = as.factor(locality),
        year_factor = factor_clean(year),
        month = factor_clean(month)
      ) %>%
      mutate(pos_catch = ifelse(spp_catch > 0, 1, 0))
  }

  if (type == "trip-level-data") {
    dplyr::as_tibble(trip_dat)
  } else {
    dplyr::as_tibble(cpue_arith)
  }
}

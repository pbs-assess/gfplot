#' Tidy IPHC survey data
#'
#' **This function determines the qualifying "fleet" for CPUE analyses. It is
#' meant to be used with modern data available at the fishing event level.
#'
#' @param dat An input data frame from [get_cpue_index()].

#' @param year_range The range of years to include.
#' FROM tidy_cpue_index():
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
#'  NEEDED HERE
#' @param species_common The species common name.
#' @param hook_level species-specific hook-level counts from [get_iphc_hooks()].
#' @param skate_info species-independent skate-level information from
#'   [get_iphc_skates_info()].
#' @param set_info species-indepentn set-level information from
#'   [get_iphc_sets_info()].

#'
#' @return tibble with one row for each set in each year, with columns
#'   year, station, lat, lon, E_it (effective skate number), N_it (number of
#'   fish caught of the given species), C_it (catch rate of given species, as
#'   numbers per effective skate), E_it20, N_it20 and C_it20 are the same but
#'   considering the first 20 hooks of each skate only, usable (whether the set
#'   should be used, based on IPHC codes; some cannot be used for geospatial
#'   analysis but are included here), keep (whether to keep the set based on
#'   geography, only latitude for now).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_cpue_index(gear = "bottom trawl")
#' walleye <- tidy_cpue_index(d, "walleye pollock",
#'   area_grep_pattern = "5[CDE]+")
#' }
##'
##' @return
##' @author
tidy_iphc_survey <- function(hook_level, skate_info, set_info,
                             species_common) {
                            # FROM tidy_cpue_index():, decide if need
                            #year_range = c(1996, as.numeric(format(Sys.Date(), "%Y")) - 1),
                            #lat_range = c(48, Inf),
                            #min_positive_tows = 100,
                            #min_positive_trips = 5,
                            #min_yrs_with_trips = 5,
                            #area_grep_pattern = "5[CDE]+",
                            #lat_band_width = 0.1,
                            #depth_band_width = 25,
                            #clean_bins = TRUE,
                            #depth_bin_quantiles = c(0.001, 0.999),
                            #min_bin_prop = 0.001,
                            #lat_bin_quantiles = c(0, 1),
                            #gear = "bottom trawl") {
    # Append some skate info to the hook info, to then calculate counts for
    #**all
    #  hooks and counts for first 20 hooks
    hook_w_skate_info <- left_join(hook_level,
                                  select(skateinfo, setID, skateID,
                                         firstHook, lastHook, hook20),
                                  by = "skateID") %>%
                 # number on hook providing hook is in first 20
                 mutate(numOnHook20 = numOnHook * (hook >= (firstHook -0.1)) *
                                       (hook <= (hook20 + 0.1) ))
# YYR assessment had to hook = NA for six hooks so had to:
# data[which(is.na(data$numOnHook20)), "numOnHook20"] = 0     # so no NA's later

    # Summarise to skate level then join with skate info (to include zero counts)
    skate_counts_nonzero <- summarise(group_by(hook_w_skate_info,
                                               skateID),
                                      species = unique(species),
                                      countPerSkate = sum(numOnHook),
                                      countPerSkate20 = sum(numOnHook20))
    skate_counts <- left_join(skateinfo,
                              skate_counts_nonzero,
                              by = "skateID") %>%
                    mutate(chumCountPerSkate = countPerSkate * (bait == 110),
                           chumCountPerSkate20 = countPerSkate20 * (bait == 110) )
    # To get rid of NA's (needed later):
    skate_counts$species = unique(skate_counts$species)[!is.na(unique(skate_counts$species))]
    skate_counts[is.na(skates_count)] = 0

    # Summarise to set leavel then join with set info (to include zero counts)
    #  and just use chumbait hooks
    set_counts_nonzero <- summarise(group_by(skate_counts, setID),
                                    H_it = sum(chumObsHooksPerSkate),
                                    species = unique(species),
                                    N_it = sum(chumCountPerSkate),
                                    N_it20 = sum(chumCountPerSkate20),
                                    hooksChumRatio = H_it /
                                        sum(obsHooksPerSkate),
                                        # for B.1 in YYR 2014
                                    hooksChumRatio20 =sum(chumObsHooksPerSkate20) /
                                        sum(obsHooksPerSkate))
                                        # for B.4 in YYR 2014, fraction in ():
                                        # tilde{H}_it / H^*_{it}
    set_counts <- = left_join(setinfo,
                              set_counts_nozero,
                              by = "setID") %>%
                    mutate(E_it = effSkateIPHC * hooksChumRatio,
                           E_it20 = effSkateIPHC * hooksChumRatio20) %>%
    #setVals2003toNow = select(setsYYRoverall,
    #                      year,
    #                      station = block,
    #                      lat,
    #                      lon = long,
    #                      E_it,
    #                      N_it,
    #                      E_it20,
    #                      N_it20,
    #                     usable) %>%
                    mutate(C_it = N_it / E_it,
                           C_it20 = N_it20 / E_it20) %>%
                    # To re-order:
                    select(year,
                          station = block,
                          lat,
                          lon = long,
                          E_it,
                          N_it,
                          C_it,
                          E_it20,
                          N_it20,
                          C_it20,
                          usable) %>%
                    as_tibble()
}

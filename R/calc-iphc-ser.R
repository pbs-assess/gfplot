#' Calculate Series A, B, C and D from the IPHC data
#'
#' For a given species and tibble containing one row for each set in each year,
#'  and outpu from [tidy_iphc_survey()] or ***, . Currently just working for
#'  years with hook-by-hook data and all hooks were enumerated (2003-2012,
#'  2014-2017 and likely later), for which we can build all four Series A-D.
#'  Series A - first 20 hooks from each skate, only north of WCVI
#'  Series B - all hooks from each skate, only north of WCVI
#'  Series C - all hooks from each skate, full coast
#'  Series C - first 20 hooks from each skate, full coast
#' @param set_counts species-specific set-level counts from [tidy_iphc_survey()]
#'  or **other.
#' @param lat_cut_off cut off below which sets are excluded for Series A and B
#' @return **tibble with one row for each set in each year, with columns
#'   year, station, lat, lon, E_it (effective skate number), N_it (number of
#'   fish caught of the given species), C_it (catch rate of given species, as
#'   numbers per effective skate), E_it20, N_it20 and C_it20 are the same but
#'   considering the first 20 hooks of each skate only, usable (whether the set
#'   should be used, based on IPHC codes; some cannot be used for geospatial
#'   analysis but are included here).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' **yelloweye = tidy_iphc_survey(get_iphc_hooks("yelloweye rockfish"),
#'                              get_iphc_skates_info(),
#'                              get_iphc_sets_info() )
#' }
calc_iphc_ser <- function(set_counts, lat_cut_off=50.6) {
    ser_A <- filter(set_counts, usable == "Y", lat > lat_cut_off)
    ser_A <- summarise(group_by(ser_A, year),
                       Sets = n(),
                       NoYYR20 = sum(C_it20 == 0) / n(),
                       SampleIT20=mean(C_it20))

    ser_B <- filter(set_counts, usable == "Y", lat > lat_cut_off)
    ser_B <- summarise(group_by(ser_B, year),
                       Sets = n(),
                       NoYYR = sum(C_it == 0) / n(),
                       SampleIT=mean(C_it))
    list(ser_A = ser_A, ser_B = ser_B)
}

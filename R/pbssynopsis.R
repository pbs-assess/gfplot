#' \pkg{PBSsynopsis} package
#'
#' PBS Synopsis Report package
#'
#' See the README on
#' \href{https://github.com/seananderson/pbs-synopsis#readme}{GitHub}
#'
#' @docType package
#' @name pbssynopsis
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr bind_rows case_when pull contains tibble
#' @importFrom rlang enquo quo_name ":=" "!!" .data
#' @importFrom dplyr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline scale_fill_manual
#'   scale_colour_manual scale_x_continuous scale_size_area coord_cartesian
#'   guides geom_point facet_wrap xlab ylab geom_col ylim xlim geom_rect
#'   geom_text scale_fill_continuous

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "SURVEY_SERIES_TYPE_CODE", "trip_start_date", "SPECIES_CODE",
  "SPECIES_COMMON_NAME", "SPECIES_DESC", "SPECIES_SCIENCE_NAME",
  "SURVEY_SERIES_DESC", "SURVEY_SERIES_ID", "SURVEY_SERIES_TYPE_CODE",
  "discarded_kg", "discarded_pcs", "gear", "landed_kg",
  "landed_pcs", "species_common_name", "species_desc", "species_science_name",
  "trip_start_date", "year",

  # weighting:
  "month", "freq", "trip_id", "sample_id", "survey_id", "value", "prop",
  "annual_prop", "quarter", "sum_freq",

  "area_km2",
  "catch_weight",
  "density_kgpm2",
  "fe_end_date",
  "fishing_event_id",
  "grouping1",
  "grouping_code",
  "landed_kg_quarter",
  "landed_kg_year",
  "samp_catch_weight_quarter",
  "samp_trip_catch_weight",
  "sum_weighted_freq1",
  "weight",
  "weighted_freq1",
  "weighted_freq1_scaled",
  "weighted_freq2",
  "weighted_freq2_scaled",
  "weighting1",
  "weighting1_total",
  "weighting2",
  "weighting2_total",

  # plotting:

  "age",
  "maturity_code",
  "n_spp",
  "sex",
  "survey",
  "variable"

  ))

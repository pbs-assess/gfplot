#' gfsynopsis package
#'
#' PBS Synopsis Report package
#'
#' See the README on
#' \href{https://github.com/seananderson/pbs-synopsis#readme}{GitHub}
#'
#' @docType package
#' @name gfsynopsis
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
#' @importFrom rlang enquo quo_name ":=" "!!" .data
#' @importFrom forcats fct_reorder
#' @importFrom assertthat assert_that are_equal
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dplyr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_vline scale_fill_manual
#'   scale_colour_manual scale_x_continuous scale_size_area coord_cartesian
#'   guides geom_point facet_wrap xlab ylab geom_col ylim xlim geom_rect
#'   geom_text scale_fill_continuous geom_line labs scale_y_continuous
#'   guide_legend geom_ribbon element_text scale_shape_manual element_line
#'   geom_path geom_polygon coord_equal stat_summary_hex facet_grid
#'   position_identity
#' @importFrom stats coef model.matrix lm binomial rnorm
#'   update.formula formula as.formula density sd
#' @importFrom methods as
#' @importFrom grDevices chull
#' @import Rcpp
NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") utils::globalVariables(c(
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
  "variable",
  "survey_series_desc",

  # lengths:
  "bin_size", "counts", "proportion", "total",

  # aging precision:
  "age_reading_id", "age_reading_type_code", "ageing_method_desc",
  "ageing_param", "employee_id", "female", "glmm", "has_precision", "mature",
  "mature_at", "maturity_convention_desc", "maturity_convention_description",
  "maturity_convention_maxvalue", "n_employee", "p", "species_code",
  "specimen_id", "specimen_sex_code",

  # cpue:

  "best_date", "best_depth", "fe_start_date", "hours_fished", "latitude",
  "locality_code", "longitude", "n_trips_per_year", "n_years", "pos_catch",
  "scrambled_vessel", "species_category_code", "spp_catch", "spp_in_fe",
  "spp_in_row", "sum_catch", "total_positive_tows", "trips_over_thresh",
  "trips_over_treshold_this_year", "vessel_name",
  "est", "est_log", "lwr", "se_log", "upr",  "est_link", "model", "se_link",
  "n_date", "pars", "par_name", "par_group", "par_name_short", "se",
  "vessel", "pred", "term",

  "PID", "SID", "nepacLLhigh", "isobath",

  # surveys:
  "Var1", "Var2", "X", "Y", "akima_depth","depth", "depth_m",
  "depth_mean", "depth_scaled", "depth_sd", "present", "species", "start_lat",
  "start_lon", "x", "y", "z", "bctopo",

  "maturity_convention_code"

  ))

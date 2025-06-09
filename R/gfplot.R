#' gfplot package
#'
#' gfplot package
#'
#' See the README on
#' \href{https://github.com/seananderson/gfplot#readme}{GitHub}
#'
#' @name gfplot
#' @importFrom dplyr distinct filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr bind_rows case_when pull contains tibble rename as_tibble
#' @importFrom rlang enquo quo_name ":=" "!!" .data sym
#' @importFrom forcats fct_reorder
#' @importFrom assertthat assert_that are_equal
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes aes_string geom_hline geom_vline scale_fill_manual
#'   scale_colour_manual scale_x_continuous scale_size_area coord_cartesian
#'   guides geom_point facet_wrap xlab ylab geom_col ylim xlim geom_rect
#'   geom_text scale_fill_continuous geom_line labs scale_y_continuous
#'   guide_legend geom_ribbon element_text scale_shape_manual element_line
#'   geom_path geom_polygon coord_equal stat_summary_hex facet_grid
#'   position_identity coord_fixed scale_color_viridis_d scale_colour_viridis_d
#' @importFrom stats coef model.matrix lm binomial rnorm
#'   update.formula formula as.formula density sd
#' @importFrom methods as
#' @importFrom grDevices chull
#' @importFrom scales comma
#' @importFrom stats t.test
#' @importFrom utils read.csv
#' @importFrom rosettafish en2fr
# avoid R CMD check warning; import something:
#' @importFrom PBSdata .PBSdataEnv
"_PACKAGE"
NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "SURVEY_SERIES_TYPE_CODE", "trip_start_date", "SPECIES_CODE",
    "SPECIES_COMMON_NAME", "SPECIES_DESC", "SPECIES_SCIENCE_NAME",
    "SURVEY_SERIES_DESC", "SURVEY_SERIES_ID", "SURVEY_SERIES_TYPE_CODE",
    "discarded_kg", "discarded_pcs", "gear", "landed_kg",
    "landed_pcs", "species_common_name", "species_desc", "species_science_name",
    "trip_start_date", "year",

    # binomial_perc:
    "uniroot",

    # extract_maturity_perc, extract_maturity_perc_re, fit_mat_ogive,
    # mat_par_delta_method, plot_mat_annual_ogives
    "family",

    # plot_growth:
    "geom_jitter",

    # plot_lengths:
    "survey_abbrev2",

    # plot_mat_ogive:
    "age_or_length",

    # plot_survey_index:
    "geomean",

    # plot_survey_index:
    "biomass_scaled",

    # split_catch_by_sex:
    "new_weight", "group_name", "group_weight", "est_sample_weight",
    "n_fish_sampled", "mean_weight_kg", "group_n", "weighted.mean",
    "median_prop_ann", "n_events_sampled", "n_fish_by_surv_yr",
    "mean_prop_ann", "mean_ann_weight_kg", "median_prop", "total_ann_samples",
    "total_ann_fish", "mean_prop", "total_survey_samples",
    "total_survey_fish", "split_catch_type",

    # weighting:
    "month", "freq", "trip_id", "sample_id", "survey_id", "value", "prop",
    "annual_prop", "quarter", "sum_freq",

    "adult",
    "area_km2",
    "catch_weight",
    "count",
    "cutoff_day",
    "day_of_year",
    "density_kgpm2",
    "est_sample_mass",
    "fe_end_date",
    "fishing_event_id",
    "grouping1",
    "grouping_code",
    "landed_kg_quarter",
    "landed_kg_year",
    "mass_ratio",
    "mass_ratio_mature",
    "maturity_mass",
    "measured_weight",
    "method",
    "n_mature",
    "n_sampled",
    "n_weights",
    "new_mass",
    "samp_catch_weight_quarter",
    "samp_trip_catch_weight",
    "sum_weighted_freq1",
    "threshold",
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
    "survey_abbrev",

    # lengths:
    "bin_size", "counts", "proportion", "total",

    # aging precision:
    "age_reading_id", "age_reading_type_code", "ageing_method_desc",
    "ageing_param", "employee_id", "female", "glmm", "has_precision", "mature",
    "mature_at", "maturity_convention_desc", "maturity_convention_description",
    "maturity_convention_maxvalue", "n_employee", "p", "species_code",
    "specimen_id", "specimen_sex_code",

    # maturity:
    "n_scaled",

    # cpue:
    "best_date", "best_depth", "fe_start_date", "hours_fished", "latitude",
    "locality_code", "longitude", "n_trips_per_year", "n_years", "pos_catch",
    "scrambled_vessel", "species_category_code", "spp_catch", "spp_in_fe",
    "spp_in_row", "sum_catch", "total_positive_tows", "trips_over_thresh",
    "trips_over_treshold_this_year", "vessel_name",
    "est", "est_log", "lwr", "se_log", "upr", "est_link", "model", "se_link",
    "n_date", "pars", "par_name", "par_group", "par_name_short", "se",
    "vessel", "pred", "term",

    "PID", "SID", "nepacLLhigh", "isobath",

    # surveys:
    "Var1", "Var2", "X", "Y", "akima_depth", "depth", "depth_m",
    "depth_mean", "depth_scaled", "depth_sd", "present", "species", "start_lat",
    "start_lon", "x", "y", "z", "bctopo",

    # survey ts:
    "num_pos_sets", "biomass", "lowerci", "mean_cv", "num_pos_sets",
    "num_sets", "re", "surv_order", "survey_name", "upperci",

    "maturity_convention_code",

    "total_month", "month_jitter", "maturity_name_short", "survey_series_id",
    ".n", "maturity", "maturity_name",

    "ageing_method", "length_bin", "weighted_prop", "year_jitter",

    "vessel_effect", "year_effect",

    "sample_source_code", "keeper", "area",

    "totcatch_kg", "fyear", "SURVEY_ABBREV", "vessel_registration_number",
    "sampling_desc", "mean_num_pos_sets", "cv", "sets",
    "true_b", "true", "mean_num_sets",

    # IPHC survey calculations
    "C_it", "C_it20", "E_it", "E_it20", "H_it", "N_it", "N_it20", "bait",
    "block", "chumCountPerSkate", "chumCountPerSkate20", "chumObsHooksPerSkate",
    "chumObsHooksPerSkate20", "countPerSkate", "countPerSkate20",
    "deplHooksPerSkate", "effSkateIPHC", "firstHook", "hook", "hook20",
    "hooksChumRatio", "hooksChumRatio20", "iphcUsabilityCode", "lastHook",
    "lastHookTemp", "lat", "long", "numOnHook", "numOnHook20", "obsHooksPerSkate",
    "obsHooksPerSkate20", "usable", "setID", "skateID",
    "I_t20BootHigh", "I_t20BootLow", "I_t20BootMean", "I_t20SampleMean",
    "I_tBootCV", "I_tBootHigh", "I_tBootLow", "I_tBootMean", "I_tSampleMean",
    "num_pos", "num_pos20", "num_pos_sets",
    "everything", "prop_empty_sets", "wcvi",
    "catchCount", "effSkate",
    "lon", "spNameIPHC", "specCount", "station",
    "countData1995", "countData2013", "data1996to2002",
    "setData1995", "setData2013", "spNameIPHC", "Sets",


    # others
    "parent_rsty_id", "parent_taxonomic_unit", "row_version", "rsty_id",
    "species_grouping", "taxonomic_rank",

    # other
    "usability_code",

    ".time_diff",
    ".year_start_date",
    "action_start_date",
    "common_df",
    "count_surveys_since_2008",

    # historical CPUE tidy
    "best_depth_m", "locality", "locality_description", "mean_depth",
    "species_ageing_group",

    "avg_value", "min_value", "max_value", "parameter", "type",
    "each_specimen_collected",
    "temp",
    "maturity_assignment",
    "maturity_short_names",

    # cpue map stuff
    "label", "major"
  ))
}


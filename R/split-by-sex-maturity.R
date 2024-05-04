#' Split catch weights by relative weight or counts sampled for sex and/or maturity
#'
#' @param survey_sets Data from [gfdata::get_survey_sets()].
#' @param fish Data from [gfdata::get_survey_samples()] or matching format where female are coded as `sex = 2`.
#' @param catch_variable Which biomass variable will be split. Can be total weight or densities.
#'   Variable by this name will be saved indicating which was used.
#' @param split_by_sex Logical for should catch be split by sex?
#' @param split_by_maturity Logical for should catch be split by a maturity at length threshold?
#' @param immatures_pooled If split by maturity, `TRUE` when immatures not to be split by sex.
#' @param survey List of survey abbreviations. Default `NULL` includes all surveys in `survey_sets`.
#'    Sets without samples will use annual survey mean/median proportions. If no samples were collected,
#'    other surveys in that year may be used (see `prioritize_time` option) or the global proportion.
#'    Because of this, one should only run this function on data of one population and gear type at a
#'    time such that morphological relationships and selectivity are relatively consistent. If you want
#'    only survey and year combinations with sample data, filter for `!is.na(median_prop_ann)`.
#'    All available samples will still be used to determine thresholds.
#' @param years List of years. Default `NULL` includes all years.
#' @param cutoff_quantile Set max cutoff quantile for weight modelled from lengths.
#' @param p_threshold Probability of maturity to split at. Default = `0.5`. Alternatives are `0.05` or `0.95`.
#' @param use_median_ratio If `TRUE`, uses median proportion mature when catch too small to have biological samples collected.
#'    Default is FALSE, which uses mean proportion mature.
#' @param sample_id_re If `TRUE` then the ogive model will include random intercepts
#'    for sample ID.
#' @param year_re Option to have lengths at maturity vary by year.
#' @param split_by_weight Default is `FALSE`, but will automatically be switched to `TRUE` for the
#'    standard biomass variable names (`catch_weight`, `density_kgpm2`, `density_kgkm2`).
#' @param custom_maturity_at A numeric vector of two threshold codes to define
#'    maturity at with the first being for males and the second for females.
#'    Defaults to `NULL`, which brings in default values from maturity assignment
#'    dataframe included with this package.
#'    `NA` in either position will also retain the default.
#' @param min_sample_number If fewer sets sampled than this threshold, or fewer fish sampled than 3x this value,
#'    the proportion for unsampled sets will be based on either all other years of the survey,
#'    or other surveys in the same year, depending on `prioritize_time` option choice.
#' @param prioritize_time Default `NULL` chooses to apply the annual mean (or median if `use_median_ratio = TRUE`)
#'    when annual sample number for a survey are fewer than `min_sample_number` and the SD of observed proportions across all years within a
#'    survey is larger than across all samples within a year. `TRUE` forces this behaviour without comparing SDs.
#'    `FALSE` uses survey level mean/median whenever the annual sample number for that survey is less than the `min_sample_number`.
#' @param custom_length_thresholds Instead of estimating length at maturity, they can be provided as a vector
#'    of length 2, first being for males and the second for females. Defaults to `NULL`
#'    to estimate these lengths from maturity ogives.
#' @param plot Logical for whether to produce plots.

#'
#' @export
#' @importFrom dplyr if_else
#' @examples
#' \dontrun{
#' d_survey_sets <- gfdata::get_survey_sets("pacific cod")
#' d_survey_samples <- gfdata::get_survey_samples("pacific cod")
#'
#' d_by_maturity <- split_catch_by_sex(
#'   d_survey_sets, d_survey_samples,
#'   survey = c("SYN HS", "SYN QCS"),
#'   years = NULL,
#'   plot = TRUE
#' )
#' }
split_catch_by_sex <- function(survey_sets, fish,
                               catch_variable = "catch_weight",
                               split_by_sex = TRUE,
                               split_by_maturity = TRUE,
                               immatures_pooled = FALSE,
                               survey = NULL,
                               years = NULL,
                               min_sample_number = 10,
                               prioritize_time = NULL,
                               cutoff_quantile = 0.9995,
                               p_threshold = 0.5,
                               use_median_ratio = FALSE,
                               sample_id_re = TRUE,
                               year_re = FALSE,
                               split_by_weight = FALSE, # changes internally for some weight-based catch variables
                               custom_maturity_at = NULL,
                               custom_length_thresholds = NULL,
                               plot = FALSE) {
  if (catch_variable == "catch_weight" | catch_variable == "density_kgpm2" | catch_variable == "density_kgkm2") {
    split_by_weight <- TRUE
    }

  if (is.null(survey)) survey <- unique(survey_sets[["survey_abbrev"]])
  if (is.null(years)) years <- unique(survey_sets[["year"]])

  species <- fish$species_common_name[1]

  if(split_by_maturity){
  fish <- fish %>% filter(!is.na(length))
  }

  .d <- survey_sets %>%
    filter(survey_abbrev %in% survey) %>%
    filter(year %in% years)

  if (!("area_swept" %in% colnames(.d))) {
    .d$area_swept1 <- .d$doorspread_m * .d$tow_length_m
    .d$area_swept2 <- .d$doorspread_m * (.d$speed_mpm * .d$duration_min)
    .d$area_swept <- ifelse(!is.na(.d$area_swept1), .d$area_swept1, .d$area_swept2)
  }

  survey_sets <- .d
# browser()
  # check for duplicated fishing_event_ids
  check_for_duplicates <- survey_sets[duplicated(survey_sets$fishing_event_id), ]

  if (nrow(check_for_duplicates) > 0) {
    stop("There are duplicted fishing_event_ids in the survey_sets dataframe.
         Remove these before running this function.")
  }

  if (nrow(fish) > 0) {

    f_fish <- fish %>%
      # filter(!is.na(length)) %>%
      filter(sex == 2) %>%
      mutate(year_f = as.character(year))

    m_fish <- fish %>%
      # filter(!is.na(length)) %>%
      filter(sex == 1) %>%
      mutate(year_f = as.character(year))

    # we want to include fish that were not able to be sexed when not fully splitting by sex
    try(u_fish <- fish %>%
      # filter(!is.na(length)) %>%
      filter(!(sex %in% c(1, 2))) %>%
      mutate(year_f = as.character(year)))

    fish_lengths <- fish %>%
      ## when some surveys or sets of years lack maturity data, can we still get something from other surveys?
      filter(survey_abbrev %in% survey) %>%
      filter(year %in% years) %>%
      filter(!is.na(length))

    # if (split_by_weight) {

      # model weight of fish with only length data
      # TMB model to estimate weight of length only fish
      f_weight <- fit_length_weight(fish_lengths, sex = "female", method == "tmb")
      m_weight <- fit_length_weight(fish_lengths, sex = "male", method == "tmb")

      f_fish <- mutate(f_fish,
        model_weight = exp(f_weight$pars$log_a + f_weight$pars$b * (log(length))) * 1000,
        new_weight = weight
      )

      m_fish <- mutate(m_fish,
        model_weight = exp(m_weight$pars$log_a + m_weight$pars$b * (log(length))) * 1000,
        new_weight = weight
      )

      # for unknown sex fish, use average of male and female coefs
      u_fish <- mutate(u_fish,
                       model_weight = exp(((m_weight$pars$log_a + f_weight$pars$log_a) / 2) + (m_weight$pars$b + f_weight$pars$b) / 2 * (log(length))) * 1000,
                       new_weight = weight
      )

      # only apply simulated weight when below chosen cutoff_quantile
      max_model <- quantile(f_fish$weight, probs = c(cutoff_quantile), na.rm = TRUE)
      f_fish$model_weight[f_fish$model_weight > max_model] <- max_model
      f_fish$new_weight[is.na(f_fish$weight)] <- f_fish$model_weight[is.na(f_fish$weight)]

      max_model_m <- quantile(m_fish$weight, probs = c(cutoff_quantile), na.rm = TRUE)
      m_fish$model_weight[m_fish$model_weight > max_model_m] <- max_model_m
      m_fish$new_weight[is.na(m_fish$weight)] <- m_fish$model_weight[is.na(m_fish$weight)]

      u_fish$model_weight[u_fish$model_weight > max(max_model, max_model_m)] <- max(max_model, max_model_m)
      u_fish$new_weight[is.na(u_fish$weight)] <- u_fish$model_weight[is.na(u_fish$weight)]
    # }

    if (split_by_maturity) {
      if(is.null(custom_length_thresholds)) {

      # does maturity data exist within focal surveys for this species?
      maturity_codes <- unique(fish_lengths$maturity_code)
      fish_w_mat <- filter(fish, !is.na(maturity_code), !is.na(length))

      if (length(maturity_codes) < 3) {
        # if maturity data lacking within focal surveys for this species,
        # does it exists elsewhere in sample data provided?
        maturity_codes <- unique(fish_w_mat$maturity_code)

        if (length(maturity_codes) < 3) {
          # no
          warning("Fewer than 3 maturity codes; returning NULL data.", call. = FALSE)
          return(list(data = survey_sets, maturity = NULL, weight_model = NULL))
        } else {
          # yes
          warning("Fewer than 3 maturity codes available from target surveys,",
                  "but all survey data provided were used to define the split.", call. = FALSE)
        }
      } else {
        # there is enough maturity data in the surveys and years of interest to just use those
        warning("There is enough maturity data in the surveys and years of interest to just use those,",
                "but all survey data provided were used to define the split. If you don't want this,",
                "filter the fish samples to include only surveys and/or years of interest.",
                call. = FALSE)
        # fish <- fish_lengths
      }

      # fish <- filter(fish, !is.na(maturity_code), !is.na(length))

      # Check if only some years without maturity data, and set year_re = FALSE in that case
      years_w_maturity <- fish_w_mat %>%
        group_by(year) %>%
        mutate(maturity_levels = length(unique(maturity_code)))

      levels_per_year <- unique(years_w_maturity$maturity_levels)

      if (max(levels_per_year) < 3) { # NA plus only one recorded maturity level is max ever recorded
        warning("Maturity data not recorded, so catch not split.", call. = FALSE)
        return(list(data = survey_sets, model = NULL))
      }

      if (min(levels_per_year) < 3) { # some years lack maturity data

        if (length(levels_per_year) < 1) { # TODO: check if this threshold should actually be 2
          warning("Maturity data not recorded, so catch not split.", call. = FALSE)
          return(list(data = survey_sets, model = NULL))
        } else {
          warning("Some years lack maturity data, but catch still split.", call. = FALSE)
# browser()
          m <- fit_mat_ogive(fish_w_mat,
                             type = "length",
                             sample_id_re = sample_id_re,
                             usability_codes = NULL,
                             custom_maturity_at = custom_maturity_at)

          if (p_threshold == 0.5) {
            f_fish$threshold <- m$mat_perc$f.p0.5
            m_fish$threshold <- m$mat_perc$m.p0.5
          }
          if (p_threshold == 0.05) {
            f_fish$threshold <- m$mat_perc$f.p0.05
            m_fish$threshold <- m$mat_perc$m.p0.05
          }
          if (p_threshold == 0.95) {
            f_fish$threshold <- m$mat_perc$f.p0.95
            m_fish$threshold <- m$mat_perc$m.p0.95
          }
        }
      } else {
        if (year_re) {
          m <- fit_mat_ogive(fish_w_mat,
                             type = "length",
                             sample_id_re = sample_id_re,
                             year_re = TRUE,
                             usability_codes = NULL,
                             custom_maturity_at = custom_maturity_at)
          if (p_threshold == 0.5) {
            f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.5)
            m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.5)
          }
          if (p_threshold == 0.05) {
            f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.05)
            m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.05)
          }
          if (p_threshold == 0.95) {
            f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.95)
            m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.95)
          }
        } else {
          m <- fit_mat_ogive(fish_w_mat,
                             type = "length",
                             usability_codes = NULL,
                             sample_id_re = sample_id_re,
                             custom_maturity_at = custom_maturity_at)

          # apply global estimates to all catches
          if (p_threshold == 0.5) {
            f_fish$threshold <- m$mat_perc$f.p0.5
            m_fish$threshold <- m$mat_perc$m.p0.5
          }
          if (p_threshold == 0.05) {
            f_fish$threshold <- m$mat_perc$f.p0.05
            m_fish$threshold <- m$mat_perc$m.p0.05
          }
          if (p_threshold == 0.95) {
            f_fish$threshold <- m$mat_perc$f.p0.95
            m_fish$threshold <- m$mat_perc$m.p0.95
          }
          # # or could choose to save sample_id estimates and apply them like this
          # if(p_threshold == 0.5) {
          #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.5)
          #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.5)
          # }
          # if(p_threshold == 0.05) {
          #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.05)
          #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.05)
          # }
          # if(p_threshold == 0.95) {
          #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.95)
          #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.95)
          # }
        }
      }
      } else {
        m <- NULL
        f_fish$threshold <- custom_length_thresholds[2]
        m_fish$threshold <- custom_length_thresholds[1]
      }

      # browser()
      # classify each fish as immature or mature based on above thresholds
      f_fish <- mutate(f_fish, mature = if_else(length >= threshold, 1, 0, missing = NULL))
      m_fish <- mutate(m_fish, mature = if_else(length >= threshold, 1, 0, missing = NULL))

      imm_fish <- NULL
      mat_fish <- NULL

      # create groups
      if (split_by_sex) {
        if (immatures_pooled) {
          try(imm_fish <- u_fish %>%
            filter(length < min(c(f_fish$threshold, m_fish$threshold), na.rm = TRUE)) %>%
            mutate(mature = 0))

          # since not splitting by sex for immatures, we want to include immatures that were not able to be sexed
          fish_groups <- bind_rows(f_fish, m_fish, imm_fish) %>%
            # but only when sexes where collected for other specimens
            filter(fishing_event_id %in% unique(f_fish$fishing_event_id, m_fish$fishing_event_id)) %>%
            mutate(group_name = ifelse(mature == 1,
              paste("Mature", ifelse(sex == 1, "males", "females")),
              "Immature"
            ))
        } else {
          fish_groups <- rbind(f_fish, m_fish) %>%
            mutate(group_name = ifelse(mature == 1,
              paste("Mature", ifelse(sex == 1, "males", "females")),
              paste("Immature", ifelse(sex == 1, "males", "females"))
            ))
        }
      } else {

        try(mat_fish <- u_fish %>%
          filter(length >= mean(c(f_fish$threshold, m_fish$threshold), na.rm = TRUE)) %>%
          mutate(mature = 1))

        try(imm_fish <- u_fish %>%
          filter(length < mean(c(f_fish$threshold, m_fish$threshold), na.rm = TRUE)) %>%
          mutate(mature = 0))

        fish_groups <- rbind(f_fish, m_fish, imm_fish, mat_fish) %>%
          mutate(group_name = ifelse(mature == 1, "Mature", "Immature"))
      }
    } else {
      # just split by sex
      fish_groups <- rbind(f_fish, m_fish) %>%
        mutate(group_name = ifelse(sex == 1, "Males", "Females"))
    }

    # list of all event group combos
    all_groups_for_all_events <- expand.grid(
      fishing_event_id = unique(fish_groups$fishing_event_id),
      group_name = unique(fish_groups$group_name)
    )

    # split by weight ----
    if (split_by_weight) {

      group_values <- fish_groups %>%
        filter(!is.na(new_weight)) %>%
        group_by(fishing_event_id, group_name) %>%
        mutate(group_weight = sum(new_weight)) %>%
        dplyr::add_tally() %>%
        rename(group_n = n) %>%
        ungroup()

      set_values <- group_values %>%
        group_by(fishing_event_id) %>%
        filter(!is.na(new_weight)) %>%
        dplyr::add_tally() %>%
        rename(n_fish_sampled = n) %>%
        mutate(
          est_sample_weight = sum(new_weight, na.rm = TRUE),
          proportion = group_weight / est_sample_weight,
          measured_weight = sum(weight),
          n_weights = sum(!is.na(weight))
        ) %>%
        ungroup() %>%
        select(
          fishing_event_id,
          n_fish_sampled, # these are optional data checks
          est_sample_weight, # needed for perc_sampled data check below
          measured_weight,
          n_weights,
          # maturity_class_weight,
          group_name,
          group_n,
          group_weight,
          proportion
        ) %>%
        unique()


      # add 0 when a particular group was not found
      set_values_filled <- left_join(all_groups_for_all_events, set_values) %>%
        group_by(fishing_event_id) %>%
        # duplicate event level values for all groups including those not represented in the sample
        mutate(
          est_sample_weight = mean(est_sample_weight, na.rm = TRUE),
          n_fish_sampled = mean(n_fish_sampled, na.rm = TRUE),
          measured_weight = mean(measured_weight, na.rm = TRUE),
          n_weights = mean(n_weights, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        replace(is.na(.), 0) # seems to be needed

      # add NA when a particular group was not found
      # should give df with nrows in survey_sets * number of unique group_names
      all_groups_for_all_events2 <- expand.grid(
        fishing_event_id = unique(survey_sets$fishing_event_id),
        group_name = unique(set_values$group_name)
      )

# browser()

      survey_sets2 <- left_join(all_groups_for_all_events2, survey_sets)
      sets_w_ratio <- left_join(survey_sets2, set_values_filled) %>%
        group_by(fishing_event_id) %>%
        mutate(
          mean_weight_kg = max(est_sample_weight, na.rm = TRUE)/max(n_fish_sampled, na.rm = TRUE)/1000
        ) %>%
        ungroup() %>%
        mutate(
          # removes false 0s if they were introduced a few lines above
          # I'm not sure if this actually happens
          mean_weight_kg = ifelse(n_fish_sampled==0, NA_real_, mean_weight_kg),
          proportion = ifelse(n_fish_sampled==0, NA_real_, proportion)
          ) %>%
        group_by(group_name, survey_series_id, year) %>%
        mutate(
          n_events_sampled = sum(!is.na(proportion)),
          n_fish_by_surv_yr = sum(n_fish_sampled, na.rm = TRUE),
          # median_prop_ann = ifelse(all(is.na(proportion)), NA_real_, spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE)),
          # mean_prop_ann = weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE),
          median_prop_ann = ifelse(all(is.na(proportion)), NA_real_, median(proportion, w = n_fish_sampled, na.rm = TRUE)),
          mean_prop_ann = mean(proportion, na.rm = TRUE),
          mean_ann_weight_kg = weighted.mean(mean_weight_kg, w = n_fish_sampled, na.rm = TRUE)
          # mean_ann_weight_kg = mean(mean_weight_kg, na.rm = TRUE)
        ) %>%
        ungroup()

      # currently using unweighted because we are extrapolating to unsampled locations, so location should be unit of measurement
      # if weighted turns out to be better, turn this warning on and document change, possibly even rename function?
      # warning("This function uses means or medians weighted by the number
      #         of fish sampled for aggregating ratios at all scales.")

      # generated proportions for use on unsampled sets ----

      if(is.null(prioritize_time)){

      within_yr <- sets_w_ratio %>% filter(!is.na(proportion)) %>%
        group_by(group_name, year) %>%
        dplyr::add_tally() %>% filter(n >= min_sample_number) %>%
        summarise(sd = sd(proportion, na.rm = TRUE),
                  se = sd(proportion, na.rm = TRUE)/sqrt(length(proportion))
                  )

      btw_yr <- sets_w_ratio %>% filter(!is.na(proportion)) %>%
        group_by(group_name, survey_series_id) %>%
        dplyr::add_tally() %>% filter(n >= min_sample_number) %>%
        summarise(sd = sd(proportion, na.rm = TRUE),
                  se = sd(proportion, na.rm = TRUE)/sqrt(length(proportion))
                  )

      # mean(within_yr$sd, na.rm = TRUE)
      # mean(btw_yr$sd, na.rm = TRUE)
      # browser()

      # calculated SE too, in case SD was too sensitive but SD seems to be working
        if(mean(btw_yr$sd, na.rm = TRUE) > mean(within_yr$sd, na.rm = TRUE)){
          prioritize_time = TRUE
        } else {
          prioritize_time = FALSE
        }
      }

      if(prioritize_time){
      # some survey years are missing sample data, so try annual means for these surveys, which tend to not catch anyway
      sets_w_ratio <- sets_w_ratio %>% group_by(group_name, year) %>%
        mutate(
          use_within_yr_prop = TRUE,
          total_ann_samples = sum(!is.na(proportion)),
          total_ann_fish = sum(n_fish_sampled, na.rm = TRUE),
          # median_prop = ifelse(all(is.na(proportion)), NA_real_,
          #                      ifelse(is.na(median_prop_ann)| n_events_sampled < min_sample_number,
          #                      round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
          #                      median_prop_ann
          #                      )),
          # mean_prop = ifelse(is.na(mean_prop_ann)| n_events_sampled < min_sample_number,
          #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
          #                    mean_prop_ann
          #                    ),
          median_prop = ifelse(all(is.na(proportion)), NA_real_,
                               ifelse(is.na(median_prop_ann)|
                                        n_events_sampled < min_sample_number|
                                          n_fish_by_surv_yr < min_sample_number*3,
                                      round(median(proportion, na.rm = TRUE), 3),
                                      median_prop_ann
                               )),
          mean_prop = ifelse(is.na(mean_prop_ann)|
                               n_events_sampled < min_sample_number|
                                  n_fish_by_surv_yr < min_sample_number*3,
                             round(mean(proportion, na.rm = TRUE), 3),
                             mean_prop_ann
          ),
          # mean_weight_kg = ifelse(is.na(mean_ann_weight_kg),
          #                         round(weighted.mean(mean_weight_kg, w = n_fish_sampled, na.rm = TRUE), 3), mean_weight_kg)
          mean_weight_kg = ifelse(is.na(mean_ann_weight_kg),
                                  round(mean(mean_weight_kg,na.rm = TRUE), 3), mean_weight_kg)
        ) %>% ungroup() %>%
        # some surveys are missing data for some years so we use survey level means in these years
        group_by(group_name, survey_series_id) %>%
        mutate(
          total_survey_samples = sum(!is.na(proportion)),
          total_survey_fish = sum(n_fish_sampled, na.rm = TRUE),
          # median_prop = ifelse(all(is.na(proportion)), NA_real_,
          #                      ifelse(is.na(median_prop)|total_ann_samples < min_sample_number,
          #                      round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
          #                      median_prop
          #                      )),
          # mean_prop = ifelse(is.na(mean_prop)|total_ann_samples < min_sample_number,
          #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
          #                    mean_prop
          #                    ),
          median_prop = ifelse(all(is.na(proportion)), NA_real_,
                               ifelse(is.na(median_prop)|
                                        total_ann_samples < min_sample_number|
                                          total_ann_fish < min_sample_number*3,
                                      round(median(proportion, na.rm = TRUE), 3),
                                      median_prop
                               )),
          mean_prop = ifelse(is.na(mean_prop)|
                               total_ann_samples < min_sample_number|
                                 total_ann_fish < min_sample_number*3,
                             round(mean(proportion, na.rm = TRUE), 3),
                             mean_prop
          ),
          # mean_weight_kg = ifelse(is.na(mean_weight_kg),
          #                         round(weighted.mean(mean_weight_kg, w = n_fish_sampled, na.rm = TRUE), 3), mean_weight_kg)
          mean_weight_kg = ifelse(is.na(mean_weight_kg),
                                  round(mean(mean_weight_kg, na.rm = TRUE), 3), mean_weight_kg)
        ) %>%
        ungroup() %>%
        # if that fails use global means
        group_by(group_name) %>%
        mutate(
          total_samples = sum(!is.na(proportion)),
          # median_prop = ifelse(all(is.na(proportion)), NA_real_,
          #                      ifelse(is.na(median_prop)|total_survey_samples < min_sample_number,
          #                      round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
          #                      median_prop
          #                      )),
          # mean_prop = ifelse(is.na(mean_prop)|total_survey_samples < min_sample_number,
          #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
          #                    mean_prop
          #                    ),
          median_prop = ifelse(all(is.na(proportion)), NA_real_,
                               ifelse(is.na(median_prop)|
                                        total_survey_samples < min_sample_number|
                                          total_survey_fish < min_sample_number*3,
                                      round(median(proportion, na.rm = TRUE), 3),
                                      median_prop
                               )),
          mean_prop = ifelse(is.na(mean_prop)|
                               total_survey_samples < min_sample_number|
                                  total_survey_fish < min_sample_number*3,
                             round(mean(proportion, na.rm = TRUE), 3),
                             mean_prop
          ),
          # mean_weight_kg = ifelse(is.na(mean_weight_kg),
          #                         round(weighted.mean(mean_weight_kg, w = n_fish_sampled, na.rm = TRUE), 3), mean_weight_kg)
          mean_weight_kg = ifelse(is.na(mean_weight_kg),
                                  round(mean(mean_weight_kg, na.rm = TRUE), 3), mean_weight_kg)
        ) %>%
        ungroup()
      } else {
          # skip straight to survey level means when missing or insufficient data for some years
        sets_w_ratio <- sets_w_ratio %>% group_by(group_name, year) %>%
          mutate(
            use_within_yr_prop = FALSE,
            total_ann_samples = sum(!is.na(proportion))
          ) %>% ungroup() %>%
          group_by(group_name, survey_series_id) %>%
          mutate(
            total_survey_samples = sum(!is.na(proportion)),
            total_survey_fish = sum(n_fish_sampled, na.rm = TRUE),
            # median_prop = ifelse(all(is.na(proportion)), NA_real_,
            #                      ifelse(is.na(median_prop_ann)| n_events_sampled < min_sample_number,
            #                      round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                      median_prop_ann
            # )),
            # mean_prop = ifelse(is.na(mean_prop_ann)| n_events_sampled < min_sample_number,
            #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                    mean_prop_ann
            # )
            median_prop = ifelse(all(is.na(proportion)), NA_real_,
                                 ifelse(is.na(median_prop_ann)|
                                          n_events_sampled < min_sample_number|
                                            n_fish_by_surv_yr < min_sample_number*3,
                                        round(median(proportion, na.rm = TRUE), 3),
                                        median_prop_ann
                                 )),
            mean_prop = ifelse(is.na(mean_prop_ann)|
                                 n_events_sampled < min_sample_number|
                                    n_fish_by_surv_yr < min_sample_number*3,
                               round(mean(proportion, na.rm = TRUE), 3),
                               mean_prop_ann
            ),
            # mean_weight_kg = ifelse(is.na(mean_ann_weight_kg),
            #                         round(weighted.mean(mean_weight_kg, w = n_fish_sampled, na.rm = TRUE), 3), mean_weight_kg)
            mean_weight_kg = ifelse(is.na(mean_ann_weight_kg),
                                    round(mean(mean_weight_kg, na.rm = TRUE), 3), mean_weight_kg)
          ) %>%
          ungroup() %>%
          # if that fails use global means
          group_by(group_name) %>%
          mutate(
            total_samples = sum(!is.na(proportion)),
            total_fish = sum(n_fish_sampled, na.rm = TRUE),
            # median_prop = ifelse(all(is.na(proportion)), NA_real_,
            #                      ifelse(is.na(median_prop)|total_survey_samples < min_sample_number,
            #                      round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                      median_prop
            # )),
            # mean_prop = ifelse(is.na(mean_prop)|total_survey_samples < min_sample_number,
            #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                    mean_prop
            # ),
            median_prop = ifelse(all(is.na(proportion)), NA_real_,
                                 ifelse(is.na(median_prop)|
                                          total_survey_samples < min_sample_number|
                                            total_survey_fish < min_sample_number*3,
                                        round(median(proportion, na.rm = TRUE), 3),
                                        median_prop
                                 )),
            mean_prop = ifelse(is.na(mean_prop)|
                                 total_survey_samples < min_sample_number|
                                   total_survey_fish < min_sample_number*3,
                               round(mean(proportion, na.rm = TRUE), 3),
                               mean_prop
            ),
            # mean_weight_kg = ifelse(is.na(mean_weight_kg),
            #                         round(weighted.mean(mean_weight_kg, w = n_fish_sampled, na.rm = TRUE), 3), mean_weight_kg)
            mean_weight_kg = ifelse(is.na(mean_weight_kg),
                                    round(mean(mean_weight_kg, na.rm = TRUE), 3), mean_weight_kg)
          ) %>%
          ungroup()
      }

      sets_w_ratio <- sets_w_ratio %>%
        mutate(
          # replace NAs with 0 for all event group combinations not represented in the samples
          group_n = ifelse(is.na(group_n), 0, group_n),
          group_weight = ifelse(is.na(group_weight), 0, group_weight),
          est_sample_weight = ifelse(is.na(est_sample_weight), 0, est_sample_weight),
          n_fish_sampled = ifelse(is.na(n_fish_sampled), 0, n_fish_sampled),
          measured_weight = ifelse(is.na(measured_weight), 0, measured_weight),
          n_weights = ifelse(is.na(n_weights), 0, n_weights)
        )


      # chose value to use when a sample weight ratio is not available
      if (use_median_ratio) {
        sets_w_ratio <- sets_w_ratio %>%
          mutate(
            proportion = ifelse(is.na(proportion), median_prop, proportion)
          )
      } else {
        sets_w_ratio <- sets_w_ratio %>%
          mutate(
            proportion = ifelse(is.na(proportion), mean_prop, proportion)
          )
      }

      # add column to check for discrepancies between total catch weight and biological sample weights
      # est_sample_weight is in g while catch_weight is in kg
      sets_w_ratio$unsampled_catch <- round((sets_w_ratio$catch_weight - (sets_w_ratio$est_sample_weight / 1000)), 2)
      sets_w_ratio$perc_sampled <- round((sets_w_ratio$est_sample_weight / 1000) / sets_w_ratio$catch_weight, 2) * 100

      # estimate a catch count using mean weight of individuals sampled or survey-level means when no samples
      sets_w_ratio$est_catch_count <- sets_w_ratio$catch_weight / sets_w_ratio$mean_weight_kg

      # sets_w_ratio$perc_sampled[Inf] <- NA

      # correct any false 0s and resulting NAs in set weight variables
      global_mean_weight <- mean(fish$weight, na.rm = TRUE)

      if (("catch_weight" %in% colnames(.d)) & ("catch_count" %in% colnames(.d))) {
        # browser()
        .d <- sets_w_ratio
        .d$catch_count <- ifelse(.d$catch_weight > 0 & .d$catch_count == 0, NA, .d$catch_count)
        .d$catch_weight <- ifelse(.d$catch_count > 0 & .d$catch_weight == 0, NA, .d$catch_weight)
        .d <- .d %>% mutate(catch_weight = case_when(
            is.na(catch_weight) & catch_count == n_fish_sampled ~ est_sample_weight / 1000,
            is.na(catch_weight) & catch_count > n_fish_sampled & n_fish_sampled > 0 ~ (est_sample_weight / 1000 / n_fish_sampled) * catch_count,
            # is.na(catch_weight) & n_fish_sampled == 0 ~ global_mean_weight*catch_count,
            is.na(catch_weight) & n_fish_sampled == 0 ~ NA,
            !is.na(catch_weight) ~ catch_weight))


        if (("density_pcpm2" %in% colnames(.d))) {
          .d$density_pcpm2 <- ifelse(.d$catch_count > 0 & .d$density_pcpm2 == 0, NA, .d$density_pcpm2)
        }
        if (("density_kgpm2" %in% colnames(.d))) {
          .d$density_kgpm2 <- ifelse(.d$catch_weight > 0 & .d$density_kgpm2 == 0, NA, .d$density_kgpm2)
          .d <- .d %>% mutate(
            density_kgpm2 = case_when(
              is.na(density_kgpm2) ~ catch_weight / area_swept,
              !is.na(density_kgpm2) ~ density_kgpm2
            )
          )
        }
        sets_w_ratio <- .d
      }

    } else {
      # if splitting proportion of a count ----
      # this hasn't yet been tested
      warning("Splitting count data hasn't been tested for this function yet.
              Recommend stepping through this part of the function the first time you use it.")
      # browser()
      group_values <- fish_groups %>%
        group_by(fishing_event_id, group_name) %>%
        dplyr::add_tally() %>%
        rename(group_n = n) %>%
        ungroup()

      set_values <- group_values %>%
        group_by(fishing_event_id) %>%
        dplyr::add_tally() %>%
        rename(n_fish_sampled = n) %>%
        mutate(
          est_sample_weight = sum(new_weight),
          mean_weight_kg = (est_sample_weight / n_fish_sampled)/ 1000,
          proportion = group_n / n_fish_sampled
        ) %>%
        ungroup() %>%
        select(
          fishing_event_id,
          n_fish_sampled,
          group_name,
          group_n,
          mean_weight_kg,
          proportion
        ) %>%
        unique()

      # add 0 when a particular group was not found
      set_values_filled <- left_join(all_groups_for_all_events, set_values) %>%
        group_by(fishing_event_id) %>%
        # duplicate event level values for all groups including those not represented in the sample
        mutate(n_fish_sampled = mean(n_fish_sampled, na.rm = TRUE)) %>%
        ungroup() %>%
        replace(is.na(.), 0)

      # add NA when a particular group was not found
      # should give df with nrows in survey_sets * number of unique group_names
      all_groups_for_all_events2 <- expand.grid(
        fishing_event_id = unique(survey_sets$fishing_event_id),
        group_name = unique(set_values$group_name)
      )

      # generated proportions for use on unsampled sets ----
      survey_sets2 <- left_join(all_groups_for_all_events2, survey_sets)
      sets_w_ratio <- left_join(survey_sets2, set_values_filled) %>%
        mutate(
          # removes false 0s introduced a few lines above
          proportion = ifelse(n_fish_sampled==0, NA_real_, proportion)
        ) %>%
        group_by(group_name, survey_series_id, year) %>%
        mutate(
          n_events_sampled = sum(!is.na(proportion)),
          median_prop_ann = median(proportion, na.rm = TRUE),
          mean_prop_ann = mean(proportion, na.rm = TRUE),
          # median_prop_ann = ifelse(all(is.na(proportion)), NA_real_, spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE)),
          # mean_prop_ann = weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE)
        ) %>%
        ungroup()

      # generated proportions for use on unsampled sets ----
      if(is.null(prioritize_time)){

        within_yr <- sets_w_ratio %>% filter(!is.na(proportion)) %>%
          group_by(group_name, year) %>%
          dplyr::add_tally() %>% filter(n >= min_sample_number) %>%
          summarise(sd = sd(proportion, na.rm = TRUE),
                    se = sd(proportion, na.rm = TRUE)/sqrt(length(proportion))
          )

        btw_yr <- sets_w_ratio %>% filter(!is.na(proportion)) %>%
          group_by(group_name, survey_series_id) %>%
          dplyr::add_tally() %>% filter(n >= min_sample_number) %>%
          summarise(sd = sd(proportion, na.rm = TRUE),
                    se = sd(proportion, na.rm = TRUE)/sqrt(length(proportion))
          )

        # mean(within_yr$sd, na.rm = TRUE)
        # mean(btw_yr$sd, na.rm = TRUE)
        # browser()

        # calculated SE too, in case SD was too sensitive but SD seems to be working
        if(mean(btw_yr$sd, na.rm = TRUE) > mean(within_yr$sd, na.rm = TRUE)){
          prioritize_time = TRUE
        } else {
          prioritize_time = FALSE
        }
      }

      if(prioritize_time){
        # some survey years are missing sample data, so try annual means for these surveys, which tend to not catch anyway
        sets_w_ratio <- sets_w_ratio %>% group_by(group_name, year) %>%
          mutate(
            use_within_yr_prop = TRUE,
            total_ann_samples = sum(!is.na(proportion)),
            # median_prop = ifelse(all(is.na(proportion)), NA_real_,
            #                      ifelse(is.na(median_prop_ann)| n_events_sampled < min_sample_number,
            #                             round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                             median_prop_ann
            #                      )),
            # mean_prop = ifelse(is.na(mean_prop_ann)| n_events_sampled < min_sample_number,
            #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                    mean_prop_ann
            # )
            median_prop = ifelse(is.na(median_prop_ann)| n_events_sampled < min_sample_number,
                                 round(median(proportion, na.rm = TRUE), 3),
                                 median_prop_ann
            ),
            mean_prop = ifelse(is.na(mean_prop_ann)| n_events_sampled < min_sample_number,
                               round(mean(proportion, na.rm = TRUE), 3),
                               mean_prop_ann
            )
          ) %>% ungroup() %>%
          # some surveys are missing data for some years so we use survey level means in these years
          group_by(group_name, survey_series_id) %>%
          mutate(
            total_survey_samples = sum(!is.na(proportion)),
            # median_prop = ifelse(all(is.na(proportion)), NA_real_,
            #                      ifelse(is.na(median_prop)|total_ann_samples < min_sample_number,
            #                             round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                             median_prop
            #                      )),
            # mean_prop = ifelse(is.na(mean_prop)|total_ann_samples < min_sample_number,
            #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                    mean_prop
            # )
            median_prop = ifelse(is.na(median_prop)|total_ann_samples < min_sample_number,
                                 round(median(proportion, na.rm = TRUE), 3),
                                 median_prop
            ),
            mean_prop = ifelse(is.na(mean_prop)|total_ann_samples < min_sample_number,
                               round(mean(proportion, na.rm = TRUE), 3),
                               mean_prop
            )
          ) %>%
          ungroup() %>%
          # if that fails use global means
          group_by(group_name) %>%
          mutate(
            total_samples = sum(!is.na(proportion)),
            # median_prop = ifelse(all(is.na(proportion)), NA_real_,
            #                      ifelse(is.na(median_prop)|total_survey_samples < min_sample_number,
            #                             round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                             median_prop
            #                      )),
            # mean_prop = ifelse(is.na(mean_prop)|total_survey_samples < min_sample_number,
            #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                    mean_prop
            # )
            median_prop = ifelse(is.na(median_prop)|total_survey_samples < min_sample_number,
                                 round(median(proportion, na.rm = TRUE), 3),
                                 median_prop
            ),
            mean_prop = ifelse(is.na(mean_prop)|total_survey_samples < min_sample_number,
                               round(mean(proportion, na.rm = TRUE), 3),
                               mean_prop
            )
          ) %>%
          ungroup()
      } else {
        # skip straight to survey level means when missing or insufficient data for some years
        sets_w_ratio <- sets_w_ratio %>% group_by(group_name, year) %>%
          mutate(
            use_within_yr_prop = FALSE,
            total_ann_samples = sum(!is.na(proportion))
          ) %>% ungroup() %>%
          group_by(group_name, survey_series_id) %>%
          mutate(
            total_survey_samples = sum(!is.na(proportion)),
            median_prop = ifelse(all(is.na(proportion)), NA_real_,
                                 ifelse(is.na(median_prop_ann)| n_events_sampled < min_sample_number,
                                        round(median(proportion, na.rm = TRUE), 3),
                                        median_prop_ann
                                 )),
            mean_prop = ifelse(is.na(mean_prop_ann)| n_events_sampled < min_sample_number,
                               round(mean(proportion, na.rm = TRUE), 3),
                               mean_prop_ann
            )
          ) %>%
          ungroup() %>%
          # if that fails use global means
          group_by(group_name) %>%
          mutate(
            total_samples = sum(!is.na(proportion)),
            # median_prop = ifelse(all(is.na(proportion)), NA_real_,
            #                      ifelse(is.na(median_prop)|total_survey_samples < min_sample_number,
            #                             round(spatstat.geom::weighted.median(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                             median_prop
            #                      )),
            # mean_prop = ifelse(is.na(mean_prop)|total_survey_samples < min_sample_number,
            #                    round(weighted.mean(proportion, w = n_fish_sampled, na.rm = TRUE), 3),
            #                    mean_prop
            # )
            median_prop = ifelse(all(is.na(proportion)), NA_real_,
                                 ifelse(is.na(median_prop)|total_survey_samples < min_sample_number,
                                        round(median(proportion, na.rm = TRUE), 3),
                                        median_prop
                                 )),
            mean_prop = ifelse(is.na(mean_prop)|total_survey_samples < min_sample_number,
                               round(mean(proportion, na.rm = TRUE), 3),
                               mean_prop
            )
          ) %>%
          ungroup()
      }

      sets_w_ratio <- sets_w_ratio %>%
        mutate(
          # replace NAs with 0 for all event group combinations not represented in the samples
          group_n = ifelse(is.na(group_n), 0, group_n),
          n_fish_sampled = ifelse(is.na(n_fish_sampled), 0, n_fish_sampled)
        )

      # chose value to use when a sample weight ratio is not available
      if (use_median_ratio) {
        sets_w_ratio <- sets_w_ratio %>%
          mutate(
            proportion = ifelse(is.na(proportion), median_prop, proportion)
          )
      } else {
        sets_w_ratio <- sets_w_ratio %>%
          mutate(
            proportion = ifelse(is.na(proportion), mean_prop, proportion)
          )
      }

      if ("catch_weight" %in% colnames(.d)){
        sets_w_ratio$est_catch_count <- sets_w_ratio$catch_weight / sets_w_ratio$mean_weight_kg
      }

      # add column to check for discrepancies between total catch count and biological sample count
      # fist were there any catches with weights and samples but no counts?
      sets_w_ratio$catch_count <- ifelse(
        !(sets_w_ratio$catch_count > 0) & sets_w_ratio$catch_weight > 0 & sets_w_ratio$n_fish_sampled > 0,

        ## we could assume everything was sampled
        ## this could be checked by summing weights of samples and comparing that to total catch weight
        # sets_w_ratio$n_fish_sampled,

        ## or we can use the estimated count based on the mean weights measured
        sets_w_ratio$est_catch_count,

        sets_w_ratio$catch_count
        )

      sets_w_ratio$unsampled_catch <- round((sets_w_ratio$catch_count - (sets_w_ratio$n_fish_sampled)), 2)
      sets_w_ratio$perc_sampled <- round((sets_w_ratio$n_fish_sampled) / sets_w_ratio$catch_count, 2) * 100

      # sets_w_ratio$perc_sampled[Inf] <- NA
    }

    sets_w_ratio$split_catch_type <- sets_w_ratio[[catch_variable]]

    data <- sets_w_ratio %>%
      mutate(
        group_catch_est = split_catch_type * proportion
      )

    data$split_catch_type <- catch_variable

    data <- data[,which(unlist(lapply(data, function(x)!all(is.na(x))))), with=FALSE]

    if (plot) {

      fish_w_maturity <- filter(fish, !is.na(length) & maturity_code > 0)
      survey_names <- unique(fish_w_maturity$survey_abbrev)
      ssid_string <- paste(survey_names, collapse = " ")

      maturity_plot <- NA

      try(
        (maturity_plot <- plot_mat_ogive(m) +
          ggplot2::ggtitle(paste("Length at maturity for", species, "surveys", ssid_string, "")))
      )

      weight_plot <- NA
      if (split_by_weight){
      try(
        (weight_plot <- ggplot(fish_groups, aes(length, new_weight, colour = as.factor(sex))) +
          geom_point(size = 1.5, alpha = 0.35, shape = 1) +
          geom_point(aes(length, weight), shape = 16, size = 1.25, alpha = 0.65) +
          scale_color_viridis_d(begin = 0.1, end = 0.6) +
          facet_wrap(~year) +
          gfplot::theme_pbs() +
          xlab("") +
          ylab("Weight (open circles are estimates)") +
          labs(colour = "Sex") +
          ggplot2::ggtitle(paste("Length-weight relationship for", species, "surveys", ssid_string, ""))
        )
      )
      }
    }
  } else {
    return(list(data = survey_sets, model = NULL))
  }

  if (split_by_maturity) {
    # if(is.null(custom_length_thresholds)) {
      if (plot) {
        list(data = data, m = m, maturity_plot = maturity_plot, weight_plot = weight_plot)
      } else {
        list(data = data, m = m)
      }
   # } else {
   #   if (plot) {
   #   list(data = data, model = NULL, maturity_plot = maturity_plot, weight_plot = weight_plot)
   #   } else {
   #     list(data = data, model = NULL)
   #   }
   # }
  } else {
    list(data = data, model = NULL)
  }
}

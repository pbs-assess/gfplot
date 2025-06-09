#' Split catch densities by mass ratio of maturity classes
#'
#' @param survey_sets Data from [gfdata::get_survey_sets()].
#' @param fish Data from [gfdata::get_survey_samples()].
#' @param split_dens_type Which density type will be split and saved as a column providing units for
#'     adult_density and imm_density columns.
#' @param survey List of survey abbreviations.
#' @param years List of years. Default 'NULL' includes all years.
#' @param cutoff_quantile Set max cutoff quantile for mass modeled from lengths.
#' @param p_threshold Probability of maturity to split at. Default = 0.5. Alternatives are 0.05 or 0.95.
#' @param use_median_ratio If TRUE, uses median proportion mature (if FALSE uses mean proportion mature)
#'    when catch too small to have biological samples collected.
#' @param sample_id_re If `TRUE` then the model will include random intercepts
#'    for sample ID.
#' @param year_re Option to have lengths at maturity vary by year, but this requires the gfvelocities package.
#' @param plot Logical for whether to produce plots
#'    (length-weight and length-at-maturity relationships).
#' @export
#' @importFrom dplyr if_else
#' @examples
#' \dontrun{
#' d_survey_sets <- gfdata::get_survey_sets("pacific cod")
#' d_survey_samples <- gfdata::get_survey_samples("pacific cod")
#'
#' d_by_maturity <- split_catch_maturity(
#'   d_survey_sets, d_survey_samples,
#'   survey = c("SYN HS", "SYN QCS"),
#'   years = NULL,
#'   plot = TRUE
#' )
#' }
split_catch_maturity <- function(survey_sets, fish,
                                 split_dens_type = "density_kgpm2",
                                 survey = c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG"),
                                 years = NULL,
                                 cutoff_quantile = 0.9995,
                                 p_threshold = 0.5,
                                 use_median_ratio = TRUE,
                                 sample_id_re = TRUE,
                                 year_re = FALSE,
                                 plot = FALSE) {
  if (is.null(years)) years <- unique(survey_sets[["year"]])

  species <- fish$species_common_name[1]

  fish <- fish %>%
    filter(survey_abbrev %in% survey) %>%
    filter(year %in% years)

  survey_sets <- survey_sets %>%
    filter(survey_abbrev %in% survey) %>%
    filter(year %in% years)

  model_ssid <- unique(survey_sets$survey_series_id)
  ssid_string <- paste(model_ssid, collapse = " ")

  # does maturity data exist at all for this species?
  maturity_codes <- unique(fish$maturity_code)

  if (length(maturity_codes) < 3) {
    warning("Fewer than 3 maturity codes; returning NULL data.", call. = FALSE)
    return(list(data = survey_sets, maturity = NULL, mass_model = NULL))
  }

  if (nrow(fish) > 0) {
    # TMB model to estimate mass of length only fish
    f_mass <- fit_length_weight(fish, sex = "female", method == "tmb")
    m_mass <- fit_length_weight(fish, sex = "male", method == "tmb")

    f_fish <- fish %>%
      filter(sex == 2) %>%
      mutate(year_f = as.character(year))
    m_fish <- fish %>%
      filter(sex == 1) %>%
      mutate(year_f = as.character(year))

    # Check if only some years without maturity data, and set year_re = FALSE in that case
    years_w_maturity <- fish %>%
      group_by(year) %>%
      mutate(maturity_levels = length(unique(maturity_code)))

    levels_per_year <- unique(years_w_maturity$maturity_levels)

    if (max(levels_per_year) < 3) { # NA plus only one recorded maturity level is max ever recorded
      warning("Maturity data not recorded, so catch not split.", call. = FALSE)
      return(list(data = survey_sets, model = NULL))
    }

    if (min(levels_per_year) < 3) { # some years lack maturity data

      if (length(levels_per_year) < 3) {
        warning("Maturity data not recorded, so catch not split.", call. = FALSE)
        return(list(data = survey_sets, model = NULL))
      } else {
        warning("Some years lack maturity data, but catch still split.", call. = FALSE)

        m <- fit_mat_ogive(fish, type = "length", sample_id_re = sample_id_re)

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
        m <- fit_mat_ogive(fish, type = "length", sample_id_re = sample_id_re, year_re = TRUE)
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
        m <- fit_mat_ogive(fish, type = "length", sample_id_re = sample_id_re)

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

    # model mass of fish with only length data
    f_fish <- mutate(f_fish,
      adult = if_else(length >= threshold, 1, 0, missing = NULL),
      model_mass = exp(f_mass$pars$log_a + f_mass$pars$b * (log(length))) * 1000,
      new_mass = weight
    )

    # only apply simulated mass when below chosen cutoff_quantile
    max_model <- quantile(f_fish$weight, probs = c(cutoff_quantile), na.rm = TRUE)
    f_fish$model_mass[f_fish$model_mass > max_model] <- max_model
    f_fish$new_mass[is.na(f_fish$weight)] <- f_fish$model_mass[is.na(f_fish$weight)]

    m_fish <- mutate(m_fish,
      adult = if_else(length >= threshold, 1, 0, missing = NULL),
      model_mass = exp(m_mass$pars$log_a + m_mass$pars$b * (log(length))) * 1000,
      new_mass = weight
    )

    max_model_m <- quantile(m_fish$weight, probs = c(cutoff_quantile), na.rm = TRUE)
    m_fish$model_mass[m_fish$model_mass > max_model_m] <- max_model_m
    m_fish$new_mass[is.na(m_fish$weight)] <- m_fish$model_mass[is.na(m_fish$weight)]

    # combine dataframes of female and male fish
    fish_maturity <- rbind(f_fish, m_fish) %>%
      mutate(sex = ifelse(sex == 2, "F", "M"))

    set_maturity <- fish_maturity %>%
      group_by(fishing_event_id, adult) %>%
      mutate(maturity_mass = sum(new_mass)) %>%
      dplyr::add_tally() %>%
      rename(count = n) %>%
      ungroup()

    set_ratio <- set_maturity %>%
      group_by(fishing_event_id) %>%
      dplyr::add_tally() %>%
      mutate(
        est_sample_mass = sum(new_mass, na.rm = TRUE),
        mass_ratio = maturity_mass / est_sample_mass,
        measured_weight = sum(weight),
        n_weights = sum(!is.na(weight))
      ) %>%
      filter(adult == 1) %>%
      rename(mass_ratio_mature = mass_ratio, n_mature = count, n_sampled = n) %>%
      select(
        fishing_event_id,
        est_sample_mass, # needed for perc_sampled data check below
        measured_weight, n_weights, n_mature, n_sampled, # these are optional data checks
        mass_ratio_mature
      ) %>%
      unique()


    sets_w_ratio <- left_join(survey_sets, set_ratio, by = "fishing_event_id")

    # chose value to use when a sample mass ratio is not available
    if (use_median_ratio) {
      na_value <- median(sets_w_ratio$mass_ratio_mature, na.rm = TRUE)
    } else {
      na_value <- mean(sets_w_ratio$mass_ratio_mature, na.rm = TRUE)
    }

    sets_w_ratio$mass_ratio_mature[is.na(sets_w_ratio$mass_ratio_mature)] <- na_value

    # add column to check for discrepencies between total catch weight and biological sample weights
    # est_sample_mass is in g while catch_weight is in kg

    sets_w_ratio$unsampled_catch <- round((sets_w_ratio$catch_weight - sets_w_ratio$est_sample_mass / 1000), 2)
    sets_w_ratio$perc_sampled <- round((sets_w_ratio$est_sample_mass / 1000) / sets_w_ratio$catch_weight, 2) * 100

    sets_w_ratio$split_dens_type <- sets_w_ratio[[split_dens_type]]

    data <- sets_w_ratio %>%
      mutate(
        adult_density = split_dens_type * mass_ratio_mature,
        imm_density = split_dens_type * (1 - mass_ratio_mature)
      )

    data$split_dens_type <- split_dens_type

    if (plot) {
      try(
        maturity_plot <- plot_mat_ogive(m) +
          ggplot2::ggtitle(paste("Length at maturity for", species, "surveys", ssid_string, ""))
      )
      try(
        mass_plot <- ggplot(fish_maturity, aes(length, new_mass, colour = as.factor(sex))) +
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
    }
  } else {
    return(list(data = survey_sets, model = NULL))
  }
  if (plot) {
    list(data = data, m = m, maturity_plot = maturity_plot, mass_plot = mass_plot)
  } else {
    list(data = data, m = m)
  }
}

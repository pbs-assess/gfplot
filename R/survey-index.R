#' Plot survey relative biomass mass index
#'
#' @examples
#' \dontrun{
#' get_survey_index("lingcod") %>%
#'   tidy_survey_index() %>%
#'   plot_survey_index()
#' }
#' @name plot_survey_index

#' @param dat For [tidy_survey_index()], data from [get_survey_index()]. For
#'   [plot_survey_index()], data from [tidy_survey_index()].
#' @param survey A character vector of survey abbreviations to include.
#' @param min_years The minimum number of years for a survey before it is
#'   included.
#' @param year_range If not `NULL`, a numeric vector of length 2 giving the
#'   range of years to plot.
#' @rdname plot_survey_index
#' @export
tidy_survey_index <- function(dat,
                              survey = c(
                                "SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI",
                                "HBLL OUT N",
                                "HBLL OUT S", "HBLL INS N", "HBLL INS S",
                                "OTHER HS MSA", "IPHC FISS"
                              ),
                              min_years = 3, year_range = NULL) {
  if (is.null(year_range)) {
    year_range <- range(dat$year, na.rm = TRUE)
  }

  d <- filter(
    dat,
    survey_abbrev %in% survey,
    year >= year_range[[1]], year <= year_range[[2]]
  )

  dup <- group_by(d, species_common_name) %>%
    summarise(n_spp = length(unique(species_science_name))) %>%
    filter(n_spp > 1L)
  # assertthat::are_equal(nrow(dup), 0L)

  dup <- group_by(d, year, survey_abbrev, species_science_name) %>%
    summarise(n = n())
  # assertthat::are_equal(max(dup$n), 1L)

  # must be N years
  d <- d %>%
    arrange(species_common_name, survey_abbrev, year) %>%
    group_by(survey_abbrev, species_common_name) %>%
    mutate(n_years = length(unique(year))) %>%
    mutate(mean_cv = mean(re, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(n_years >= min_years)

  all_surv <- expand.grid(
    survey_abbrev = survey,
    stringsAsFactors = FALSE
  )

  d$survey_abbrev <- as.character(d$survey_abbrev)
  d <- left_join(all_surv, d, by = c(
    "survey_abbrev"
  ))

  trans <- tibble(
    survey_abbrev = survey,
    surv_order = seq(1, length(survey))
  )
  d <- inner_join(d, trans, by = "survey_abbrev") %>%
    mutate(survey_abbrev = fct_reorder(survey_abbrev, surv_order))
  select(
    d, survey_abbrev, year,
    biomass, lowerci, upperci, mean_cv,
    num_sets, num_pos_sets
  )
}

#' @param col A vector of two colours for the lines and shading.
#' @param max_cv A coefficient of variation above which a panel will be shaded
#'   as more uncertain.
#' @param max_set_fraction A fraction of positive sets above which a panel will
#' be shaded as more uncertain.
#' @param xlim If not `NULL`, the x axis limits.
#' @param survey_cols If not `NULL`, a named character vector of colors for the
#'   various surveys.
#' @param scale Logical: scale the biomass by the maximum?
#' @param year_increment Increment for the year x axis.
#' @param french Logical for French or English.
#' @param hide_y_axis Logical: hide the y axis ticks and labels?
#'
#' @export
#'
#' @rdname plot_survey_index

plot_survey_index <- function(dat, col = brewer.pal(9, "Greys")[c(3, 7)],
                              max_cv = 0.4,
                              max_set_fraction = 0.05,
                              xlim = NULL,
                              survey_cols = NULL,
                              scale = TRUE,
                              year_increment = 5,
                              french = FALSE,
                              hide_y_axis = FALSE) {

  if (scale) {
    d <- dat %>%
      group_by(survey_abbrev) %>%
      mutate(
        biomass_scaled = biomass / max(upperci),
        lowerci_scaled = lowerci / max(upperci),
        upperci_scaled = upperci / max(upperci)
      ) %>%
      ungroup()
  } else {
    d <- dat %>%
      mutate(
        biomass_scaled = biomass,
        lowerci_scaled = lowerci,
        upperci_scaled = upperci
      )
  }

  labs <- unique(select(d, survey_abbrev))
  if (is.null(xlim)) {
    yrs <- range(d$year, na.rm = TRUE)
  } else {
    yrs <- xlim
  }

  if (!is.null(survey_cols)) {
    line_col <- survey_cols
    fill_col <- paste0(substr(line_col, 1L, 7L), as.character(0.7 * 100))
    names(fill_col) <- names(line_col)
  } else {
    line_col <- rep(col[[2]], length(levels(d$survey_abbrev)))
    fill_col <- rep(col[[1]], length(levels(d$survey_abbrev)))
  }

  stats_df <- select(d, survey_abbrev, mean_cv, num_sets, num_pos_sets) %>%
    group_by(survey_abbrev) %>%
    summarise(
      mean_cv = sprintf("%.2f", round(mean(mean_cv, na.rm = TRUE), 2)),
      mean_num_pos_sets = sprintf("%.0f", round(mean(num_pos_sets, na.rm = TRUE), 0)),
      mean_num_sets = sprintf("%.0f", round(mean(num_sets, na.rm = TRUE), 0))) %>%
    mutate(sets = paste0(en2fr("Mean +ve sets", french), ": ", mean_num_pos_sets, "/", mean_num_sets)) %>%
    mutate(cv = paste0(en2fr("Mean", french), " CV: ", mean_cv)) %>%
    mutate(cv = ifelse(mean_cv == "NaN", "", cv)) %>%
    mutate(sets = ifelse(mean_num_pos_sets == "NaN", "", sets)) %>%
    mutate(mean_cv = as.numeric(mean_cv)) %>%
    mutate(mean_num_pos_sets = as.numeric(mean_num_pos_sets),
      mean_num_sets = as.numeric(mean_num_sets))

  if (french) {
    stats_df$cv <- gsub("\\.", ",", stats_df$cv)
  }

  uncertain <- stats_df %>%
    mutate(uncertain = mean_cv > max_cv |
        mean_num_pos_sets / mean_num_sets < max_set_fraction |
        sets == "") %>%
    filter(uncertain)

  g <- ggplot(d, aes_string("year", "biomass_scaled"))

  if (scale) {
    g <- g + geom_rect(data = uncertain, xmin = 1800, xmax = 2050, ymin = -0.02,
        ymax = max(d$upperci_scaled, na.rm = TRUE) * 1.1,
        inherit.aes = FALSE, fill = "grey96")
  }

  g <- g +
    geom_vline(xintercept = seq(yrs[1], yrs[2]), col = "grey96", lwd = 0.3) +
    geom_vline(xintercept = seq(mround(yrs[1], 5), yrs[2], 5), col = "grey93") +
    geom_ribbon(aes_string(
      ymin = "lowerci_scaled", ymax = "upperci_scaled",
      fill = "survey_abbrev"
    ), colour = NA, alpha = 0.5) +
    geom_line(col = "#00000050", size = 1) +
    geom_point(aes_string(colour = "survey_abbrev"),
      pch = 21, fill = "grey95", size = 1.4, stroke = 1
    ) +
    facet_wrap(~survey_abbrev, ncol = 2, scales = "fixed") +
    theme_pbs() +
    theme(panel.spacing = unit(-0.1, "lines")) +
    ylim(-0.005, NA) +
    coord_cartesian(expand = FALSE, xlim = yrs + c(-0.5, 0.5)) +
    scale_fill_manual(values = fill_col) +
    scale_colour_manual(values = line_col) +
    xlab("") +
    ylab(en2fr("Survey relative biomass indices", french)) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    guides(fill = FALSE, colour = FALSE) +
    scale_x_continuous(breaks = seq(0, yrs[2], year_increment))

  if (scale) {
  g <- g + geom_text(
      data = labs, x = yrs[1] + 0.5, y = 0.89,
      aes_string(label = "survey_abbrev"),
      inherit.aes = FALSE, colour = "grey30", size = 3, hjust = 0
    ) +
      geom_text(data = stats_df, aes_string(label = "cv"),
        x = yrs[1] + 0.5, y = 0.67,
        colour = "grey35", size = 2.65, hjust = 0) +
      geom_text(data = stats_df,
        aes_string(label = "sets"),
        x = yrs[1] + 0.5, y = 0.49,
        colour = "grey35", size = 2.65, hjust = 0)
  } else {
    g <- g + theme(
      strip.text.x = element_text(colour = "grey30")
    )
  }

  if (hide_y_axis)
    g <- g + theme(axis.text.y = element_text(colour = "white"),
      axis.ticks.y = element_line(colour = "white"))

  g
}

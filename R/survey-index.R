#' Plot survey relative biomass mass index
#'
#' @examples
#' \dontrun{
#' get_survey_index("lingcod") %>%
#'   tidy_survey_index() %>%
#'   plot_survey_index()
#'
#' # Or without pipes:
#' d <- get_survey_index("lingcod")
#' head(d)
#'
#' d_tidy <- tidy_survey_index(d)
#' head(d_tidy)
#'
#' plot_survey_index(d_tidy)
#' }

#' @name plot_survey_index
#' @param dat TODO
#' @param survey TODO
#' @param min_years TODO
#' @param year_range TODO
#' @family tidy data functions
#' @rdname plot_survey_index
#' @export
tidy_survey_index <- function(dat,
                              survey = c(
                                "SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI",
                                "HBLL OUT N",
                                "HBLL OUT S", "HBLL INS N", "HBLL INS S",
                                "MSA HS", "IPHC FISS"
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
  assertthat::are_equal(nrow(dup), 0L)

  dup <- group_by(d, year, survey_abbrev, species_science_name) %>%
    summarise(n = n())
  assertthat::are_equal(max(dup$n), 1L)

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
    species_common_name = unique(d$species_common_name),
    stringsAsFactors = FALSE
  )
  d <- left_join(all_surv, d, by = c(
    "species_common_name",
    "survey_abbrev"
  ))

  trans <- tibble(
    survey_abbrev = survey,
    surv_order = seq(1, length(survey))
  )
  d <- inner_join(d, trans, by = "survey_abbrev") %>%
    mutate(survey_abbrev = fct_reorder(survey_abbrev, surv_order))
  select(
    d, species_common_name, survey_abbrev, year,
    biomass, lowerci, upperci, mean_cv,
    num_sets, num_pos_sets
  )
}

#' @param col TODO
#' @param title TODO
#' @param survey_cols TODO
#'
#' @export
#' @family plotting functions
#'
#' @rdname plot_survey_index

plot_survey_index <- function(dat, col = brewer.pal(9, "Greys")[c(3, 7)],
                              title = "Biomass indices", survey_cols = NULL) {
  d <- dat %>%
    group_by(survey_abbrev) %>%
    mutate(
      biomass_scaled = biomass / max(upperci),
      lowerci_scaled = lowerci / max(upperci),
      upperci_scaled = upperci / max(upperci)
    ) %>%
    ungroup()

  labs <- unique(select(d, survey_abbrev))
  yrs <- range(d$year, na.rm = TRUE)

  if (!is.null(survey_cols)) {
    fill_col <- stats::setNames(survey_cols, levels(d$survey_abbrev))
    fill_col <- paste0(substr(fill_col, 1L, 7L), as.character(0.7 * 100))
  } else {
    fill_col <- rep(col[[1]], length(levels(d$survey_abbrev)))
  }

  ggplot(d, aes_string("year", "biomass_scaled")) +
    geom_vline(xintercept = seq(yrs[1], yrs[2]), col = "grey98") +
    geom_vline(xintercept = seq(mround(yrs[1], 5), yrs[2], 5), col = "grey95") +
    geom_ribbon(aes_string(
      ymin = "lowerci_scaled", ymax = "upperci_scaled",
      fill = "survey_abbrev"
    ), colour = NA, alpha = 0.3) +
    geom_line(col = "#00000050", size = 1) +
    geom_point(
      pch = 21, colour = col[[2]], fill = "grey60", size = 1.6, stroke = 1
    ) +
    facet_wrap(~survey_abbrev, ncol = 2) +
    theme_pbs() +
    theme(panel.spacing = unit(-0.1, "lines")) +
    ylim(-0.01, NA) +
    coord_cartesian(expand = FALSE, xlim = yrs + c(-0.5, 0.5)) +
    scale_fill_manual(values = fill_col) +
    xlab("") +
    ylab("Relative biomass") +
    theme(
      axis.text.y = element_text(colour = "white"),
      axis.ticks.y = element_line(colour = "white"),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    labs(title = title) +
    guides(fill = FALSE, colour = FALSE) +
    geom_text(
      data = labs, x = yrs[1] + 0.5, y = 0.88,
      aes_string(label = "survey_abbrev"),
      inherit.aes = FALSE, colour = "grey30", size = 2.75, hjust = 0
    ) +
    scale_x_continuous(breaks = seq(0, yrs[2], 10))
}

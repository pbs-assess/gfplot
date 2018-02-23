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
#' @param surveys TODO
#' @param survey_names TODO
#' @param min_years TODO
#' @param year_range TODO
#' @family tidy data functions
#' @rdname plot_survey_index
#' @export
tidy_survey_index <- function(dat,
  surveys = c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey",
    "PHMA Rockfish Longline Survey - Outside North",
    "PHMA Rockfish Longline Survey - Outside South",
    "IRF Longline Survey (North)",
    "IRF Longline Survey (South)",
    "Hecate Strait Multispecies Assemblage Survey",
    "IPHC Longline Survey"),
  survey_names = c("West Coast Haida Gwaii Synoptic",
    "Hecate Strait Synoptic",
    "Queen Charlotte Sound Synoptic",
    "West Coast Vancouver Island Synoptic",
    "PHMA Rockfish LL - North",
    "PHMA Rockfish LL - South",
    "IRF LL Survey North",
    "IRF LL Survey South",
    "Hecate Strait Multispecies Assemblage",
    "IPHC Longline"),
  min_years = 3, year_range = NULL) {

  if (is.null(year_range))
    year_range <- range(dat$year, na.rm = TRUE)

  d <- filter(dat,
    .data$survey_series_desc %in% surveys,
    year >= year_range[[1]], year <= year_range[[2]])

  dup <- dplyr::group_by(d, species_common_name) %>%
    summarise(n_spp = length(unique(species_science_name))) %>%
    filter(n_spp > 1L)
  assertthat::are_equal(nrow(dup), 0L)

  dup <- dplyr::group_by(d, .data$year,
    .data$survey_series_desc,
    .data$species_science_name) %>%
    summarise(n = n())
  assertthat::are_equal(max(dup$n), 1L)

  # must be N years
  d <- d %>% arrange(.data$species_common_name,
    .data$survey_series_desc, .data$year) %>%
    group_by(.data$survey_series_desc, .data$species_common_name) %>%
    mutate(n_years = length(unique(.data$year))) %>%
    mutate(mean_cv = mean(.data$re, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(.data$n_years >= min_years)

  all_surv <- expand.grid(survey_series_desc = surveys,
    species_common_name = unique(d$species_common_name),
    stringsAsFactors = FALSE)
  d <- left_join(all_surv, d, by = c("species_common_name",
      "survey_series_desc"))

  trans <- tibble(survey_series_desc = surveys, survey_name = survey_names,
    surv_order = seq(1, length(survey_names)))
  d <- inner_join(d, trans, by = "survey_series_desc") %>%
    mutate(survey_name = fct_reorder(.data$survey_name, .data$surv_order))
  select(d, .data$species_common_name, .data$survey_name, .data$year,
    .data$biomass, .data$lowerci, .data$upperci, .data$mean_cv,
    .data$num_sets, .data$num_pos_sets)
}

#' @param col TODO
#' @param title TODO
#'
#' @export
#' @family plotting functions
#'
#' @rdname plot_survey_index

plot_survey_index <- function(dat, col = brewer.pal(9, "Greys")[c(3, 7)],
  title = "Biomass indices") {

  d <- dat %>%
    group_by(.data$survey_name) %>%
    mutate(biomass_scaled = .data$biomass / max(.data$upperci),
      lowerci_scaled = .data$lowerci / max(.data$upperci),
      upperci_scaled = .data$upperci / max(.data$upperci)) %>%
    ungroup()

  labs <- unique(select(d, .data$survey_name))
  yrs <- range(d$year, na.rm = TRUE)

  ggplot(d, aes_string("year", "biomass_scaled")) +
    geom_vline(xintercept = seq(yrs[1], yrs[2]), col = "grey97") +
    geom_vline(xintercept = seq(mround(yrs[1], 5), yrs[2], 5), col = "grey93") +
    geom_ribbon(aes_string(ymin = "lowerci_scaled", ymax = "upperci_scaled"),
      colour = NA, fill = col[[1]]) +
    geom_line(col = "#00000050", size = 1) +
    geom_point(pch = 21, colour = col[[2]], fill = "grey60", size = 1.6,
      stroke = 1) +
    facet_wrap(~survey_name, ncol = 2) +
    theme_pbs() +
    theme(panel.spacing = unit(-0.1, "lines")) +
    ylim(-0.01, NA) +
    coord_cartesian(expand = FALSE, xlim = yrs + c(-0.5, 0.5)) +
    xlab("") +
    ylab("Relative biomass") +
    theme(
      axis.text.y = element_text(colour = "white"),
      axis.ticks.y = element_line(colour = "white"),
      strip.background = element_blank(),
      strip.text.x = element_blank()) +
    labs(title = title) +
    geom_text(data = labs, x = yrs[1] + 0.5, y = 0.88,
      aes_string(label = "survey_name"),
      inherit.aes = FALSE, colour = "grey30", size = 2.75, hjust = 0) +
    scale_x_continuous(breaks = seq(0, yrs[2], 10))
}

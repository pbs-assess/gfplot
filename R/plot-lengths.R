# TODO
#
# @param dat
# tidy_pbs_lengths <- function(dat) {

# }

#' TODO
#'
#' @param dat TODO
#' @param n_bins TODO
#' @param bin_size TODO
#' @param min_specimens TODO
plot_lengths <- function(dat, n_bins = 25, bin_size = NULL, min_specimens = 20L) {

  survs <- c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey",
    "IPHC Longline Survey",
    "PHMA Rockfish Longline Survey - Outside North",
    "PHMA Rockfish Longline Survey - Outside South")

  dat <- filter(dat, survey_series_desc %in% survs)
  dat <- dat[!duplicated(dat$specimen_id), ] # critical!!
  dat <- dat %>%
    select(species_common_name, .data$species_science_name,
      .data$sample_id,
      .data$year, .data$age, .data$length, .data$weight,
      .data$maturity_code, .data$sex, .data$survey_series_desc)

  # bad data:
  dat <- dat[!(dat$length > 600 & dat$species_common_name == "north pacific spiny dogfish"), ]
  dat <- dat[!(dat$length > 600 & dat$species_common_name == "big skate"), ]
  dat <- dat[!(dat$length > 600 & dat$species_common_name == "longnose skate"), ]
  dat <- dat[!(dat$length > 60 & dat$species_common_name == "pacific tomcod"), ]
  dat <- dat[!(dat$length > 50 & dat$species_common_name == "quillback rockfish"), ]
  dat <- dat[!(dat$length < 10 & dat$weight/1000 > 1.0 &
      dat$species_common_name == "pacific flatnose"), ]

  dat <- filter(dat, !is.na(length), !is.na(sex))

  surveys <- c("WCHG", "HS", "QCS", "WCVI", "IPHC", "PHMA LL (N)", "PHMA LL (S)")
  su <- dplyr::tibble(survey = surveys,
    survey_series_desc = survs)
  all <- expand.grid(survey = surveys, year = seq(min(dat$year), max(dat$year)),
    stringsAsFactors = FALSE)

  surv_year_counts <- dat %>%
    left_join(su, by = "survey_series_desc") %>%
    group_by(.data$year, .data$survey) %>%
    summarise(total = n(), max_length = max(.data$length)) %>%
    ungroup() %>%
    full_join(all, by = c("year", "survey"))

  counts <- select(surv_year_counts, .data$total, .data$year,
    .data$survey, .data$max_length) %>% unique()

  max_length <- filter(counts, .data$total >= min_specimens) %>%
    dplyr::pull(.data$max_length) %>% max()

  if (is.null(bin_size)) {
    lengths <- seq(0, max_length + 0.1, length.out = n_bins)
  } else {
    lengths <- seq(0, max_length + 0.1, bin_size)
  }

  dd <- dat %>%
    left_join(su, by = "survey_series_desc") %>%
    mutate(length_bin = lengths[findInterval(.data$length, lengths)]) %>%
    group_by(.data$year, .data$survey, .data$length_bin, .data$sex) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(.data$year, .data$survey) %>%
    mutate(max_n = max(.data$n)) %>%
    ungroup() %>%
    mutate(proportion = .data$n/.data$max_n) %>%
    mutate(sex = ifelse(.data$sex == 1, "M", "F")) %>%
    full_join(all, by = c("year", "survey")) %>%
    left_join(counts, by = c("year", "survey")) %>%
    mutate(proportion = ifelse(.data$total >= min_specimens, .data$proportion, NA))

  dd$sex[is.na(dd$sex)] <- "M"

  x_breaks <- pretty(dd$length_bin, 4)
  N <- length(x_breaks)
  x_breaks <- x_breaks[seq(1, N - 1)]
  range_lengths <- diff(range(dd$length_bin, na.rm = TRUE))

  # plot_lengths <- function()
  ggplot(dd, aes_string("length_bin", "proportion")) +
    ggplot2::geom_col(width = bin_size, aes_string(colour = "sex",
      fill = "sex"), size = 0.3,
      position = ggplot2::position_identity()) +
    ggplot2::facet_grid(forcats::fct_rev(as.character(year))~
        forcats::fct_relevel(survey,
          su$survey)) +
    theme_pbs() +
    scale_fill_manual(values = c("M" = "grey80", "F" = "#FF000010")) +
    scale_colour_manual(values = c("M" = "grey40", "F" = "red")) +
    coord_cartesian(expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    xlab("Length (cm)") +
    ylab("Relative length frequency") +
    ylim(-0.06, 1.1) +
    theme(
      axis.text.y = ggplot2::element_text(colour = "white"),
      axis.ticks.y = ggplot2::element_line(colour = "white")) +
    labs(colour = "Sex", fill = "Sex") +
    geom_text(data = counts,
      x = min(dd$length_bin, na.rm = TRUE) + 0.02 * range_lengths, y = 0.8,
      aes_string(label = "total"),
      inherit.aes = FALSE, colour = "grey50", size = 2.25, hjust = 0) +
    labs(title = "Length frequencies") +
    theme(panel.grid.major.x = ggplot2::element_line(colour = "grey92"))
}


# weighting:
# # bio_specs <- readRDS("data-cache/all-commercial-bio.rds") %>%
# #   filter(species_common_name %in% "pacific ocean perch")
# # catch <- readRDS("data-cache/all-catches.rds") %>%
# #   filter(species_common_name %in% "pacific ocean perch")
# survey_specimens <- readRDS("data-cache/all-survey-bio.rds") %>%
#   filter(species_common_name %in% "pacific ocean perch")
# survey_tows <- readRDS("data-cache/all-survey-spatial-tows.rds") %>%
#   filter(species_common_name %in% "pacific ocean perch")
#
# out <- purrr::map_df(survs, function(x) {
#   surv_spec <- dplyr::filter(survey_specimens, survey_series_desc == x)
#   surv_tows <- dplyr::filter(survey_tows, survey_series_desc == x)
#   o <- surv_spec %>% join_comps_survey(surv_tows, length, bin_size = 2) %>%
#     weight_comps()
#   o$survey_series_desc <- x
#   o
# })
#
# ggplot(out, aes_string("value", "weighted_prop")) +
#   ggplot2::geom_col(width = 2) +
#   facet_grid(forcats::fct_rev(as.character(year))~forcats::fct_relevel(survey_series_desc,
#     survs)) +
#   theme_pbs() +
#   theme(panel.spacing = unit(-0.5, "lines")) +
#   ylim(0, NA) +
#   coord_cartesian(expand = FALSE)

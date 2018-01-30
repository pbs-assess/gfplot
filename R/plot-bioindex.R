mround <- function(x, base){
  base * round(x / base)
}

plot_bioindex <- function() {

  d <- readRDS("data-cache/all-boot-biomass-indices.rds")
  d <- dplyr::filter(d, species_science_name != "ophiodontinae") # lingcod duplicate spp.
  d <- dplyr::filter(d, species_science_name != "cetorhinidae") # basking shark duplicate spp.
  dup <- dplyr::group_by(d, species_common_name) %>%
    dplyr::summarise(n_spp = length(unique(species_science_name))) %>%
    dplyr::arrange(-n_spp) %>%
    dplyr::filter(n_spp > 1)
  stopifnot(nrow(dup) == 0)

  species <- "lingcod"
  xlim <- c(1975, 2017)
  d <- filter(d, species_common_name %in% species,
    year >= xlim[1])

  # TODO: THERE MULTIPLE VALUES PER YEAR SOMETIMES? answer: species_common_name
  d <- dplyr::group_by(d, .data$year,
    .data$survey_series_desc,
    .data$species_science_name) %>%
    dplyr::summarise(
      biomass = mean(.data$biomass),
      lowerci = mean(.data$lowerci),
      upperci = mean(.data$upperci),
      re = mean(.data$re),
      num_sets = mean(.data$num_sets),
      num_pos_sets = mean(.data$num_pos_sets),
      survey_series_id = .data$survey_series_id[1],
      species_common_name = .data$species_common_name[1]) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$species_common_name, .data$survey_series_desc,
      .data$year)

  # must be N years
  d <- dplyr::group_by(d, survey_series_desc, species_common_name) %>%
    dplyr::mutate(n_years = length(unique(year))) %>%
    dplyr::mutate(mean_cv = mean(re, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_years >= 4)

  make_plot <- TRUE
  if (nrow(d) == 0) make_plot <- FALSE

  surveys <- c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey",
    "PHMA Rockfish Longline Survey - Outside North",
    "PHMA Rockfish Longline Survey - Outside South",
    "IRF Longline Survey (North)",
    "IRF Longline Survey (South)",
    "Hecate Strait Multispecies Assemblage Survey",
    "IPHC Longline Survey")

  d <- dplyr::filter(d, .data$survey_series_desc %in% surveys) %>%
    dplyr::group_by(.data$species_common_name, .data$survey_series_desc) %>%
    dplyr::mutate(biomass_scaled = .data$biomass / max(.data$upperci),
      lowerci_scaled = .data$lowerci / max(.data$upperci),
      upperci_scaled = .data$upperci / max(.data$upperci))

  all_surv <- data.frame(survey_series_desc = surveys, stringsAsFactors = FALSE)
  d <- dplyr::left_join(all_surv, d, by = "survey_series_desc")

  d$survey_series_desc <- factor(d$survey_series_desc, levels = surveys)

  d <- d %>% dplyr::mutate(survey_series_desc = dplyr::recode(survey_series_desc,
    `West Coast Haida Gwaii Synoptic Survey` = "West Coast Haida Gwaii Synoptic",
    `Hecate Strait Synoptic Survey` = "Hecate Strait Synoptic",
    `Queen Charlotte Sound Synoptic Survey` = "Queen Charlotte Sound Synoptic",
    `West Coast Vancouver Island Synoptic Survey` = "West Coast Vancouver Island Synoptic",
    `PHMA Rockfish Longline Survey - Outside North` = "PHMA Rockfish Longline - Outside North",
    `PHMA Rockfish Longline Survey - Outside South` = "PHMA Rockfish Longline - Outside South",
    `IRF Longline Survey (North)` = "IRF Longline Survey North",
    `IRF Longline Survey (South)` = "IRF Longline Survey South",
    `Hecate Strait Multispecies Assemblage Survey` = "Hecate Strait Multispecies Assemblage",
    `IPHC Longline Survey` = "IPHC Longline"))

  labs <- unique(select(d, survey_series_desc))

  cols <- c(
    RColorBrewer::brewer.pal(9, "Greens")[3],
    RColorBrewer::brewer.pal(9, "Blues")[3],
    RColorBrewer::brewer.pal(9, "Reds")[3],
    RColorBrewer::brewer.pal(9, "Purples")[3]
  )

  cols_dark <- c(
    RColorBrewer::brewer.pal(9, "Greens")[7],
    RColorBrewer::brewer.pal(9, "Blues")[7],
    RColorBrewer::brewer.pal(9, "Reds")[7],
    RColorBrewer::brewer.pal(9, "Purples")[7]
  )

  yrs <- c(min(d$year, na.rm = TRUE), max(d$year, na.rm = TRUE))
  ggplot(d, aes_string("year", "biomass_scaled")) +
    geom_vline(xintercept = seq(yrs[1], yrs[2]), col = "grey97") +
    geom_vline(xintercept = seq(mround(yrs[1], 5), yrs[2], 5), col = "grey93") +
    geom_ribbon(aes_string(ymin = "lowerci_scaled", ymax = "upperci_scaled"),
      colour = NA, fill = cols[[2]]) +
    # geom_linerange(aes_string(ymin = "lowerci_scaled", ymax = "upperci_scaled"),
    #   colour = "grey40") +
    geom_line(col = "#00000050", size = 1) +
    geom_point(pch = 21, colour = cols_dark[[2]], fill = "grey60", size = 1.6, stroke = 1) +
    ggplot2::facet_wrap(~survey_series_desc, ncol = 2) +
    theme_pbs() +
    # scale_fill_manual(values = c("M" = "grey80", "F" = "#FF000001")) +
    # scale_colour_manual(values = c("M" = "grey40", "F" = "red")) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    ylim(-0.03, NA) +
    coord_cartesian(expand = FALSE, xlim = yrs + c(-0.5, 0.5)) +
    xlab("") +
    ylab("") +
    theme(axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank()) +
    labs(title = "Biomass indices") +
    geom_text(data = labs, x = yrs[1] + 0.5, y = 0.88,
      aes_string(label = "survey_series_desc"),
      inherit.aes = FALSE, colour = "grey50", size = 2.75, hjust = 0) +
    scale_x_continuous(breaks = seq(0, yrs[2], 10))

}

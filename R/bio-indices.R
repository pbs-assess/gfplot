source("R/add-label.R")

plot_index_sparks <- function(species) {

  library(tidyverse)

  d <- readRDS("data-cache/all-boot-biomass-indices.rds")
  d <- filter(d, species_science_name != "ophiodontinae") # lingcod duplicate spp.
  d <- filter(d, species_science_name != "cetorhinidae") # basking shark duplicate spp.

  dup <- group_by(d, species_common_name) %>%
    summarise(n_spp = length(unique(species_science_name))) %>%
    arrange(-n_spp) %>%
    filter(n_spp > 1)
  stopifnot(nrow(dup) == 0)

  xlim <- c(1975, 2017)
  d <- filter(d, species_common_name %in% species,
    year >= xlim[1])

  # TODO FIX? WHY ARE THERE MULTIPLE VALUES PER YEAR SOMETIME!?
  d <- group_by(d, year, survey_series_desc, species_science_name) %>%
    summarise(biomass = mean(biomass), lowerci = mean(lowerci), upperci = mean(upperci),
      re = mean(re), num_sets = mean(num_sets), num_pos_sets = mean(num_pos_sets),
      survey_series_id = survey_series_id[1],
      species_common_name = species_common_name[1]) %>%
    ungroup() %>% as.data.frame() %>%
    arrange(species_common_name, survey_series_desc, year)

  # must be N years
  d <- group_by(d, survey_series_desc, species_common_name) %>%
    mutate(n_years = length(unique(year))) %>%
    mutate(mean_cv = mean(re, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(n_years >= 4)

  make_plot <- TRUE
  if (nrow(d) == 0) make_plot <- FALSE

  surveys <- c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey",
    "PHMA Rockfish Longline Survey - Outside North",
    "PHMA Rockfish Longline Survey - Outside South",
    # "Sablefish Offshore Standardized",
    # "Sablefish Inlet Standardized",
    # "Sablefish Stratified Random",
    "IRF Longline Survey (North)",
    "IRF Longline Survey (South)",
    "Hecate Strait Multispecies Assemblage Survey",
    # "Restratification of Goose Island Gully for the 2009 POP Assessment.",
    "Queen Charlotte Sound Shrimp Survey",
    "West Coast Vancouver Island Shrimp Survey",
    "IPHC Longline Survey"
  )

  if (make_plot) {
    d <- filter(d, survey_series_desc %in% surveys) %>%
      group_by(species_common_name, survey_series_desc) %>%
      mutate(biomass_scaled = biomass / max(upperci),
        lowerci_scaled = lowerci / max(upperci),
        upperci_scaled = upperci / max(upperci))

    all_surv <- data.frame(survey_series_desc = surveys, stringsAsFactors = FALSE)
    d <- left_join(all_surv, d, by = "survey_series_desc")

    d$survey_series_desc <- factor(d$survey_series_desc, levels = surveys)

    d <- d %>% mutate(survey_series_desc = dplyr::recode(survey_series_desc,
      `West Coast Haida Gwaii Synoptic Survey` = "West Coast Haida Gwaii Synoptic",
      `Hecate Strait Synoptic Survey` = "Hecate Strait Synoptic",
      `Queen Charlotte Sound Synoptic Survey` = "Queen Charlotte Sound Synoptic",
      `West Coast Vancouver Island Synoptic Survey` = "West Coast Vancouver Island Synoptic",
      `PHMA Rockfish Longline Survey - Outside North` = "PHMA Rockfish Longline - Outside North",
      `PHMA Rockfish Longline Survey - Outside South` = "PHMA Rockfish Longline - Outside South",
      # `Sablefish Offshore Standardized`,
      # `Sablefish Inlet Standardized`,
      # `Sablefish Stratified Random`,
      `IRF Longline Survey (North)` = "IRF Longline Survey North",
      `IRF Longline Survey (South)` = "IRF Longline Survey South",
      `Hecate Strait Multispecies Assemblage Survey` = "Hecate Strait Multispecies Assemblage",
      # `Restratification of Goose Island Gully for the 2009 POP Assessment.` = "Goose Island Gully",
      `Queen Charlotte Sound Shrimp Survey` = "Queen Charlotte Sound Shrimp",
      `West Coast Vancouver Island Shrimp Survey` = "West Coast Vancouver Island Shrimp",
      `IPHC Longline Survey` = "IPHC Longline"))


    cols <- c(
      RColorBrewer::brewer.pal(9, "Greens")[3],
      RColorBrewer::brewer.pal(9, "Blues")[3],
      RColorBrewer::brewer.pal(9, "Reds")[3],
      RColorBrewer::brewer.pal(9, "Purples")[3]
    )
    cols <- paste0(cols, "")

    cols_dark <- c(
      RColorBrewer::brewer.pal(9, "Greens")[7],
      RColorBrewer::brewer.pal(9, "Blues")[7],
      RColorBrewer::brewer.pal(9, "Reds")[7],
      RColorBrewer::brewer.pal(9, "Purples")[7]
    )

    par(mfrow = c(6, 2), oma = c(2, .5, .5, .5), cex = 0.7, mar = c(0, 0, 0, 0),
      mgp = c(2, 0.4, 0),
      tcl = -0.3)
    # par(xpd = NA)

    panel <<- 0

    plyr::d_ply(d, "survey_series_desc", function(x) {

      panel <<- panel + 1
      plot(1, 1, type = "n", xlim = xlim + c(-0.4, 0.4),
        ylim = c(-0.03, 1.03), axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
      abline(v = seq(1960, 2020, 10), col = "grey87", lwd = 1.1)
      abline(v = seq(1960, 2020, 2), col = "grey95", lwd = 0.7)
      box(col = "grey50")

      if (panel %in% c(11:12))
        axis(1, col = "grey60", col.ticks = "grey70", col.axis = "grey30",
          at = seq(1980, 2040, 5))

      if (!is.na(x$year[[1]]) & length(unique(x$year)) > 1) {


        shade <- "grey80"
        pt_col <- "grey40"
        if (unique(x$survey_series_desc) == "West Coast Haida Gwaii Synoptic") {
          shade <- cols[[2]]
          pt_col <- cols_dark[[2]]
        }

        if (unique(x$survey_series_desc) == "Hecate Strait Synoptic") {
          shade <- cols[[1]]
          pt_col <- cols_dark[[1]]
        }

        if (unique(x$survey_series_desc) == "West Coast Vancouver Island Synoptic") {
          shade <- cols[[4]]
          pt_col <- cols_dark[[4]]
        }

        if (unique(x$survey_series_desc) == "Queen Charlotte Sound Synoptic") {
          shade <- cols[[3]]
          pt_col <- cols_dark[[3]]
        }

        polygon(c(x$year, rev(x$year)), c(x$lowerci_scaled, rev(x$upperci_scaled)),
          col = shade, border = NA)
        lines(x$year, x$biomass_scaled, col = "#00000050", lwd = 2)
        points(x$year, x$biomass_scaled, pch = 21, col = pt_col, bg = "grey60", cex = 1.0,
          lwd = 1.6)
      }

      add_label(-0.005, 0.15, unique(x$survey_series_desc), col = "grey30", cex = 0.8)
    })
  } else { # blank:
    par(mfrow = c(6, 2), oma = c(2, .5, .5, .5), cex = 0.7, mar = c(0, 0, 0, 0),
      mgp = c(2, 0.4, 0),
      tcl = -0.3)
    for (i in 1:12) {
      plot(1, 1, type = "n", xlim = xlim + c(-0.4, 0.4), ylim = c(-0.03, 1.03),
        axes = FALSE, ann = FALSE,
        xaxs = "i", yaxs = "i")
      box(col = "grey50")
      if (i %in% c(11:12))
        axis(1, col = "grey60", col.ticks = "grey70", col.axis = "grey30",
          at = seq(1980, 2040, 5))
    }
  }
}


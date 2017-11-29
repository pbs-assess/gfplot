#' @param xfrac The fraction over from the left side.
#' @param yfrac The fraction down from the top.
#' @param label The text to label with.
#' @param pos Position to pass to text()
#' @param ... Anything extra to pass to text(), e.g. cex, col.
add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}


plot_index_sparks <- function(species) {
  
  library(tidyverse)
  
  d <- readRDS("~/Dropbox/dfo/data/all-boot-biomass-indices.rds")
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_science_name <- tolower(d$species_science_name)
  
  d <- filter(d, species_common_name %in% species)
  
  surveys <- c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey",
    "PHMA Rockfish Longline Survey - Outside North",
    "PHMA Rockfish Longline Survey - Outside South",
    "Sablefish Offshore Standardized",
    "Sablefish Inlet Standardized",
    "Sablefish Stratified Random",
    "Queen Charlotte Sound Shrimp Survey",
    "West Coast Vancouver Island Shrimp Survey",
    "IPHC Longline Survey"
  )
  
  d <- filter(d, survey_series_desc %in% surveys)
  
  # what to plot?
  # bio <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
  # names(bio) <- tolower(names(bio))
  # bio$species_common_name <- tolower(bio$species_common_name)
  # bio$species_science_name <- tolower(bio$species_science_name)
  # bio$year <- lubridate::year(bio$trip_start_date)
  # 
  # spp <- group_by(bio, species_common_name) %>% filter(
  #   year > 2000,
  #   !is.na(species_common_name),
  #   species_common_name != "all species",
  #   !grepl("shrimp", species_common_name),
  #   !grepl("urchin", species_common_name),
  #   !grepl("jelly", species_common_name)
  # ) %>% summarise(total_catch = sum(catch_weight, na.rm = TRUE)) %>% 
  #   arrange(-total_catch) %>% `[`(1:12, 1:2)
  
  # d <- inner_join(d, spp) %>% 
  d <- d %>% 
    group_by(species_common_name, survey_series_desc) %>% 
    mutate(biomass_scaled = biomass / max(upperci), 
      lowerci_scaled = lowerci / max(upperci),
      upperci_scaled = upperci / max(upperci))
  
  # ggplot(d, aes(year, biomass_scaled)) + 
  #   geom_ribbon(aes(ymin = lowerci_scaled, ymax = upperci_scaled), fill = "grey80", col = NA) +
  #   # geom_point() +
  #   facet_grid(survey_series_desc~forcats::fct_reorder(species_common_name, -total_catch), 
  #     scales = "free_y") +
  #   # geom_segment(aes(x = year, xend = year, 
  #     # y = lowerci_scaled, yend = upperci_scaled), colour = "grey50") +
  #   geom_line(colour = "grey10") +
  #   ggsidekick::theme_sleek()
  #   # coord_cartesian(ylim = c(0, 2))
  #   
  
  # d$survey_series_desc <- gsub("Outside North", "Out. N.", d$survey_series_desc)
  # d$survey_series_desc <- gsub("Outside South", "Out. S.", d$survey_series_desc)
  # d$survey_series_desc <- gsub(" Survey", "", d$survey_series_desc)
  
  all_surv <- data.frame(survey_series_desc = surveys, stringsAsFactors = FALSE)
  d <- left_join(all_surv, d, by = "survey_series_desc")
  
  d$survey_series_desc <- factor(d$survey_series_desc, levels = surveys)
  
  cols <- c(
    RColorBrewer::brewer.pal(9, "Greens")[2],
    RColorBrewer::brewer.pal(9, "Blues")[2],
    RColorBrewer::brewer.pal(9, "Reds")[2],
    RColorBrewer::brewer.pal(9, "Purples")[2]
  )
  cols <- paste0(cols, "")
  
  par(mfrow = c(6, 2), oma = c(2, .5, .5, .5), cex = 0.7, mar = c(0, 0, 0, 0), mgp = c(2, 0.4, 0),
    tcl = -0.3)
  # par(xpd = NA)
  
  panel <<- 0
  
    plyr::d_ply(d, "survey_series_desc", function(x) {
    
    panel <<- panel + 1
    plot(1, 1, type = "n", xlim = c(2002, 2017) + c(-0.3, 0.3), ylim = c(0, 1.03), axes = FALSE, ann = FALSE,
      xaxs = "i", yaxs = "i")
    abline(v = seq(1960, 2020, 2), col = "grey90")
    if (!is.na(x$year[[1]]) & length(unique(x$year)) > 1) {
      
      main_scale1 <- ggplot2::scale_fill_distiller(palette = "Blues", direction = 1)
      main_scale2 <- ggplot2::scale_fill_distiller(palette = "Reds", direction = 1)
      main_scale3 <- ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)
      main_scale4 <- ggplot2::scale_fill_distiller(palette = "Purples", direction = 1)
      
      shade <- "grey90"
      if (unique(x$survey_series_desc) == "West Coast Haida Gwaii Synoptic Survey")
        shade <- cols[[2]]
      if (unique(x$survey_series_desc) == "Hecate Strait Synoptic Survey")
        shade <- cols[[1]]
      if (unique(x$survey_series_desc) == "West Coast Vancouver Island Synoptic Survey")
        shade <- cols[[4]]
      if (unique(x$survey_series_desc) == "Queen Charlotte Sound Synoptic Survey")
        shade <- cols[[3]]
      
      polygon(c(x$year, rev(x$year)), c(x$lowerci_scaled, rev(x$upperci_scaled)),
        col = shade, border = NA)
      # segments(x0 = x$year, x1 = x$year, y0 = x$lowerci_scaled, y1 = x$upperci_scaled, col = "grey60",
      #   lwd = 1.5)
      lines(x$year, x$biomass_scaled, col = "grey30", lwd = 2.5)
      # par(xpd = NA)
      points(x$year, x$biomass_scaled, pch = 21, col = "grey40", bg = "grey60", cex = 1.25, 
        lwd = 2)
      # par(xpd = FALSE)
    }
    box(col = "grey50")
    add_label(-0.005, 0.15, unique(x$survey_series_desc), col = "grey30", cex = 0.8)
    # par(xpd = FALSE)
    
    if (panel %in% c(11:12))
      axis(1, col = "grey60", col.ticks = "grey70", col.axis = "grey30",
        at = seq(2000, 2040, 4))
    
  })

  
}

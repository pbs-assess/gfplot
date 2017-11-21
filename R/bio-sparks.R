library(tidyverse)

d <- readRDS("~/Dropbox/dfo/data/all-boot-biomass-indices.rds")
names(d) <- tolower(names(d))
d$species_common_name <- tolower(d$species_common_name)
d$species_science_name <- tolower(d$species_science_name)

surveys <- c("Queen Charlotte Sound Synoptic Survey",
  # "Hecate Strait Multispecies Assemblage Survey",
  "Hecate Strait Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey",
  "West Coast Haida Gwaii Synoptic Survey",
  # "Sablefish Offshore Standardized",
  # "Sablefish Stratified Random",
  "PHMA Rockfish Longline Survey - Outside North",
  "PHMA Rockfish Longline Survey - Outside South"
  # "Sablefish Inlet Standardized"
  )

d <- filter(d, survey_series_desc %in% surveys)

# what to plot?
bio <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
names(bio) <- tolower(names(bio))
bio$species_common_name <- tolower(bio$species_common_name)
bio$species_science_name <- tolower(bio$species_science_name)
bio$year <- lubridate::year(bio$trip_start_date)

spp <- group_by(bio, species_common_name) %>% filter(
  year > 2000,
  !is.na(species_common_name),
  species_common_name != "all species",
  !grepl("shrimp", species_common_name),
  !grepl("urchin", species_common_name),
  !grepl("jelly", species_common_name)
) %>% summarise(total_catch = sum(catch_weight, na.rm = TRUE)) %>% 
  arrange(-total_catch) %>% `[`(1:12, 1:2)

d <- inner_join(d, spp) %>% 
  group_by(species_common_name, survey_series_desc) %>% 
  mutate(biomass_scaled = biomass / max(1), 
    lowerci_scaled = lowerci / max(1),
    upperci_scaled = upperci / max(1))

ggplot(d, aes(year, biomass_scaled)) + 
  geom_ribbon(aes(ymin = lowerci_scaled, ymax = upperci_scaled), fill = "grey80", col = NA) +
  # geom_point() +
  facet_grid(survey_series_desc~forcats::fct_reorder(species_common_name, -total_catch), 
    scales = "free_y") +
  # geom_segment(aes(x = year, xend = year, 
    # y = lowerci_scaled, yend = upperci_scaled), colour = "grey50") +
  geom_line(colour = "grey10") +
  ggsidekick::theme_sleek()
  # coord_cartesian(ylim = c(0, 2))
  

d <- readRDS("~/Dropbox/dfo/data/all-catches.rds")
names(d) <- tolower(names(d))
d$species_common_name <- tolower(d$species_common_name)
d$species_scientific_name <- tolower(d$species_scientific_name)
d$year <- lubridate::year(d$best_date)

library(tidyverse)

catches <- d %>% filter(!is.na(species_common_name), !is.na(year), 
  fishery_sector == "GROUNDFISH TRAWL") %>% 
  group_by(species_common_name) %>% 
  summarise(landed_kg = sum(landed_kg, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-landed_kg) %>% 
  `[`(1:50, 1:2)

spp <- gsub("/", "-", gsub(" ", "-", catches$species_common_name))
spp <- spp[-which(spp == "spiny-dogfish")]
spp <- spp[-which(spp == "unknown-fish")] 
spp <- spp[-which(spp == "scorpionfishes")]
spp <- spp[-which(spp == "pacific-herring")]
spp <- spp[-which(spp == "cassin's-auklet")]
spp <- spp[-which(spp == "gulls")]
spp <- spp[-which(spp == "skates")]
spp <- spp[-which(spp == "starry-flounder")]
spp <- spp[-which(spp == "lefteye-flounders")]
spp <- spp[-which(spp == "sculpins")]
spp <- spp[-which(spp == "american-shad")]
spp <- spp[-which(spp == "chub-mackerel")]
spp <- spp[-which(spp == "jack-mackerel")]

spp <- sort(spp)
spp <- c("pacific-ocean-perch", "dover-sole", "spotted-ratfish", "north-pacific-spiny-dogfish", "redbanded-rockfish", 
  "shortraker-rockfish", "sablefish", "walleye-pollock", spp)
spp <- spp[!duplicated(spp)]

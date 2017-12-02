# d <- readRDS("~/Dropbox/dfo/data/all-catches.rds")
# names(d) <- tolower(names(d))
# d$species_common_name <- tolower(d$species_common_name)
# d$species_scientific_name <- tolower(d$species_scientific_name)
# d$year <- lubridate::year(d$best_date)
# 
# library(tidyverse)
# 
# catches <- d %>% filter(!is.na(species_common_name), !is.na(year), 
#   fishery_sector == "GROUNDFISH TRAWL") %>% 
#   group_by(species_common_name) %>% 
#   summarise(landed_kg = sum(landed_kg, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   arrange(-landed_kg) %>% 
#   `[`(1:50, 1:2)
# 
# spp <- catches$species_common_name
# 
# spp <  spp[ which(spp == "spiny dogfish")]
# spp <  spp[ which(spp == "unknown fish")] 
# spp <  spp[ which(spp == "scorpionfishes")]
# spp <  spp[ which(spp == "pacific herring")]
# spp <  spp[ which(spp == "cassin's auklet")]
# spp <  spp[ which(spp == "gulls")]
# spp <  spp[ which(spp == "skates")]
# spp <  spp[ which(spp == "starry flounder")]
# spp <  spp[ which(spp == "lefteye flounders")]
# spp <  spp[ which(spp == "sculpins")]
# spp <  spp[ which(spp == "american shad")]
# spp <  spp[ which(spp == "chub mackerel")]
# spp <  spp[ which(spp == "jack mackerel")]
# 
# spp <- sort(spp)

get_spp_names <- function(file = "data/spp-of-interest.csv") {
  spp <- readr::read_csv(file, 
    col_types = list(species_common_name = readr::col_character(),
    type = readr::col_character()))
  spp$species_common_name <- tolower(gsub(" $", "", spp$species_common_name))
  spp <- dplyr::filter(spp, !species_common_name %in% c(
    "sixgill shark",
    "soupfin shark",
    "pectoral rattail",
    "lamp grenadier",
    "pearly prickleback"
  ))
  
  spp$species_common_name <- sub("spiny dogfish", "north pacific spiny dogfish",
    spp$species_common_name)

  spp <- spp[!duplicated(spp), ]
  # spp <- arrange(spp, type, species_common_name)
    
  spp$spp_w_hyphens <- gsub("/", "-", gsub(" ", "-", spp$species_common_name))
  
  as.data.frame(spp)
}

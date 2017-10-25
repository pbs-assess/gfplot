d <- readRDS("../../Dropbox/dfo/data/all-survey-bio.rds")
names(d) <- tolower(names(d))
d$species_common_name <- tolower(d$species_common_name)
d$species_science_name <- tolower(d$species_science_name)

library(tidyverse)
# library(mapdata)
# library(lubridate)

sp <- filter(d, species_common_name %in% "shortraker rockfish")
pairs(sp[,c("sex", "age", "length", "maturity", "weight")])

# filter(sp, sex == 2) %>% 
# pairs(.[,c("sex", "age", "length", "maturity", "weight")])


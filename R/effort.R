d <- readRDS("data-cache/all-catches.rds")

library(dplyr)
library(ggplot2)

source("R/make-spp-list.R")
torun <- get_spp_names()

d <- filter(d, species_common_name %in% 
    filter(torun, type == "A")$species_common_name)

d <- mutate(d, gear = dplyr::recode(gear,
  UNKNOWN = "Unknown/trawl",
  `BOTTOM TRAWL` = "Bottom trawl",
  `HOOK AND LINE` = "Hook and line",
  `MIDWATER TRAWL` = "Midwater trawl",
  `TRAP` = "Trap",
  `UNKNOWN TRAWL` = "Unknown/trawl"))

dat <- filter(d, year >= 1996)
dat <- dat %>% 
  filter(fishery_sector == "GROUNDFISH TRAWL") %>% 
  mutate(fe_time = 
      as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))) %>% 
  group_by(species_common_name, year) %>% 
  summarise(total_hours = sum(fe_time, na.rm = TRUE)) %>% 
  ungroup()

ggplot(dat, aes(year, total_hours)) +
  geom_col() +
  facet_wrap(~species_common_name, scales = "fixed")

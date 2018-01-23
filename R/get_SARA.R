#========================================================================
# Create Species at Risk Act (SARA) status dataframe
# January 18, 2018
# Elise Keppel
#========================================================================

library(tidyr)

# Import up-to-date dataset from Paul Grant (SARA coordinator)
# Pull in required columns
# Exclude salmon
sara <- readxl::read_xlsx("data/SARA_listings.xlsx") %>%
  select(name, sara_status, range, sara_schedule, date_of_listing) %>%
  filter(!grepl("Salmon", name))

sara <- sara %>%
  mutate(Species_name = sub("\\).*", "", sub(".*\\(", "", name))) %>%
  mutate(Common_name = sub("\\(.*\\)", "", name))

# possibly rename species - such as rougheye rockfish type I and type II

# ------------------
# Or from website:
library(rvest)
h <- read_html("http://www.registrelep-sararegistry.gc.ca/sar/index/default_e.cfm")
d <- h %>% html_nodes("table") %>%
  .[[1]] %>%
  html_table() %>%
  .[-(1:2), ]

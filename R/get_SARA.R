#========================================================================
# Create Species at Risk Act (SARA) status dataframe
# January 18, 2018
# Elise Keppel
#========================================================================

setwd("D:\\GitHub\\pbs-synopsis")

require(dplyr)
require(rio)
require(stringr)


# Import up-to-date dataset from Paul Grant (SARA coordinator)
# Pull in required columns
# Exclude salmon
sara <- import("data/SARA_listings.xlsx") %>%
  select(2,5,6,7,8) %>%
  filter(!grepl("Salmon",name)) %>%
  as_tibble
j <- sara$name

sara <-sara %>%
  mutate(Species_name = sub("\\).*", "", sub(".*\\(", "", j)))

sara <-sara %>%
  mutate(Common_name = sub("\\(.*\\)", "", j))

# possibly rename species - such as rougheye rockfish type I and type II

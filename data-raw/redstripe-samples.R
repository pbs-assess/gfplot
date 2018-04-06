# rs_comm_samples <- readRDS("inst/test-weighting/data-cache/pbs-comm-samples.rds")
# rs_survey_samples <- readRDS("inst/test-weighting/data-cache/pbs-survey-samples.rds")
# rs_catch <- readRDS("inst/test-weighting/data-cache/pbs-catch.rds")
# rs_survey_sets <- readRDS("inst/test-weighting/data-cache/pbs-survey-sets.rds")

rs_comm_samples <- readRDS("../gfsynopsis/report/data-cache/pbs-comm-samples.rds")
rs_survey_samples <- readRDS("../gfsynopsis/report/data-cache/pbs-survey-samples.rds")
rs_catch <- readRDS("../gfsynopsis/report/data-cache/pbs-catch.rds")
rs_survey_sets <- readRDS("../gfsynopsis/report/data-cache/pbs-survey-sets.rds")

spp <- "pacific ocean perch"

rs_survey_sets <- dplyr::filter(rs_survey_sets, species_common_name == spp)
rs_survey_samples <- dplyr::filter(rs_survey_samples, species_common_name == spp)
rs_comm_samples <- dplyr::filter(rs_comm_samples, species_common_name == spp)
rs_catch <- dplyr::filter(rs_catch, species_common_name == spp)

library(dplyr)

# # main age/length data:
# rs_comm_samples <- get_comm_samples("redstripe rockfish", discard_keepers = TRUE)
# rs_survey_samples <- get_survey_samples("redstripe rockfish")
#
# # for weighting:
# rs_catch <- get_catch("redstripe rockfish")
# rs_survey_sets <- get_survey_sets("redstripe rockfish")

rs_comm_samples <- filter(rs_comm_samples, year > 2008, year < 2015)
rs_comm_samples$species_scientific_name <- NULL

rs_survey_samples <- filter(
  rs_survey_samples, year > 2003, year < 2018,
  survey_series_desc %in% c(
    "Queen Charlotte Sound Synoptic Bottom Trawl",
    "West Coast Vancouver Island Synoptic Bottom Trawl"
  )
)
rs_survey_samples$species_scientific_name <- NULL

rs_catch <- filter(rs_catch, year > 2008, year < 2015)
rs_catch$minor_stat_area_code <- NULL
rs_catch$species_scientific_name <- NULL
rs_catch$fe_start_date <- NULL
rs_catch$best_date <- NULL
rs_catch$fishery_sector <- NULL

rs_survey_sets <- filter(
  rs_survey_sets, year > 2003, year < 2018,
  survey_series_desc %in% c(
    "Queen Charlotte Sound Synoptic Bottom Trawl",
    "West Coast Vancouver Island Synoptic Bottom Trawl"
  )
)
rs_survey_sets$species_scientific_name <- NULL

usethis::use_data(rs_comm_samples, rs_survey_samples, rs_catch, rs_survey_sets,
  overwrite = TRUE
)

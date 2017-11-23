library(tidyverse)
source("R/survey_functions.R")
source("R/cpue.R")

main_scale <- viridis::scale_fill_viridis(option = "C")
main_scale <- ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1)
# binary_scale <- ggplot2::scale_fill_gradient2(low = scales::muted("blue"), 
#   mid = "grey90", high = scales::muted("red"), 
#   midpoint = 0.5, limits = c(0, 1))
# spatial_scale <- ggplot2::scale_fill_gradient2(low = scales::muted("blue"), 
#   mid = "grey90", high = scales::muted("red"))

surv_wchg <- fit_spatial_survey_model("arrowtooth flounder",
  c("West Coast Haida Gwaii Synoptic Survey"),
  years = c(2016:2017))
surv_wcvi <- fit_spatial_survey_model("arrowtooth flounder",
  c("West Coast Vancouver Island Synoptic Survey"),
  years = c(2016:2017))
surv_qcs <- fit_spatial_survey_model("arrowtooth flounder",
  c("Queen Charlotte Sound Synoptic Survey"),
  years = c(2015:2017))
surv_hs <- fit_spatial_survey_model("arrowtooth flounder",
  c("Hecate Strait Synoptic Survey"),
  years = c(2016:2017))
save(surv_wchg, surv_wcvi, surv_qcs, surv_hs, file = "arrow-spatial-fits.rda")
# ---

pdf("arrow-spatial-survey.pdf", width = 8, height = 8)
plot_bc_map(surv_wchg$prediction, surv_wchg$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_wcvi$prediction, surv_wcvi$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_qcs$prediction, surv_qcs$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_hs$prediction, surv_hs$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
dev.off()

# --

surv_wchg <- fit_spatial_survey_model("yellowtail rockfish",
  c("West Coast Haida Gwaii Synoptic Survey"),
  years = c(2015:2017))
surv_wcvi <- fit_spatial_survey_model("yellowtail rockfish",
  c("West Coast Vancouver Island Synoptic Survey"),
  years = c(2015:2017))
surv_qcs <- fit_spatial_survey_model("yellowtail rockfish",
  c("Queen Charlotte Sound Synoptic Survey"),
  years = c(2015:2017))
surv_hs <- fit_spatial_survey_model("yellowtail rockfish",
  c("Hecate Strait Synoptic Survey"),
  years = c(2016:2017))
save(surv_wchg, surv_wcvi, surv_qcs, surv_hs, file = "yellowtail-spatial-fits.rda")

# ---

pdf("yellowtail-rockfish-spatial-survey.pdf", width = 8, height = 8)
plot_bc_map(surv_wchg$prediction, surv_wchg$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_wcvi$prediction, surv_wcvi$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_qcs$prediction, surv_qcs$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_hs$prediction, surv_hs$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
dev.off()


surv_wchg <- fit_spatial_survey_model("walleye pollock",
  c("West Coast Haida Gwaii Synoptic Survey"),
  years = c(2015:2017))
surv_wcvi <- fit_spatial_survey_model("walleye pollock",
  c("West Coast Vancouver Island Synoptic Survey"),
  years = c(2015:2017))
surv_qcs <- fit_spatial_survey_model("walleye pollock",
  c("Queen Charlotte Sound Synoptic Survey"),
  years = c(2015:2017))
surv_hs <- fit_spatial_survey_model("walleye pollock",
  c("Hecate Strait Synoptic Survey"),
  years = c(2016:2017))
save(surv_wchg, surv_wcvi, surv_qcs, surv_hs, file = "walleye-spatial-fits.rda")

# ---

pdf("walleye-rockfish-spatial-survey.pdf", width = 8, height = 8)
plot_bc_map(surv_wchg$prediction, surv_wchg$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_wcvi$prediction, surv_wcvi$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_qcs$prediction, surv_qcs$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_hs$prediction, surv_hs$data, "sqrt(combined)", main_scale, 
  pt_col = "#00000080", pt_fill = "#00000040")
dev.off()


##############

dcpue <- readRDS("~/Dropbox/dfo/data/all-spatial-cpue.rds")
pdf("arrow-cpue.pdf", width = 3, height = 3)
plot_spatial_cpue(dcpue, "arrowtooth flounder", bin_width = 7)
dev.off()
##############

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
  arrange(-total_catch) %>% `[`(1:40, 1:2)

for(i in unique(spp$species_common_name)) {
  pdf(paste0("sparks/", gsub("/", "-", gsub(" ", "-", i)), ".pdf"), width = 2.5, height = 4.75)
  plot_index_sparks(i)
  dev.off()
}


source("R/bio.R")

source("R/catches.R")
library(tidyverse)
source("R/survey_functions.R")
source("R/cpue.R")

main_scale <- viridis::scale_fill_viridis(option = "C")
main_scale <- ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1)
main_scale_col <- ggplot2::scale_colour_distiller(palette = "Spectral", direction = -1)
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

pdf("arrow-spatial-survey2.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
plot_bc_map_base(surv_wchg$predictions, surv_wchg$data, "sqrt(combined)",
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map_base(surv_wcvi$predictions, surv_wcvi$data, "sqrt(combined)",
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map_base(surv_qcs$predictions, surv_qcs$data, "sqrt(combined)",
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map_base(surv_hs$predictions, surv_hs$data, "sqrt(combined)",
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
plot_bc_map(surv_wchg$predictions, surv_wchg$data, "sqrt(combined)", 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_wcvi$predictions, surv_wcvi$data, "sqrt(combined)", 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_qcs$predictions, surv_qcs$data, "sqrt(combined)", 
  pt_col = "#00000080", pt_fill = "#00000040")
plot_bc_map(surv_hs$predictions, surv_hs$data, "sqrt(combined)", 
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

main_scale1 <- ggplot2::scale_fill_distiller(palette = "Blues", direction = 1)
main_scale2 <- ggplot2::scale_fill_distiller(palette = "Reds", direction = 1)
main_scale3 <- ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)
main_scale4 <- ggplot2::scale_fill_distiller(palette = "Purples", direction = 1)

pdf("walleye-rockfish-spatial-survey3.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
plot_bc_map_base(surv_wchg$predictions, surv_wchg$data, "sqrt(combined)",
  pt_col = "#00000080", pt_fill = "#00000040", pal_fill = main_scale1)
plot_bc_map_base(surv_hs$predictions, surv_hs$data, "sqrt(combined)",
  pt_col = "#00000080", pt_fill = "#00000040", pal_fill = main_scale3)
plot_bc_map_base(surv_qcs$predictions, surv_qcs$data, "sqrt(combined)",
  pt_col = "#00000080", pt_fill = "#00000040", pal_fill = main_scale2)
plot_bc_map_base(surv_wcvi$predictions, surv_wcvi$data, "sqrt(combined)",
  pt_col = "#00000080", pt_fill = "#00000040", pal_fill = main_scale4)
dev.off()



##############

dcpue <- readRDS("~/Dropbox/dfo/data/all-spatial-cpue.rds")
dir.create("cpue", showWarnings = FALSE)

for(i in tolower(names(rev(sort(table(dcpue$SPECIES_COMMON_NAME)))[1:50]))) {
  print(i)
  pdf(paste0("cpue/", gsub("/", "-", gsub(" ", "-", i)), ".pdf"), width = 3, height = 3)
  plot_spatial_cpue(dcpue, i, bin_width = 7, 
    pal_function = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues")))
  dev.off()
}

##############
# what to plot?
bio <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
names(bio) <- tolower(names(bio))
bio$species_common_name <- tolower(bio$species_common_name)
bio$species_science_name <- tolower(bio$species_science_name)
bio$year <- lubridate::year(bio$trip_start_date)

source("R/bio-sparks.R")

spp <- group_by(bio, species_common_name) %>% filter(
  year > 2000,
  !is.na(species_common_name),
  species_common_name != "all species",
  !grepl("shrimp", species_common_name),
  !grepl("urchin", species_common_name),
  !grepl("jelly", species_common_name)
) %>% summarise(total_catch = sum(catch_weight, na.rm = TRUE)) %>%
  arrange(-total_catch) %>% `[`(1:70, 1:2)


for(i in unique(spp$species_common_name)) {
  pdf(paste0("sparks/", gsub("/", "-", gsub(" ", "-", i)), ".pdf"), width = 4, height = 4.75)
  plot_index_sparks(i)
  dev.off()
}


source("R/bio.R")

source("R/catches.R")

source("R/joy.R")

source("R/joy.R")
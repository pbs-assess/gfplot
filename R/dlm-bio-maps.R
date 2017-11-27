library(tidyverse)
source("R/survey_functions.R")

# main_scale <- viridis::scale_fill_viridis(option = "C")
# main_scale <- ggplot2::scale_fill_distiller(palette = "Blues", direction = -1)
# main_scale_col <- ggplot2::scale_colour_distiller(palette = "Blues", direction = -1)
# binary_scale <- ggplot2::scale_fill_gradient2(low = scales::muted("blue"), 
#   mid = "grey90", high = scales::muted("red"), 
#   midpoint = 0.5, limits = c(0, 1))
# spatial_scale <- ggplot2::scale_fill_gradient2(low = scales::muted("blue"), 
#   mid = "grey90", high = scales::muted("red"))

species <- tolower(c("Squalus suckleyi", "sebastes borealis",
  "Sebastes babcocki", "Microstomus pacificus",
  "Glyptocephalus zachirus", "Hydrolagus colliei",
  "Sebastes alutus"))

d <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
common <- unique(tolower(filter(d, SPECIES_SCIENCE_NAME %in% toupper(species))$SPECIES_COMMON_NAME))

surv_wchg <- fit_spatial_survey_model(common[1],
  c("West Coast Haida Gwaii Synoptic Survey"),
  years = c(2016:2017))
surv_wcvi <- fit_spatial_survey_model(common[1],
  c("West Coast Vancouver Island Synoptic Survey"),
  years = c(2016:2017))
surv_qcs <- fit_spatial_survey_model(common[1],
  c("Queen Charlotte Sound Synoptic Survey"),
  years = c(2015:2017))
surv_hs <- fit_spatial_survey_model(common[1],
  c("Hecate Strait Synoptic Survey"),
  years = c(2016:2017))
save(surv_wchg, surv_wcvi, surv_qcs, surv_hs, file = paste0(common, "-spatial-fits.rda"))
# ---


main_scale1 <- ggplot2::scale_fill_distiller(palette = "Blues", direction = 1)
main_scale2 <- ggplot2::scale_fill_distiller(palette = "Reds", direction = 1)
main_scale3 <- ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)
main_scale4 <- ggplot2::scale_fill_distiller(palette = "Purples", direction = 1)

pdf(paste0(common, "-spatial-survey.pdf"), width = 8, height = 8)
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


dcpue <- readRDS("~/Dropbox/dfo/data/all-spatial-cpue.rds")
dir.create("cpue-dlm", showWarnings = FALSE)

d <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
common <- unique(tolower(filter(dcpue, SPECIES_SCIENTIFIC_NAME %in% toupper(species))$SPECIES_COMMON_NAME))

source("R/cpue.R")

for(i in common) {
  print(i)
  pdf(paste0("cpue-dlm/", gsub("/", "-", gsub(" ", "-", i)), ".pdf"), width = 3, height = 3)
  plot_spatial_cpue(dcpue, i, bin_width = 7, 
    pal_function = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues")))
  dev.off()
}

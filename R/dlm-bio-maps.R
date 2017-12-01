library(tidyverse)
source("R/survey_functions.R")

species <- tolower(c("Squalus suckleyi", "sebastes borealis",
  "Sebastes babcocki", "Microstomus pacificus",
  "Glyptocephalus zachirus", "Hydrolagus colliei",
  "Sebastes alutus", "Sebastes aleutianus"))

d <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
common <- unique(tolower(filter(d, SPECIES_SCIENCE_NAME %in% 
    toupper(species))$SPECIES_COMMON_NAME))

common <- c(common, tolower("ROUGHEYE/BLACKSPOTTED ROCKFISH COMPLEX"))

# as.character(wesanderson::wes_palette("Darjeeling", 5)[-3])

main_scale1 <- ggplot2::scale_fill_distiller(palette = "Blues", direction = 1)
main_scale2 <- ggplot2::scale_fill_distiller(palette = "Reds", direction = 1)
main_scale3 <- ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)
main_scale4 <- ggplot2::scale_fill_distiller(palette = "Purples", direction = 1)

source("R/make-spp-list.R")
common <- spp

# for (i in 1:(length(common)-1)) {
for (i in 1:8) {  
  
  save_file <- paste0("spatial-survey/", 
    gsub("/", "-", gsub(" ", "-", common[i])), "-spatial-fits.rda")
  
  # if (common[i] != "shortraker rockfish") {
    if (!file.exists(save_file)) {
      message(paste("Fitting models for", common[i]))
      surv_wchg <- fit_spatial_survey_model(common[i],
        c("West Coast Haida Gwaii Synoptic Survey"),
        years = c(2016:2017))
      surv_wcvi <- fit_spatial_survey_model(common[i],
        c("West Coast Vancouver Island Synoptic Survey"),
        years = c(2016:2017))
      surv_qcs <- fit_spatial_survey_model(common[i],
        c("Queen Charlotte Sound Synoptic Survey"),
        years = c(2015:2017))
      surv_hs <- fit_spatial_survey_model(common[i],
        c("Hecate Strait Synoptic Survey"),
        years = c(2016:2017))
      save(surv_wchg, surv_wcvi, surv_qcs, surv_hs, 
        file = paste0("spatial-survey/", 
          gsub("/", "-", gsub(" ", "-", common[i])), "-spatial-fits.rda"))
    } else {
      load(save_file)
      message(paste("Reloading", print(common[i]), "models"))
    }
  # }
  
  pdf(paste0("spatial-survey/", gsub("/", "-", gsub(" ", "-", common[i])), 
    "-spatial-survey.pdf"), width = 8, height = 6.46)
  par(mfrow = c(2, 2))
  par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
  
  # plot_predictions <- ifelse(common[i] %in% c("spotted ratfish", "shortraker rockfish"), 
  #   FALSE, TRUE)

  
  message(paste("Plotting maps for", print(common[i])))
  p <- list()
  p[[1]] <- plot_bc_map_base(surv_wchg$predictions, surv_wchg$data, "sqrt(combined)",
    pt_col = "#00000080", pt_fill = "#00000040", pal_fill = main_scale1,
    region = "WCHG", 
    show_model_predictions = ifelse("predictions" %in% names(surv_wchg), TRUE, FALSE))
  p[[2]] <- plot_bc_map_base(surv_hs$predictions, surv_hs$data, "sqrt(combined)",
    pt_col = "#00000080", pt_fill = "#00000040", pal_fill = main_scale3,
    region = "HS", 
    show_model_predictions = ifelse("predictions" %in% names(surv_hs), TRUE, FALSE))
  p[[3]] <- plot_bc_map_base(surv_qcs$predictions, surv_qcs$data, "sqrt(combined)",
    pt_col = "#00000080", pt_fill = "#00000040", pal_fill = main_scale2,
    region = "QCS", 
    show_model_predictions = ifelse("predictions" %in% names(surv_qcs), TRUE, FALSE))
  p[[4]] <- plot_bc_map_base(surv_wcvi$predictions, surv_wcvi$data, "sqrt(combined)",
    pt_col = "#00000080", pt_fill = "#00000040", pal_fill = main_scale4,
    region = "WCVI", 
    show_model_predictions = ifelse("predictions" %in% names(surv_wcvi), TRUE, FALSE))
  dev.off()
  
  names(p) <- c("WCHG", "HS", "QCS", "WCVI")
  saveRDS(p, file = "data/boxes.rds")
  
}

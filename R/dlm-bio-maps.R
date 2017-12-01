library(tidyverse)
source("R/survey_functions.R")

# species <- tolower(c("Squalus suckleyi", "sebastes borealis",
#   "Sebastes babcocki", "Microstomus pacificus",
#   "Glyptocephalus zachirus", "Hydrolagus colliei",
#   "Sebastes alutus", "Sebastes aleutianus"))
# 
# d <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
# common <- unique(tolower(filter(d, SPECIES_SCIENCE_NAME %in% 
#     toupper(species))$SPECIES_COMMON_NAME))
# 
# common <- c(common, tolower("ROUGHEYE/BLACKSPOTTED ROCKFISH COMPLEX"))
# 
# # as.character(wesanderson::wes_palette("Darjeeling", 5)[-3])
# 

source("R/make-spp-list.R")
print(common)

surveys_to_fit <- data.frame(survey = c(
  "West Coast Haida Gwaii Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey",
  "Queen Charlotte Sound Synoptic Survey",
  "Hecate Strait Synoptic Survey"),
  years = c(2016, 2016, 2015, 2017), stringsAsFactors = FALSE)

main_scale1 <- ggplot2::scale_fill_distiller(palette = "Blues", direction = 1)
main_scale2 <- ggplot2::scale_fill_distiller(palette = "Reds", direction = 1)
main_scale3 <- ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)
main_scale4 <- ggplot2::scale_fill_distiller(palette = "Purples", direction = 1)
pal_scales <- list(main_scale1, main_scale2, main_scale3, main_scale4)

for (i in 1:3) {  
  spp_file_name <- gsub("/", "-", gsub(" ", "-", common[i]))
  save_file <- paste0("spatial-survey/", spp_file_name, "-spatial-fits.rda")

  if (!file.exists(save_file)) {
    model_fits <- list()
    message(paste("Fitting models for", common[i]))
    for (j in seq_len(4)) {
      model_fits[[j]] <- fit_spatial_survey_model(common[i],
        survey = surveys_to_fit$survey[j],
        years = surveys_to_fit$years[j], chains = 4L, iter = 1000L, max_knots = 15L,
        adapt_delta = 0.9, thin = 1L, prediction_grid_n = 150L,
        mcmc_posterior_samples = 100L, required_obs_percent = 0.1)
    }
    save(model_fits, 
      file = paste0("spatial-survey/", spp_file_name, "-spatial-fits.rda"))
  } else {
    load(save_file)
    message(paste("Reloading", print(common[i]), "models"))
  }
  
  message(paste("Plotting maps for", print(common[i])))
  pdf(paste0("spatial-survey/", spp_file_name, ".pdf"), 
    width = 8, height = 6.46)
  par(mfrow = c(2, 2))
  par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
  for (j in seq_len(4)) {
    plot_bc_map_base(model_fits[[j]]$predictions, model_fits[[j]]$data, "sqrt(combined)",
      pt_col = "#00000080", pt_fill = "#00000040", pal_fill = pal_scales[[j]],
      region = model_fits[[j]]$region, 
      show_model_predictions = ifelse("combined" %in% 
          names(model_fits[[j]]$predictions), TRUE, FALSE))
  }
  dev.off()
}

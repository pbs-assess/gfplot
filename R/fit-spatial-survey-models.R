library(tidyverse)
source("R/survey_functions.R")

source("R/make-spp-list.R")
spp <- get_spp_names()
common <- spp$species_common_name
d <- readRDS("../../Dropbox/dfo/data/select-survey-spatial-tows.rds")
stopifnot(all(common %in% unique(d$species_common_name)))
# common <- common[common %in% unique(d$species_common_name)] # temp

# TODO FIX ON PC:
 
d <- filter(d, species_science_name != "ophiodontinae") # lingcod duplicate spp.
d <- filter(d, species_science_name != "cetorhinidae") # basking shark duplicate spp.

dup <- group_by(d, species_common_name) %>% 
  summarise(n_spp = length(unique(species_science_name))) %>% 
  arrange(-n_spp) %>% 
  filter(n_spp > 1)
stopifnot(nrow(dup) == 0)

surveys_to_fit <- data.frame(survey = c(
  "West Coast Haida Gwaii Synoptic Survey",
  "Hecate Strait Synoptic Survey",
  "Queen Charlotte Sound Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"),
  years = c(2016, 2017, 2015, 2016), stringsAsFactors = FALSE)

fits_not_good <- tribble(
  ~common,            ~survey,
  "spotted ratfish",  "West Coast Haida Gwaii Synoptic Survey",
  "spotted ratfish",  "West Coast Vancouver Island Synoptic Survey",
  "spotted ratfish",  "Queen Charlotte Sound Synoptic Survey",
  "spotted ratfish",  "Hecate Strait Synoptic Survey",
  "longnose skate",   "Queen Charlotte Sound Synoptic Survey",
  "giant grenadier",  "West Coast Haida Gwaii Synoptic Survey"
)

main_scale1 <- ggplot2::scale_fill_distiller(palette = "Blues", direction = 1)
main_scale2 <- ggplot2::scale_fill_distiller(palette = "Reds", direction = 1)
main_scale3 <- ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)
main_scale4 <- ggplot2::scale_fill_distiller(palette = "Purples", direction = 1)
pal_scales <- list(main_scale1, main_scale2, main_scale3, main_scale4)[c(1, 3, 2, 4)] # TODO re-run and fix

# for (i in 1:length(common)) {
for (i in 1) {
  spp_file_name <- gsub("/", "-", gsub(" ", "-", common[i]))
  save_file <- paste0("spatial-survey/", spp_file_name, "-spatial-fits.rda")

  # if (!file.exists(save_file)) {
    # next
    message(paste("Fitting", common[i], "models"))
    model_fits <- list()
    for (j in seq_len(4)) {
      message(paste0("Fitting models for ", common[i], 
        " with survey ", surveys_to_fit$survey[j]), " in ", surveys_to_fit$years[j], ".")
      model_fits[[j]] <- fit_spatial_survey_model(common[i],
        survey = surveys_to_fit$survey[j],
        years = surveys_to_fit$years[j], chains = 3L, iter = 1200, max_knots = 15L,
        adapt_delta = 0.9, thin = 2L, prediction_grid_n = 150L,
        mcmc_posterior_samples = 100L, required_obs_percent = 0.1)
    }
    save(model_fits, 
      file = paste0("spatial-survey/", spp_file_name, "-spatial-fits.rda"))
  # } else {
    # load(save_file)
    # message(paste("Reloading", common[i], "models"))
  # }
  # 
  message(paste0("Plotting maps for ", common[i]))
  pdf(paste0("spatial-survey/", spp_file_name, ".pdf"),
    width = 8, height = 6.46)
  par(mfrow = c(2, 2))
  par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
  k <- 0 # TODO re-run and fix
  for (j in c(1, 4, 3, 2)) { # TODO re-run and fix
    k <- k + 1 # TODO re-run and fix
    plot_bc_map_base(model_fits[[j]]$predictions, model_fits[[j]]$data, "sqrt(combined)",
      pt_col = "#00000080", pt_fill = "#00000040", pal_fill = pal_scales[[k]],
      region = model_fits[[j]]$region, 
      show_model_predictions = ifelse(
        "combined" %in% names(model_fits[[j]]$predictions) & 
          (!paste0(common[i], model_fits[[j]]$survey) %in% 
              paste0(fits_not_good$common, fits_not_good$survey)), 
        TRUE, FALSE))
  }
  dev.off()
}

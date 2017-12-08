library(tidyverse)

# source("R/get-dat.R") # must be on DFO-networked PC
source("R/survey-functions.R")
source("R/cpue.R")
source("R/bio-indices.R")
source("R/make-spp-list.R")
torun <- get_spp_names()$species_common_name

dcpue <- readRDS("data-cache/all-spatial-cpue.rds")
dir.create("cpue", showWarnings = FALSE)

for(i in torun) {
  message(i)
  pdf(paste0("cpue/", gsub("/", "-", gsub(" ", "-", i)), ".pdf"),
    width = 3, height = 3)
  plot_spatial_cpue(dcpue, i, bin_width = 7,
    pal_function = colorRampPalette((RColorBrewer::brewer.pal(7, "Blues"))))
  dev.off()
}

for(i in torun) {
  message(i)
  pdf(paste0("sparks/", gsub("/", "-", gsub(" ", "-", i)), ".pdf"),
    width = 4.5, height = 4.55)
  plot_index_sparks(i)
  dev.off()
}

source("R/bio-availability.R")
source("R/catches.R")
source("R/length.R")
source("R/fit-spatial-survey-models.R")
source("R/age-bubbles.R")
source("R/growth.R")

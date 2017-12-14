library(tidyverse)

sanitize_common_name <- function(x) {
  gsub("/", "-", gsub(" ", "-", x))
}

# source("R/get-dat.R") # must be on DFO-networked PC
source("R/survey-functions.R")

source("R/cpue.R")
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

source("R/bio-indices.R")
dir.create("sparks", showWarnings = FALSE)
for(i in torun) {
  message(i)
  pdf(paste0("sparks/", sanitize_common_name(i), ".pdf"),
    width = 4.5, height = 4.55)
  plot_index_sparks(i)
  dev.off()
}

dir.create("synop", showWarnings = FALSE)
source("R/bio-availability.R")
dbio <- readRDS("data-cache/all-survey-bio.rds")
dbio_c <- readRDS("data-cache/all-commercial-bio.rds")
bio <- summarize_bio_avail(dbio, dbio_c)
source("R/make-spp-list.R")
torun <- get_spp_names()$species_common_name
for (i in seq_along(torun)) {
  pdf(paste0("synop/dat-syn-", sanitize_common_name(torun[i]), ".pdf"),
    width = 6, height = 2.75)
  message(torun[i])
  plot_bio_avail(common_name = torun[i], bio)
  dev.off()
}

source("R/catches.R")
source("R/length.R")
source("R/fit-spatial-survey-models.R")
source("R/age-bubbles.R")
source("R/growth.R")

###########################################################################
# ----------
library(tidyverse)
source("R/make-spp-list.R")
torun <- get_spp_names() %>% 
  filter(type == "A")
d <- readRDS("data-cache/all-boot-biomass-indices.rds")
d <- dplyr::filter(d, species_science_name != "ophiodontinae") # lingcod duplicate spp.
d <- dplyr::filter(d, species_science_name != "cetorhinidae") # basking shark duplicate spp.
d <- filter(d, species_common_name %in% torun$species_common_name)
d <- filter(d, survey_series_desc %in% c(
      "West Coast Haida Gwaii Synoptic Survey",
      "Hecate Strait Synoptic Survey",
      "Queen Charlotte Sound Synoptic Survey",
      "West Coast Vancouver Island Synoptic Survey",
      "PHMA Rockfish Longline Survey - Outside North",
      "PHMA Rockfish Longline Survey - Outside South",
      # "Sablefish Offshore Standardized",
      # "Sablefish Inlet Standardized",
      # "Sablefish Stratified Random",
      "IRF Longline Survey (North)",
      "IRF Longline Survey (South)",
      "Hecate Strait Multispecies Assemblage Survey",
      # "Restratification of Goose Island Gully for the 2009 POP Assessment.",
      "Queen Charlotte Sound Shrimp Survey",
      "West Coast Vancouver Island Shrimp Survey",
      "IPHC Longline Survey"
    ))
q <- group_by(d, survey_series_desc, species_common_name) %>% 
  summarise(
    cv_time = sd(biomass, na.rm=TRUE)/mean(biomass, na.rm=TRUE),
    median_cv = median(re, na.rm=TRUE),
    median_frac_pos_sets = median(num_pos_sets/num_sets))

q <- mutate(q, survey_type = case_when(
  grepl("shrimp", tolower(survey_series_desc)) ~ "shrimp",
  grepl("synoptic", tolower(survey_series_desc)) ~ "synoptic",
  grepl("sablefish", tolower(survey_series_desc)) ~ "sablefish",
  grepl("rockfish", tolower(survey_series_desc)) ~ "rockfish",
  grepl("irf", tolower(survey_series_desc)) ~ "irf longline",
  grepl("assemblage", tolower(survey_series_desc)) ~ "assemblage",
  grepl("iphc", tolower(survey_series_desc)) ~ "iphc",
  grepl("jig", tolower(survey_series_desc)) ~ "jig",
  TRUE ~ "other"
))

g <- ggplot(q, aes(cv_time, 
  forcats::fct_reorder(survey_series_desc, as.numeric(as.factor(survey_type))),
  colour = survey_type)) + 
  geom_point() +
  facet_wrap(~species_common_name, nrow = 3, scales = "fixed") +
  scale_color_brewer(palette = "Dark2") +
  ggsidekick::theme_sleek() +
  ylab("") + xlab("Metric") +
  ggtitle("CV of biomass through time") +
  guides(colour = FALSE)

ggsave("figs/cv_time.pdf", width = 24, height = 8)

g <- ggplot(q, aes(median_cv, 
  forcats::fct_reorder(survey_series_desc, as.numeric(as.factor(survey_type))),
  colour = survey_type)) + 
  geom_point() +
  facet_wrap(~species_common_name, nrow = 3, scales = "fixed") +
  scale_color_brewer(palette = "Dark2") +
  ggsidekick::theme_sleek() +
  ylab("") + xlab("Metric") +
  ggtitle("Median CV") +
  guides(colour = FALSE)

ggsave("figs/median_cv.pdf", width = 24, height = 8)

g <- ggplot(q, aes(median_frac_pos_sets, 
  forcats::fct_reorder(survey_series_desc, as.numeric(as.factor(survey_type))),
  colour = survey_type)) + 
  geom_point() +
  facet_wrap(~species_common_name, nrow = 3, scales = "fixed") +
  scale_color_brewer(palette = "Dark2") +
  ggsidekick::theme_sleek() +
  ylab("") + xlab("Metric") +
  ggtitle("Median fraction positive sets") +
  guides(colour = FALSE)

ggsave("figs/frac_pos.pdf", width = 24, height = 8)

library("PBSsynopsis")
library("tidyverse")

spp <- "pacific ocean perch"

d_index <- readRDS("data-cache/all-boot-biomass-indices.rds")
d_surv_samp <- readRDS("data-cache/all-survey-bio.rds")
d_catch <- readRDS("data-cache/all-catches.rds")

filter(d_index, species_common_name == spp) %>%
  prep_pbs_bioindex() %>%
  plot_bioindex()
ggplot2::ggsave("report/bioindex.pdf", width = 5, height = 5)

filter(d_surv_samp, species_common_name == spp) %>%
  prep_pbs_ages() %>%
  plot_ages(max_size = 3.7, sex_gap = 0.25)
ggplot2::ggsave("report/ages.pdf", width = 13, height = 5)

# TODO:
plot_lengths(d_surv_samp)
ggplot2::ggsave("report/lengths.pdf", width = 9, height = 6)

filter(d_catch, species_common_name == spp) %>%
  prep_pbs_catch() %>%
  plot_catch()
ggplot2::ggsave("report/catch.pdf", width = 6, height = 2)

filter(d_surv_samp, species_common_name == spp) %>%
  prep_pbs_samples(year_range = c(1996, 2016)) %>%
  plot_samples(x)
ggplot2::ggsave("report/samples.pdf", width = 6, height = 1.5)

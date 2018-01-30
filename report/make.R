library("PBSsynopsis")
library("tidyverse")

d_index <- readRDS("data-cache/all-boot-biomass-indices.rds")
d_surv_samp <- readRDS("data-cache/all-survey-bio.rds")
d_catch <- readRDS("data-cache/all-catches.rds")

# filter(d_surv_samp, survey_series_desc=="PHMA Rockfish Longline Survey - Outside South", !is.na(length)) %>% pull(species_common_name) %>% table %>% sort() %>% rev()
#
# filter(d_surv_samp, survey_series_desc=="IRF Longline Survey (North)", !is.na(length)) %>% pull(species_common_name) %>% table %>% sort() %>% rev()
#
# filter(d_surv_samp, survey_series_desc=="IPHC Longline Survey", !is.na(length)) %>% pull(species_common_name) %>% table %>% sort() %>% rev()


# spp <- "yelloweye rockfish"

sn <- get_spp_names() %>%
  filter(type == "A") %>%
  filter(species_common_name %in% c("pacific ocean perch", "yelloweye rockfish"))

for (i in seq_along(sn$species_common_name)) {

  spp <- sn$species_common_name[i]
  spp_f <- sn$spp_w_hyphens[i]
  message(spp)

  # g <- filter(d_index, species_common_name == spp) %>%
  #   prep_pbs_bioindex() %>%
  #   plot_bioindex()
  # ggsave(paste0("report/figs/", spp_f, "-bioindex.pdf"), width = 5, height = 5)

  # g <- filter(d_surv_samp, species_common_name == spp) %>%
  #   prep_pbs_ages() %>%
  #   plot_ages(max_size = 3.7, sex_gap = 0.25)
  # ggsave(paste0("report/figs/", spp_f, "-ages.pdf"), width = 13, height = 5)

  # # TODO:
  g <- filter(d_surv_samp, species_common_name == spp) %>%
    plot_lengths(n_bins = 25)
  ggsave(paste0("report/figs/", spp_f, "-lengths.pdf"), width = 8, height = 6)
  #
  # g <- filter(d_catch, species_common_name == spp) %>%
  #   prep_pbs_catch() %>%
  #   plot_catch()
  # ggsave(paste0("report/figs/", spp_f, "-catch.pdf"), width = 6, height = 2)
  #
  # g <- filter(d_surv_samp, species_common_name == spp) %>%
  #   prep_pbs_samples(year_range = c(1996, 2016)) %>%
  #   plot_samples()
  # ggsave(paste0("report/figs/", spp_f, "-samples.pdf"), width = 6, height = 1.5)

}

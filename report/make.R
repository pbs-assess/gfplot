library("PBSsynopsis")


d <- readRDS("data-cache/all-boot-biomass-indices.rds")
d <- prep_pbs_bioindex(d, species = "pacific ocean perch")
g <- plot_bioindex(d)
ggplot2::ggsave("report/bioindex.pdf", width = 5, height = 5)

d1 <- prep_pbs_ages(spp = "pacific ocean perch")
g <- plot_ages(d1, max_size = 3.7, sex_gap = 0.25)
ggplot2::ggsave("report/ages.pdf", width = 13, height = 5)

g <- plot_lengths()
ggplot2::ggsave("report/lengths.pdf", width = 9, height = 6)

d2 <- prep_pbs_catch("pacific ocean perch")
g <- plot_catch(d2)
ggplot2::ggsave("report/catch.pdf", width = 6, height = 2)

x <- prep_pbs_samples(year_range = c(1996, 2016))
x <- dplyr::filter(x, species_common_name == "pacific ocean perch")
g <- plot_samples(x) + ggplot2::ggtitle("Survey samples")
ggplot2::ggsave("report/samples.pdf", width = 6, height = 1.5)

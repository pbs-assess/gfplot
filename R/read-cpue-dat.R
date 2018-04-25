library(dplyr)
library(ggplot2)

## d <- readRDS("cache/pbs-cpue.rds")
d <- d_cpue

names(d) <- tolower(names(d))
d <- rename(d, total = totcatch_kg, minor_stat_area_code = min)
d$hours_fished <- as.numeric(as.character(d$hours_fished))
d$database_name <- tolower(d$database_name)
d$gear <- tolower(d$gear)
d$locality_description <- tolower(d$locality_description)

areas <- c("3[CD]+", "4B", "5[AB]+", "5[CDE]+")
d$area <- NA
for (i in seq_along(areas)) {
  d[grepl(areas[[i]], d$major_stat_area_description), "area"] <-
    gsub("\\[|\\]|\\+", "", areas[[i]])
}
specific_areas <- c("3C", "3D", "4B", "5A", "5B", "5C", "5D", "5E")
d$specific_area <- NA
for (i in seq_along(specific_areas)) {
  d[grepl(specific_areas[[i]], d$major_stat_area_description), "specific_area"] <-
    specific_areas[[i]]
}

dsum <- d %>%
  mutate(key_locality = case_when(
    specific_area == "5A" & locality_code %in% c(1, 2, 3, 4) ~ TRUE,
    specific_area == "5B" & locality_code %in% c(1, 2, 3, 4) ~ TRUE,
    specific_area == "5C" & minor_stat_area_code == 2 & locality_code %in% c(1, 2, 3) ~ TRUE,
    specific_area == "5C" & minor_stat_area_code == 6 & locality_code %in% c(1, 2, 10) ~ TRUE,
    specific_area == "5D" & minor_stat_area_code == 1 & locality_code %in% c(5) ~ TRUE,
    specific_area == "5D" & minor_stat_area_code == 4 & locality_code %in% c(1, 2) ~ TRUE,
    specific_area == "5D" & minor_stat_area_code == 5 & locality_code %in% c(1, 3) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(hours_fished < 2000) %>%
  filter(!is.na(hours_fished), !is.na(total), total > 0, hours_fished > 0) %>%
  # filter(!is.na(hours_fished), !is.na(total), hours_fished > 0) %>%
  # filter(key_locality) %>%
  # filter(best_depth_m > 0, best_depth_m < 150) %>%
  group_by(area, fyear) %>%
  summarise(
    catch = sum(total, na.rm = TRUE),
    sum_hours_fished = sum(hours_fished, na.rm = TRUE),
    arith_cpue = sum(total, na.rm = TRUE) / sum(hours_fished, na.rm = TRUE),
    geo_cpue = exp(mean(log(total / hours_fished), na.rm = TRUE))
  )

dsum <- dsum %>%
  mutate(arith_cpue = arith_cpue / exp(mean(log(arith_cpue)))) %>%
  mutate(geo_cpue = geo_cpue / exp(mean(log(geo_cpue))))

pdf("data/flatfish_cpue.pdf")
ggplot(dsum, aes(fyear, arith_cpue)) +
  geom_line(lty = 2) +
  geom_line(aes(y = geo_cpue), lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
dev.off()

pdf("data/flatfish_catch.pdf")
ggplot(dsum, aes(fyear, catch)) +
  geom_line(lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
dev.off()

pdf("data/flatfish_effort.pdf")
ggplot(dsum, aes(fyear, sum_hours_fished)) +
  geom_line(lty = 1) +
  geom_vline(xintercept = 1996, lty = 3) +
  facet_wrap(~area, scales = "free_y")
dev.off()

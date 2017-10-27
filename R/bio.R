library(tidyverse)

###########
common_name <- "yelloweye rockfish"
##########

if (!exists("dbio")) {
  dbio <- readRDS("../../Dropbox/dfo/data/all-survey-bio.rds")
  names(dbio) <- tolower(names(dbio))
  dbio$species_common_name <- tolower(dbio$species_common_name)
  dbio$species_science_name <- tolower(dbio$species_science_name)
  dbio <- mutate(dbio, year = lubridate::year(trip_start_date))
}
# sp <- filter(dbio, species_common_name %in% "shortraker rockfish")
# pairs(sp[,c("sex", "age", "length", "maturity", "weight")])

bio <- group_by(dbio, species_common_name, species_science_name, year) %>% 
  summarise(
    n_age = sum(!is.na(age)),
    n_length = sum(!is.na(length)),
    n_weight = sum(!is.na(weight)),
    n_maturity = sum(!is.na(maturity))
  ) %>% ungroup

if (!exists("dsurv")) {
  dsurv <- readRDS("../../Dropbox/dfo/data/all-survey-dat.rds")
  names(dsurv) <- tolower(names(dsurv))
  dsurv$species_common_name <- tolower(dsurv$species_common_name)
  dsurv$species_science_name <- tolower(dsurv$species_science_name)
  dsurv <- mutate(dsurv, year = lubridate::year(trip_start_date))
}

surv <- group_by(dsurv, species_common_name, species_science_name, 
  survey_series_desc, year) %>% 
  summarise(
    n_fe_weight = sum(!is.na(catch_weight)),
    n_fe_count = sum(!is.na(catch_count))
  ) %>% ungroup

labs <- data.frame(survey_series_desc = c("Hecate Strait Synoptic Survey", 
  "IPHC Longline Survey", 
  "PHMA Rockfish Longline Survey - Outside North", 
  "PHMA Rockfish Longline Survey - Outside South",
  "Queen Charlotte Sound Synoptic Survey", 
  "West Coast Haida Gwaii Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"),
  surv_short = c("HS", "IPHC", "PHMA N", "PHMA S", "QCS", "WCHG", "WCVI"), 
  stringsAsFactors = FALSE)

sps <- filter(surv, species_common_name %in% "yelloweye rockfish") %>% 
  select(-species_common_name, -species_science_name) %>% 
  inner_join(labs)

qq <- sps %>% mutate(combined = n_fe_weight + n_fe_count) %>% 
  filter(combined > 5) %>% 
  reshape2::dcast(year ~ surv_short, value.var = "combined") %>%
  right_join(data.frame(year = years))

#########

spb <- filter(bio, species_common_name %in% common_name) %>% 
  select(-species_common_name, -species_science_name)
spb <- left_join(data.frame(year = years), spb)

spb <- rename(spb, 
  `# Maturity` = n_maturity,
  `# Weight` = n_weight,
  `# Length` = n_length,
  `# Age` = n_age
)

###############
years <- seq(1996, 2016)
labs <- intersect(seq(1800, 3000, 2), years)
col <- viridis::viridis(200)
col <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(200)
grid_col <- "grey75"
box_col <- "grey55"
grid_lwd <- 1.3
surv_col <- "grey33"
axis_col <- "grey30"
###############

pdf("bio-yellow-eye.pdf", width = 6, height = 3.5)
layout(c(rep(1, 7), rep(2, 5)))
par(mar = c(2.1, 5, .5, .5), cex = 0.8, oma = c(.5, .5, .5, .5))

image(x = qq$year, y = seq(1, ncol(qq) - 1), z = as.matrix(log(qq[, -1])), axes = FALSE,
  xlab = "", ylab = "", col = surv_col)
axis(1, at = labs, labels = labs, tick = 0, line = -0.5, col.axis = axis_col)
axis(2, at = seq(1, ncol(qq) - 1), labels = names(qq)[-1], las = 1, tick = 0, 
  line = -0.2, col.axis = axis_col)
abline(v = c(years, max(years)+1)-.5, col = grid_col, lwd = grid_lwd)
abline(h = 1:ncol(qq)-.5, col = grid_col, lwd = grid_lwd)
box(col = box_col, lwd = grid_lwd)

image(x = spb$year, y = 1:4, z = as.matrix(log(spb[, -1])), axes = FALSE,
  xlab = "", ylab = "", col = col)

axis(1, at = labs, labels = labs, tick = 0, line = -0.5, col.axis = axis_col)
axis(2, at = 1:4, labels = names(spb)[-1], las = 1, tick = 0, line = -0.2,
  col.axis = axis_col)
abline(v = c(years, max(years)+1)-.5, col = grid_col, lwd = grid_lwd)
abline(h = 1:5-.5, col = grid_col, lwd = grid_lwd)
box(col = box_col, lwd = grid_lwd)
dev.off()
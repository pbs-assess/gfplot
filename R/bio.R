library(tidyverse)

if (!exists("dbio")) {
  dbio <- readRDS("../../Dropbox/dfo/data/all-survey-bio.rds")
  names(dbio) <- tolower(names(dbio))
  dbio$species_common_name <- tolower(dbio$species_common_name)
  dbio$species_science_name <- tolower(dbio$species_science_name)
  dbio <- mutate(dbio, year = lubridate::year(trip_start_date))
}

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
  ) %>% ungroup %>% 
  filter(!survey_series_desc %in% "Queen Charlotte Sound Shrimp Survey")

surveys <- data.frame(survey_series_desc = c("Hecate Strait Synoptic Survey", 
  "IPHC Longline Survey", 
  "PHMA Rockfish Longline Survey - Outside North", 
  "PHMA Rockfish Longline Survey - Outside South",
  "Queen Charlotte Sound Synoptic Survey", 
  "West Coast Haida Gwaii Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"),
  surv_short = c("HS", "IPHC", "PHMAN", "PHMAS", "QCS", "WCHG", "WCVI"), 
  stringsAsFactors = FALSE)

all_surv_n <- group_by(surv, survey_series_desc, year) %>% 
  summarise(n = sum(c(n_fe_weight, n_fe_count)))

###############
years <- seq(1996, 2016)
labs <- intersect(seq(1800, 3000, 2), years)
# col <- viridis::viridis(200, option = "D")
col <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(200)
# col <- colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral"))(200)
grid_col <- "grey75"
box_col <- "grey55"
grid_lwd <- 1.3
surv_col <- "grey33"
axis_col <- "grey30"
###############

# when were surveys done?
qq_all <- all_surv_n %>% 
  right_join(expand.grid(year = years, 
    survey_series_desc = unique(surv$survey_series_desc), stringsAsFactors = FALSE)) %>% 
  mutate(present = ifelse(!is.na(n), TRUE, FALSE)) %>% 
  select(-n) %>% ungroup() %>% 
  left_join(surveys) %>% 
  select(-survey_series_desc) %>% 
  reshape2::dcast(year ~ surv_short, value.var = "present")

######

torun <- names(rev(sort(table(dbio$species_common_name)))[seq_len(1)])

for (i in seq_along(torun)) {
  
  common_name <- torun[i]
  
  sps <- filter(surv, species_common_name %in% common_name) %>% 
    select(-species_common_name, -species_science_name) %>% 
    inner_join(surveys)
  
  qq <- sps %>% mutate(combined = n_fe_weight + n_fe_count) %>% 
    filter(combined > 5) %>% 
    reshape2::dcast(year ~ surv_short, value.var = "combined") %>%
    right_join(data.frame(year = years))
  
  # fix this!!! how ugly
  # filling in zeros for missing surveys
  if (!"HS" %in% names(qq))  qq <- left_join(data.frame(HS = NA, year = years), qq)
  if (!"IPHC" %in% names(qq))  qq <- left_join(data.frame(IPHC = NA, year = years), qq)
  if (!"PHMAN" %in% names(qq))  qq <- left_join(data.frame(PHMAN = NA, year = years), qq)
  if (!"PHMAS" %in% names(qq))  qq <- left_join(data.frame(PHMAS = NA, year = years), qq)
  if (!"QCS" %in% names(qq))  qq <- left_join(data.frame(QCS = NA, year = years), qq)
  if (!"WCHG" %in% names(qq))  qq <- left_join(data.frame(WCHG = NA, year = years), qq)
  if (!"WCVI" %in% names(qq))  qq <- left_join(data.frame(WCVI = NA, year = years), qq)
  qq <- dplyr::select(qq, year, HS, IPHC, PHMAN, PHMAS, QCS, WCHG, WCVI)
  # names(qq) <- gsub("PHMAS", "PHMA S", names(qq))
  # names(qq) <- gsub("PHMAN", "PHMA N", names(qq))
  for (i in 2:ncol(qq)) {
    qq[,i][is.na(qq[,i])] <- 0
  }
  
  assertthat::assert_that(identical(names(qq_all), names(qq)))
  qq_combined <- matrix(NA, nrow = nrow(qq), ncol = ncol(qq))
  for (i in seq_along(qq$year)) {
    for (j in seq(2, ncol(qq))) {
      if(qq[i, j] > 0) qq_combined[i, j] <- 2
      if(qq[i, j] == 0 & qq_all[i, j] == TRUE) qq_combined[i, j] <- 1
    }
  }
  
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
  
  #########
  
  pdf(paste0("synop/dat-syn-", gsub("/", "-", gsub(" ", "-", common_name)), ".pdf"), 
    width = 6, height = 3.5)
  layout(c(rep(1, 7), rep(2, 5)))
  par(mar = c(2, 4.75, .5, .5), cex = 0.8, oma = c(0, .5, .5, .5))
  
  image(x = qq$year, y = seq(1, ncol(qq) - 1), z = qq_combined[, -1], axes = FALSE,
    xlab = "", ylab = "", col = c(grid_col, "grey25"))
  axis(1, at = labs, labels = labs, tick = 0, line = -0.65, col.axis = axis_col)
  axis(2, at = seq(1, ncol(qq) - 1), labels = names(qq)[-1], las = 1, tick = 0, 
    line = -0.4, col.axis = axis_col)
  abline(v = c(years, max(years)+1)-.5, col = grid_col, lwd = grid_lwd)
  abline(h = 1:ncol(qq)-.5, col = grid_col, lwd = grid_lwd)
  box(col = box_col, lwd = grid_lwd)

  image(x = spb$year, y = 1:4, z = as.matrix(log(spb[, -1])), axes = FALSE,
    xlab = "", ylab = "", col = col)
  
  axis(1, at = labs, labels = labs, tick = 0, line = -0.65, col.axis = axis_col)
  axis(2, at = 1:4, labels = names(spb)[-1], las = 1, tick = 0, line = -0.4,
    col.axis = axis_col)
  abline(v = c(years, max(years)+1)-.5, col = grid_col, lwd = grid_lwd)
  abline(h = 1:5-.5, col = grid_col, lwd = grid_lwd)
  box(col = box_col, lwd = grid_lwd)
  
  max_val <- max(spb[,-1], na.rm = TRUE)
  # round_clean <- function(x) 10^round(log10(x))
  round_nice <- function(x, nice=c(1,2,3,4,5,6,7,8,9,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^round(log10(x)) * nice[[which(x <= 10^round(log10(x)) * nice)[[1]]]]
  }
  max_ind <- which(spb[,-1] == max_val, arr.ind = TRUE)
  max_ind <- max_ind[1,,drop = FALSE]
  text(spb$year[max_ind[,"row"][[1]]], max_ind[,"col"][[1]],
    plyr::round_any(max_val, 100), col = "white", cex = 0.6)
  
  # for (i in seq(1, ncol(qq) - 1))
  # text(spb$year, i, plyr::round_any(qq[,i], 100), col = "white", cex = 0.6)
  
  dev.off()
  
}
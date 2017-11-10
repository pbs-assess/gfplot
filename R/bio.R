library(tidyverse)

if (!exists("dsurv")) {
  dsurv <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
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

pp <- filter(surv, year >= 1996)
sort(table(pp$survey_series_desc))

ppp <- group_by(pp, survey_series_desc, year) %>% summarise(n = n())
ggplot(ppp, aes(year, n)) + facet_wrap(~survey_series_desc)+ geom_point()

surveys <- data.frame(survey_series_desc = c("Hecate Strait Synoptic Survey", 
  "IPHC Longline Survey", 
  "PHMA Rockfish Longline Survey - Outside North", 
  "PHMA Rockfish Longline Survey - Outside South",
  "Queen Charlotte Sound Synoptic Survey", 
  "West Coast Haida Gwaii Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey",
  "Sablefish Stratified Random"),
  surv_short = c("HS", "IPHC", "PHMAN", "PHMAS", "QCS", "WCHG", "WCVI", "SabStRS"), 
  stringsAsFactors = FALSE)

all_surv_n <- group_by(surv, survey_series_desc, year) %>% 
  summarise(n = sum(c(n_fe_weight, n_fe_count)))

if (!exists("dbio")) {
  dbio <- readRDS("../../Dropbox/dfo/data/all-survey-bio.rds")
  names(dbio) <- tolower(names(dbio))
  dbio$species_common_name <- tolower(dbio$species_common_name)
  dbio$species_science_name <- tolower(dbio$species_science_name)
  dbio <- mutate(dbio, year = lubridate::year(trip_start_date))
  
  ss <- readRDS("../../Dropbox/dfo/data/survey_series.rds") %>%
    select(-SURVEY_SERIES_TYPE_CODE)
  names(ss) <- tolower(names(ss))
  surv2 <- left_join(surv, ss)
  dbio <- filter(dbio, survey_series_id %in% unique(surv2$survey_series_id)) %>% 
    select(species_common_name, species_science_name, year, age, length, weight, maturity)
}

# commercial bio samples:
dbio_c <- readRDS("../../Dropbox/dfo/data/all-commercial-bio.rds")
names(dbio_c) <- tolower(names(dbio_c))
dbio_c$species_common_name <- tolower(dbio_c$species_common_name)
dbio_c$species_science_name <- tolower(dbio_c$species_science_name)
dbio_c <- select(dbio_c, species_common_name, species_science_name, year, age, length, weight, maturity)

# ss <- readRDS("../../Dropbox/dfo/data/survey_series.rds") %>%
#   select(-SURVEY_SERIES_TYPE_CODE)
# names(ss) <- tolower(names(ss))
# ds <- left_join(dbio, ss) %>% 
#   filter(year >= 1996)
# rev(sort(table(ds$survey_series_desc)))[1:20]
# q <- filter(ds, species_common_name == "pacific ocean perch")
# rev(sort(table(q$survey_series_desc)))
# 
# q$id <- paste(q$trip_start_date, q$sex, q$age, q$length, q$maturity, q$weight)
# qd <- q[duplicated(select(q, trip_start_date, sex, age, length, maturity, weight)), ]
# 
# filter(q, id %in% "2003-07-03 2 35 45.2 7 1257")
# filter(q, id %in% "2011-07-05 2 10 37.5 7 764")
# filter(q, id %in% "2004-07-05 1 NA 36.1 3 600")

dbio_combined <- bind_rows(dbio, dbio_c)

bio <- group_by(dbio_combined, species_common_name, species_science_name, year) %>% 
  summarise(
    n_age = sum(!is.na(age)),
    n_length = sum(!is.na(length)),
    n_weight = sum(!is.na(weight)),
    n_maturity = sum(!is.na(maturity))
  ) %>% ungroup

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
  inner_join(surveys) %>% 
  select(-survey_series_desc) %>% 
  reshape2::dcast(year ~ surv_short, value.var = "present") %>% 
  select(year, SabStRS, IPHC, PHMAS, PHMAN, WCVI, QCS, HS, WCHG) # reorder
names(qq_all) <- gsub("PHMAS", "PHMA S", names(qq_all))
names(qq_all) <- gsub("PHMAN", "PHMA N", names(qq_all))


######

torun <- names(rev(sort(table(dbio$species_common_name)))[seq_len(50)])

for (i in seq_along(torun)) {
  
  common_name <- torun[i]
  
  sps <- filter(surv, species_common_name %in% common_name) %>% 
    select(-species_common_name, -species_science_name) %>% 
    inner_join(surveys)
  
  qq <- sps %>% mutate(combined = n_fe_weight + n_fe_count) %>% 
    filter(combined > 3) %>% 
    reshape2::dcast(year ~ surv_short, value.var = "combined") %>%
    right_join(data.frame(year = years))
  
  # fix this!!! how ugly
  # filling in zeros for missing surveys
  if (!"HS" %in% names(qq))  qq <- left_join(data.frame(HS = NA, year = years), qq)
  if (!"SabStRS" %in% names(qq))  qq <- left_join(data.frame(SabStRS = NA, year = years), qq)
  if (!"IPHC" %in% names(qq))  qq <- left_join(data.frame(IPHC = NA, year = years), qq)
  if (!"PHMAN" %in% names(qq))  qq <- left_join(data.frame(PHMAN = NA, year = years), qq)
  if (!"PHMAS" %in% names(qq))  qq <- left_join(data.frame(PHMAS = NA, year = years), qq)
  if (!"QCS" %in% names(qq))  qq <- left_join(data.frame(QCS = NA, year = years), qq)
  if (!"WCHG" %in% names(qq))  qq <- left_join(data.frame(WCHG = NA, year = years), qq)
  if (!"WCVI" %in% names(qq))  qq <- left_join(data.frame(WCVI = NA, year = years), qq)
  qq <- dplyr::select(qq, year, SabStRS, IPHC, PHMAS, PHMAN, WCVI, QCS, HS, WCHG) # reorder
  names(qq) <- gsub("PHMAS", "PHMA S", names(qq))
  names(qq) <- gsub("PHMAN", "PHMA N", names(qq))

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
  layout(c(rep(1, 16), rep(2, 10)))
  par(mar = c(2, 4.75, .5, .5), cex = 0.8, oma = c(0, .5, 1.5, .5))
  
  graphics::image(x = qq$year, y = seq(1, ncol(qq) - 1), z = qq_combined[, -1], axes = FALSE,
    xlab = "", ylab = "", col = c(grid_col, "grey25"), breaks = c(0, 1, 2))
  axis(1, at = labs, labels = labs, tick = 0, line = -0.65, col.axis = axis_col)
  axis(2, at = seq(1, ncol(qq) - 1), labels = names(qq)[-1], las = 1, tick = 0, 
    line = -0.4, col.axis = axis_col)
  abline(v = c(years, max(years)+1)-.5, col = grid_col, lwd = grid_lwd)
  abline(h = 1:ncol(qq)-.5, col = grid_col, lwd = grid_lwd)
  box(col = box_col, lwd = grid_lwd)
  
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  mtext(text = firstup(gsub("/", "-", gsub(" ", " ", common_name))), 
    side = 3, adj = 1, line = 0.4,
    col = axis_col)

  graphics::image(x = spb$year, y = 1:4, z = as.matrix(log(spb[, -1])), axes = FALSE,
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
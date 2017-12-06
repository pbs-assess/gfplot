library(tidyverse)

dbio <- readRDS("../../Dropbox/dfo/data/all-survey-bio.rds")
names(dbio) <- tolower(names(dbio))
dbio$species_common_name <- tolower(dbio$species_common_name)
dbio$species_science_name <- tolower(dbio$species_science_name)
dbio <- mutate(dbio, year = lubridate::year(trip_start_date))

ss <- readRDS("../../Dropbox/dfo/data/survey_series.rds") %>%
  select(-SURVEY_SERIES_TYPE_CODE)
names(ss) <- tolower(names(ss))
dbio <- inner_join(dbio, ss)

dbio <- filter(dbio, survey_series_desc %in% c(
  "West Coast Haida Gwaii Synoptic Survey",
  "Queen Charlotte Sound Synoptic Survey",
  "Hecate Strait Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"
))

dbio <- dbio[!duplicated(dbio$specimen_id), ]

dup <- group_by(dbio, species_common_name) %>% 
  summarise(n_spp = length(unique(species_science_name))) %>% 
  arrange(-n_spp) %>% 
  filter(n_spp > 1)
stopifnot(nrow(dup) == 0)

dbio <- dbio %>% 
  select(species_common_name, species_science_name, year, age, length, weight, 
    maturity_code, sex, survey_series_desc)

# bad data:

dbio <- dbio[-which(dbio$length > 600 & dbio$species_common_name == "north pacific spiny dogfish"), ]
dbio <- dbio[-which(dbio$length > 600 & dbio$species_common_name == "big skate"), ]

dbio <- dbio[-which(dbio$length > 600 & dbio$species_common_name == "longnose skate"), ]

dbio <- dbio[-which(dbio$length > 60 & dbio$species_common_name == "pacific tomcod"), ]
dbio <- dbio[-which(dbio$length > 50 & dbio$species_common_name == "quillback-rockfish"), ]


#' @param xfrac The fraction over from the left side.
#' @param yfrac The fraction down from the top.
#' @param label The text to label with.
#' @param pos Position to pass to text()
#' @param ... Anything extra to pass to text(), e.g. cex, col.
add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}

surv <- data.frame(survey_series_desc = c(
  "West Coast Haida Gwaii Synoptic Survey",
  "Hecate Strait Synoptic Survey",
  "Queen Charlotte Sound Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"
),
  surv_short = c("WCHG", "HS", "QCS", "WCVI"), stringsAsFactors = FALSE)

dbio$surv_short <- NULL
dbio <- inner_join(dbio, surv)

survs <- c("West Coast Haida Gwaii Synoptic Survey", "Hecate Strait Synoptic Survey", 
  "Queen Charlotte Sound Synoptic Survey", "West Coast Vancouver Island Synoptic Survey")

#############

# >= 5 fish at >= 5 ages for >= 2 years ?

dbio_ages <- filter(dbio, !is.na(age), sex == 2) %>% 
  group_by(species_common_name, survey_series_desc) %>% 
  mutate(n_ages = n()) %>% 
  ungroup() %>% 
  filter(n_ages >= 20)

source("R/make-spp-list.R")
spp <- get_spp_names()$species_common_name
# spp <- spp[spp %in% dbio_ages$species_common_name]

dir.create("bubbles", showWarnings = FALSE)

cols <- c(
  RColorBrewer::brewer.pal(9, "Blues")[2],
  RColorBrewer::brewer.pal(9, "Greens")[2],
  RColorBrewer::brewer.pal(9, "Reds")[2],
  RColorBrewer::brewer.pal(9, "Purples")[2]
)

cols_dark <- c(
  RColorBrewer::brewer.pal(9, "Blues")[9],
  RColorBrewer::brewer.pal(9, "Greens")[9],
  RColorBrewer::brewer.pal(9, "Reds")[9],
  RColorBrewer::brewer.pal(9, "Purples")[9]
)


for (ii in 1:length(spp)) {
  
  if (spp[[ii]] %in% dbio_ages$species_common_name) {
    d <- filter(dbio_ages, species_common_name == spp[[ii]],
      !is.na(age))
    ds <- group_by(d, year, age, survey_series_desc, surv_short) %>% 
      summarise(n = n())
  }
  
  pdf(paste0("bubbles/", gsub("/", "-", gsub(" ", "-", spp[[ii]])), ".pdf"), 
    width = 3.5, height = 4)
  par(mfrow = c(4, 1), cex = 0.7, mgp = c(2, 0.4, 0), 
    mar = c(0, 0, 0, 0), oma = c(3, 3, .5, 1.5), tcl = -0.2)
  
  xrange <- c(0, 1)
  for (i in 1:length(survs)) {
    if (spp[[ii]] %in% dbio_ages$species_common_name) {
      x <- filter(ds, survey_series_desc == survs[i])
      xrange <- range(ds$age) + c(-0.5, 0.5)
    }
    plot(1, 1, xlim = xrange, ylim = c(2003, 2016) + c(-0.2, 0.2), 
      type = "n", ann = FALSE, axes = FALSE,
      xaxs = "i")
    abline(h = seq(2000, 2016, 2), col = "grey92", lwd = 0.8)
    if (spp[[ii]] %in% dbio_ages$species_common_name) {
      if (max(ds$age) >= 80) vertical_line_increment <- 20
      if (max(ds$age) >= 40 & max(ds$age) < 80) vertical_line_increment <- 10
      if (max(ds$age) >= 20 & max(ds$age) < 40) vertical_line_increment <- 5
      if (max(ds$age) < 20) vertical_line_increment <- 2
      abline(v = seq(0, max(d$age), vertical_line_increment), col = "grey92", lwd = 0.8)
      if (nrow(x) > 0) {
        # smaller (final) scale values = bigger bubbles:
        radius_scale <- max(sqrt(ds$n/3.141592)) / diff(range(ds$age)) * 50
        symbols(x$age, x$year, circles = sqrt(x$n/3.14159265)/ radius_scale, 
          inches = FALSE,
          pch = 21, 
          fg = paste0(cols_dark[i], "90"),
          bg = paste0(cols[i], "50"),
          add = TRUE, lwd = 0.8)
      }
    }
    # add_label(label = unique(d$survey_series_desc)[i], xfrac = 0.01, yfrac = 0.1, col = "grey30")
    mtext(surv$surv_short[surv$survey_series_desc == survs[i]], side = 4, line = 0.2, 
      col = "grey30", cex = 0.6)
    axis(2, las = 1, at = seq(1960, 2016, 4), 
      col = "grey60", col.axis = "grey40", 
      col.ticks = "grey70")
    box(col = "grey60")
  }
  if (spp[[ii]] %in% dbio_ages$species_common_name) {
    axis(1, col = "grey60", col.axis = "grey40", padj = -0.2, col.ticks = "grey70")
  }
  mtext("Age (years)", side = 1, line = 1.5, col = "grey40",
    cex = 0.7)
  dev.off()
}
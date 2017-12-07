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

# spp <- names(rev(sort(table(dbio$species_common_name)))[1:50])
source("R/make-spp-list.R")
spp <- get_spp_names()$species_common_name

spp %in% dbio$species_common_name
 
# spp <- "lingcod"

dbio <- mutate(dbio, sex = ifelse(sex == 1, "a_male", "b_female"))
dbio$sex <- as.factor(dbio$sex)
for (ii in seq_along(spp)) {
  

  message(spp[ii])
  dd <- filter(dbio, species_common_name == spp[[ii]])
  dd2 <- filter(dd, survey_series_desc %in% c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey"
  ))
  
  surv <- data.frame(survey_series_desc = c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey"
  ),
    surv_short = c("WCHG", "HS", "QCS", "WCVI"), stringsAsFactors = FALSE)
  
  dd2 <- suppressWarnings(inner_join(surv, dd2, by = "survey_series_desc"))
  
  scaling <- 1.8
  
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
  
  cols_dark <- paste0(cols_dark, "98")
  cols <- paste0(cols, "90")
  
  if (nrow(dd2) == 0) {
    xlim <- c(0, 1)
  } else {
    xlim <- range(dd2$length, na.rm = TRUE)
  }
  
  survs <- c("West Coast Haida Gwaii Synoptic Survey", "Hecate Strait Synoptic Survey", 
    "Queen Charlotte Sound Synoptic Survey", "West Coast Vancouver Island Synoptic Survey")
  pdf(paste0("joy/", gsub("/", "-", gsub(" ", "-", spp[[ii]])), "-joy.pdf"), width = 4, height = 4.5)
  par(mfrow = c(1, 4), oma = c(3, 4, 2, .5), mar = c(0,0,0,0), cex = 0.8,
    mgp = c(2, 0.35, 0), tcl = -0.2)
  for (j in seq_along(survs)) {
    dd3 <- filter(dd2, survey_series_desc == survs[j],
      !is.na(length))
    plot(1, 1, xlim = xlim, ylim = c(2003, 2018) + c(-0.025, 0.65), 
      ylab = "Year", xlab = "Length (cm)", axes = FALSE, yaxs = "i")
    mtext(unique(surv$surv_short)[j], side = 3, line = 0.2, cex = 0.7)
    abline(h = seq(2003, 2017, 1), col = "#00000030")
    
    if (j == 1)
      axis(2, at = seq(1941, 2017, 2) + 0, labels = seq(1941, 2017, 2), 
        las = 1, cex.axis = 0.8, col = "grey40", col.axis = "grey30")
    
    if (nrow(dd2) > 0)
     axis(1, las = 1, cex.axis = 0.8, col = "grey40", col.axis = "grey30", padj = -0.3)
    
    max_y <- list()
    den <- list()
    
    dd3 <- group_by(dd3, survey_series_desc, year, sex) %>% mutate(samples = n()) %>% 
      filter(samples >= 20) %>% 
      ungroup()
    
    if (nrow(dd3) > 0) {
      g <- ggplot(dd3, aes(length, colour = sex)) + 
        geom_density() + facet_wrap(~year) +
        scale_color_manual(values = c("a_male" = "white", "b_female" = "black"))
      
      gd <- ggplot_build(g) # extract data from ggplot
      gd1 <- gd$data[[1]]
      gd1$year <- unique(dd3$year)[gd1$PANEL]
      assertthat::assert_that(all(gd1$colour %in% c("white", "black")))
      gd1$sex <- ifelse(gd1$colour == "white", 1, 2)
      
      max_y <- max(gd1$density)
      this_scaling <- scaling / max_y
      
      for (i in seq_along(rev(unique(dd3$year)))) {
        
        for (k in 1:2) {
          x <- dplyr::filter(gd1, year == rev(unique(dd3$year))[i], sex == k)
          polygon(c(x$x, rev(x$x)),
            c(x$density * this_scaling + x$year[1], rep(x$year[1], length(x$y))),
            border = c("grey30", cols_dark[j])[k], col = c("grey70", cols[j])[k], lwd = 1.1)
        }
      }
    }
    box(col = "grey60")
  }
  mtext("Length (cm)", side = 1, outer = TRUE, col = "grey30", line = 1.5, cex = 0.7)
  dev.off()
  
}

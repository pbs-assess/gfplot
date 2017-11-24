library(tidyverse)

## VB:

dbio <- readRDS("../../Dropbox/dfo/data/all-survey-bio.rds")
names(dbio) <- tolower(names(dbio))
dbio$species_common_name <- tolower(dbio$species_common_name)
dbio$species_science_name <- tolower(dbio$species_science_name)
dbio <- mutate(dbio, year = lubridate::year(trip_start_date))

ss <- readRDS("../../Dropbox/dfo/data/survey_series.rds") %>%
  select(-SURVEY_SERIES_TYPE_CODE)
names(ss) <- tolower(names(ss))
dbio <- left_join(dbio, ss)
dbio <- dbio %>% 
  select(species_common_name, species_science_name, year, age, length, weight, 
    maturity_code, sex, survey_series_desc)

# dd <- filter(dbio, species_common_name == "arrowtooth flounder")
# ggplot(dd, aes(age, length)) + geom_point(aes(colour = as.factor(sex)))
# 
# dd_f <- filter(dd, !is.na(length), !is.na(age), !is.na(sex), sex == 2)
# # install("../pbs-mptools/")
# library(PBSmptools)
# mf <- fit_vb(age = dd_f$age, length = dd_f$length, iter = 1000, chains = 2, cores = 2)
# mf
# s <- sample_posterior_vb(mf, n = 400)
# ages <- seq(min(dd_f$age), max(dd_f$age), length.out = 200)
# p <- matrix(nrow = length(ages), ncol = nrow(s))
# for (i in 1:nrow(p)) {
#   for (j in seq_len(ncol(p))) {
#     p[i, j] <- s$linf[j] * 
#       (1 - exp(-s$k[j] * (ages[i] - s$t0[j])))
#   }
# }
# plot(dd_f$age, dd_f$length, col = "#00000040", pch = 20)
# l <- apply(p, 1, quantile, probs = 0.025)
# u <- apply(p, 1, quantile, probs = 0.975)
# polygon(c(ages, rev(ages)), c(l, rev(u)), col = "grey50", border = NA)
# lines(ages, apply(p, 1, median), col = "red", lwd = 2.5)


# dd2 <- filter(dd, survey_series_desc %in% c("Hecate Strait Synoptic Survey", 
#   "West Coast Haida Gwaii Synoptic Survey",
#   "Queen Charlotte Sound Synoptic Survey",
#   "West Coast Vancouver Island Synoptic Survey"
# ))
# ggplot(dd2, aes(as.factor(year), length)) + geom_boxplot()
# 
# ggplot(dd2, aes(length)) + geom_density() +
#   facet_grid(as.factor(year)~survey_series_desc)

dd <- filter(dbio, species_common_name == "walleye pollock")
dd2 <- filter(dd, survey_series_desc %in% c("Hecate Strait Synoptic Survey",
  "West Coast Haida Gwaii Synoptic Survey",
  "Queen Charlotte Sound Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"
))

surv <- data.frame(survey_series_desc = c("Hecate Strait Synoptic Survey",
  "West Coast Haida Gwaii Synoptic Survey",
  "Queen Charlotte Sound Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"
),
  surv_short = c("HS", "WCHG", "QCS", "WCVI"), stringsAsFactors = FALSE)

dd2 <- suppressWarnings(inner_join(surv, dd2, by = "survey_series_desc"))

scaling <- 2.5


cols <- c(
  RColorBrewer::brewer.pal(9, "Greens")[2],
  RColorBrewer::brewer.pal(9, "Blues")[2],
  RColorBrewer::brewer.pal(9, "Reds")[2],
  RColorBrewer::brewer.pal(9, "Purples")[2]
)

cols_dark <- c(
  RColorBrewer::brewer.pal(9, "Greens")[9],
  RColorBrewer::brewer.pal(9, "Blues")[9],
  RColorBrewer::brewer.pal(9, "Reds")[9],
  RColorBrewer::brewer.pal(9, "Purples")[9]
)

cols_dark <- paste0(cols_dark, "98")
cols <- paste0(cols, "90")

pdf("joy-example-walleye.pdf", width = 4, height = 4.5)
par(mfrow = c(1, 4), oma = c(3, 4, 2, .5), mar = c(0,0,0,0), cex = 0.8,
  mgp = c(2, 0.35, 0), tcl = -0.2)
for (j in seq_along(unique(dd2$survey_series_desc))) {
  dd3 <- filter(dd2, survey_series_desc == unique(dd2$survey_series_desc)[j],
    !is.na(length))
  plot(1, 1, xlim = range(dd2$length, na.rm = TRUE), ylim = range(dd2$year) + c(0.1, 1.5), 
    ylab = "Year", xlab = "Length (cm)", axes = FALSE)
  mtext(unique(dd3$surv_short), side = 3, line = 0.2, cex = 0.7)
  abline(h = seq(min(dd2$year), max(dd2$year), 1), col = "#00000030")
  
  if (j == 1)
    axis(2, at = seq(1941, 2017, 2) + 0, labels = seq(1941, 2017, 2), 
      las = 1, cex.axis = 0.8, col = "grey40", col.axis = "grey30")
  axis(1, las = 1, cex.axis = 0.8, col = "grey40", col.axis = "grey30", padj = -0.3)
  
  max_y <- list()
  den <- list()
  for (k in 1:2) {
    den[[k]] <- list()
    for (i in seq_along(rev(unique(dd3$year)))) {
      x <- dplyr::filter(dd3, year == rev(unique(dd3$year))[i], sex == k)
      den[[k]][[i]] <- density(x$length)
    }
    max_y[[k]] <- max(unlist(lapply(den[[k]], function(qq) max(qq$y))))
  }
  max_y <- max(unlist(max_y))
  this_scaling <- scaling / max_y
  
  for (i in seq_along(rev(unique(dd3$year)))) {
    
    for (k in 1:2) {
      x <- dplyr::filter(dd3, year == rev(unique(dd3$year))[i])
      polygon(c(den[[k]][[i]]$x, rev(den[[k]][[i]]$x)), 
        c(den[[k]][[i]]$y * this_scaling + x$year[1], rep(x$year[1], length(den[[k]][[i]]$y))),
        border = c("grey30", cols_dark[j])[k], col = c("grey70", cols[j])[k], lwd = 1.1)
    }
  }
  
  box(col = "grey60")
  mtext("Length (cm)", side = 1, outer = TRUE, col = "grey30", line = 1.5, cex = 0.7)
  
}
dev.off()
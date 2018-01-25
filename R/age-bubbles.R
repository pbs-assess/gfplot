library(tidyverse)

dbio <- readRDS("data-cache/all-survey-bio.rds")

dbio <- filter(dbio, survey_series_desc %in% c(
  "West Coast Haida Gwaii Synoptic Survey",
  "Queen Charlotte Sound Synoptic Survey",
  "Hecate Strait Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"
))
dbio <- dbio[!duplicated(dbio$specimen_id), ] # critical!!

dup <- group_by(dbio, species_common_name) %>%
  summarise(n_spp = length(unique(species_science_name))) %>%
  arrange(-n_spp) %>%
  filter(n_spp > 1)
stopifnot(nrow(dup) == 0)

dbio <- dbio %>%
  select(species_common_name, species_science_name, year, age,
    length, weight, maturity_code, sex, survey_series_desc)

# bad data:
dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "north pacific spiny dogfish"), ]
dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "big skate"), ]
dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "longnose skate"), ]
dbio <- dbio[!(dbio$length > 60 & dbio$species_common_name == "pacific tomcod"), ]
dbio <- dbio[!(dbio$length > 50 & dbio$species_common_name == "quillback-rockfish"), ]

source("R/add-label.R")

surv <- data.frame(survey_series_desc = c(
  "West Coast Haida Gwaii Synoptic Survey",
  "Hecate Strait Synoptic Survey",
  "Queen Charlotte Sound Synoptic Survey",
  "West Coast Vancouver Island Synoptic Survey"),
  surv_short = c("WCHG", "HS", "QCS", "WCVI"),
  stringsAsFactors = FALSE)
dbio$surv_short <- NULL # extra careful
dbio <- inner_join(dbio, surv)
survs <- surv$survey_series_desc

dbio_ages <- filter(dbio, !is.na(age)) %>%
  group_by(species_common_name, survey_series_desc) %>%
  mutate(n_ages = n()) %>%
  ungroup() %>%
  filter(n_ages >= 20)

source("R/make-spp-list.R")
spp <- get_spp_names()$species_common_name

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

# cols <- c("QCS" = cols_dark[3], "WCVI" = cols_dark[4], "HS" = cols_dark[2])


age_bins <- seq(0, 200, 2)

dir.create("bubbles-gg", showWarnings = FALSE)
for (ii in seq_along(spp[1:8])) {
  # ii <- 10
  spp[[ii]]

  # if (spp[[ii]] %in% dbio_ages$species_common_name) {
  d <- filter(dbio_ages, species_common_name == spp[[ii]],
    !is.na(age))
  ds <- d %>%
    mutate(age = age_bins[findInterval(age, age_bins)]) %>%
    group_by(year, age, survey_series_desc, sex, surv_short) %>%
    summarise(n = n()) %>%
    group_by(year, surv_short) %>%
    mutate(n_scaled = n / max(n)) %>%
    ungroup() %>%
    mutate(year = ifelse(sex == 1, year + 0.11, year - 0.11),
      sex = ifelse(sex == 2, "F", "M"))
  # }

  all_surveys <- tibble(
    surv_short = c("QCS", "HS", "WCVI", "WCHG")
  )

  complete_df <- expand.grid(
    surv_short = c("QCS", "HS", "WCVI", "WCHG"),
    age = 1,
    year = seq(min(dbio_ages$year), max(dbio_ages$year)),
    sex = NA,
    n_scaled = 1, stringsAsFactors = FALSE
  )
  if (nrow(ds) == 0)
    ds <- complete_df

  ds <- full_join(ds, all_surveys, by = "surv_short")

  ggplot(ds, aes(year, age)) +
    geom_vline(xintercept = seq(min(dbio_ages$year, na.rm = TRUE),
      max(dbio_ages$year, na.rm = TRUE), 1), col = "grey92", lwd = 0.4) +
    geom_hline(yintercept = seq(0, max(ds$age, na.rm = TRUE), 10), col = "grey92",
      lwd = 0.4) +
    geom_point(aes(size = n_scaled, group = sex, colour = sex),
      pch = 21, alpha = 0.9) +
    # geom_point(aes(size = n_scaled, group = sex, fill = sex, colour = sex),
      # pch = 21, alpha = 0.05) +
    facet_wrap(~surv_short, nrow = 1) +
    ggsidekick::theme_sleek() +
    scale_fill_manual(values = c("M" = "grey50", "F" = "#f44256")) +
    scale_colour_manual(values = c("M" = "grey50", "F" = "#f44256")) +
    scale_x_continuous(breaks = seq(2004, 2016, 4)) +
    xlab("") + ylab("Age (years)") +
    scale_size_area(max_size = 5) +
    coord_cartesian(xlim = range(dbio_ages$year, na.rm = TRUE) + c(-0.5, 0.5),
      ylim = c(0, max(ds$age, na.rm = TRUE) + 1),
      expand = FALSE) +
    guides(colour = FALSE, size = FALSE, fill = FALSE)
    # theme(panel.spacing = unit(0, "lines"))
  ggsave(paste0("bubbles-gg/", gsub("/", "-", gsub(" ", "-", spp[[ii]])), "-2.pdf"),
    width = 10, height = 6.0)
}


# pdf(paste0("bubbles/", gsub("/", "-", gsub(" ", "-", spp[[ii]])), ".pdf"),
    # width = 3.5, height = 4)
  par(mfrow = c(4, 1), cex = 0.7, mgp = c(2, 0.4, 0),
    mar = c(0, 0, 0, 0), oma = c(3, 3, .5, 1.5), tcl = -0.2)

  xrange <- c(0, 1)
  for (i in 1:length(survs)) {
    if (spp[[ii]] %in% dbio_ages$species_common_name) {
      x <- filter(ds, survey_series_desc == survs[i])
      yrange <- range(ds$age) + c(-0.5, 0.5)
    }
    plot(1, 1, ylim = yrange, xlim = c(2003, 2016) + c(-0.2, 0.2),
      type = "n", ann = FALSE, axes = FALSE,
      xaxs = "i")
    abline(v = seq(2000, 2016, 2), col = "grey92", lwd = 0.8)
    if (spp[[ii]] %in% dbio_ages$species_common_name) {
      if (max(ds$age) >= 80) horiz_line_increment <- 20
      if (max(ds$age) >= 40 & max(ds$age) < 80) horiz_line_increment <- 10
      if (max(ds$age) >= 20 & max(ds$age) < 40) horiz_line_increment <- 5
      if (max(ds$age) < 20) horiz_line_increment <- 2
      abline(v = seq(0, max(d$age), horiz_line_increment), col = "grey92", lwd = 0.8)
      if (nrow(x) > 0) {
        # smaller (final) scale values = bigger bubbles:
        # radius_scale <- max(sqrt(ds$n/3.141592)) / diff(range(ds$age)) * 50
        radius_scale <- 0.5
        x <- group_by(x, year) %>% mutate(n_scaled = n / max(n))
        symbols(x$year, x$age, circles = sqrt(x$n_scaled/3.14159265)/ radius_scale,
          inches = FALSE,
          pch = 21,
          fg = paste0(cols_dark[i], "90"),
          bg = paste0(cols[i], "50"),
          add = TRUE, lwd = 0.8)
      }
    }
    # add_label(label = unique(d$survey_series_desc)[i], xfrac = 0.01, yfrac = 0.1, col = "grey30")
    mtext(surv$surv_short[surv$survey_series_desc == survs[i]], side = 3, line = 0.2,
      col = "grey30", cex = 0.6)
    axis(1, las = 1, at = seq(1960, 2016, 4),
      col = "grey60", col.axis = "grey40",
      col.ticks = "grey70")
    box(col = "grey60")
  }
  if (spp[[ii]] %in% dbio_ages$species_common_name) {
    axis(2, col = "grey60", col.axis = "grey40", padj = -0.2, col.ticks = "grey70")
  }
  mtext("Age (years)", side = 2, line = 1.5, col = "grey40",
    cex = 0.7, outer = TRUE)
  # dev.off()
# }

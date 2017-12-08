library(tidyverse)

dbio <- readRDS("data-cache/all-survey-bio.rds")
dbio <- dbio[!duplicated(dbio$specimen_id), ] # critical!!

dbio <- dbio %>%
  select(species_common_name, species_science_name, year,
    age, length, weight, maturity_code)

# commercial bio samples:
dbio_c <- readRDS("data-cache/all-commercial-bio.rds")
assertthat::assert_that(sum(duplicated(dbio_c$specimen_id)) == 0)
dbio_c <- select(dbio_c, species_common_name, species_science_name,
  year, age, length, weight, maturity_code)

dbio_c$type <- "commercial"
dbio$type <- "survey"
dbio_combined <- dplyr::bind_rows(dbio, dbio_c)

bio <- group_by(dbio_combined, type, species_common_name,
  species_science_name, year) %>%
  summarise(
    n_age = sum(!is.na(age)),
    n_length = sum(!is.na(length) & length > 0),
    n_weight = sum(!is.na(weight) & weight > 0),
    n_maturity = sum(!is.na(maturity_code) & maturity_code > 0)
  ) %>% ungroup

# ---
years <- seq(1996, 2016)
labs <- intersect(seq(1800, 3000, 2), years)
col_com <- colorRampPalette(RColorBrewer::brewer.pal(9, "Greys")[-9])(200)
col_surv <- colorRampPalette(RColorBrewer::brewer.pal(9, "Greys")[-9])(200)
grid_col <- "grey75"
box_col <- "grey55"
grid_lwd <- 1.3
surv_col <- "grey33"
axis_col <- "grey30"

source("R/make-spp-list.R")
torun <- get_spp_names()$species_common_name

for (i in seq_along(torun)) {

  common_name <- torun[i]
  message(common_name)

  spb <- filter(bio, species_common_name %in% common_name) %>%
    select(-species_common_name, -species_science_name)

  spb <- rename(spb,
    `# Maturity` = n_maturity,
    `# Weight` = n_weight,
    `# Length` = n_length,
    `# Age` = n_age
  )

  # fill missing:
  all <- expand.grid(year = 1996:2017, type = c("commercial", "survey"), stringsAsFactors = FALSE)
  spb <- left_join(all, spb, by = c("year", "type"))

  pdf(paste0("synop/dat-syn-", gsub("/", "-", gsub(" ", "-", common_name)), ".pdf"),
    width = 6, height = 2.75)
  layout(c(rep(1, 16), rep(2, 16)))
  par(mar = c(2, 4.75, 1.5, .5), cex = 0.8, oma = c(0, .5, 0.25, .5))

  plot_grid <- function(mat_dat, col) {

    if (sum(mat_dat) == 0) col <- "white"

    graphics::image(x = unique(spb$year), y = 1:4, z = log(mat_dat+1), axes = FALSE,
      xlab = "", ylab = "", col = col)
    axis(1, at = labs, labels = labs, tick = 0, line = -0.65, col.axis = axis_col)
    axis(2, at = 1:4, labels = names(spb)[-c(1, 2)], las = 1, tick = 0, line = -0.4,
      col.axis = axis_col)
    abline(v = c(years, max(years)+1)-.5, col = grid_col, lwd = grid_lwd)
    abline(h = 1:5-.5, col = grid_col, lwd = grid_lwd)
    box(col = box_col, lwd = grid_lwd)
    max_val <- ifelse(all(is.na(mat_dat)), NA, max(mat_dat, na.rm = TRUE))

    if (!is.na(max_val) & max_val != 0) {
      round_nice <- function(x) {
        out <- plyr::round_any(max_val, 100)
        if (out == 0) out <- x
        out
      }
      max_ind <- which(mat_dat == max_val, arr.ind = TRUE)
      max_ind <- max_ind[1,,drop = FALSE]
      text(spb$year[max_ind[,"row"][[1]]], max_ind[,"col"][[1]],
        round_nice(max_val), col = "white", cex = 0.6)
    }
  }

  for (i in 3:6) {
    temp <- spb[,i]
    temp[is.na(temp)] <- 0
    spb[,i] <- temp
  }

  com <- filter(spb, type == "commercial")
  plot_grid(as.matrix((com[, -c(1:2)])), col_com)

  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  mtext(text = "Commercial samples",
    side = 3, adj = 0, line = 0.4,
    col = axis_col, cex = 0.8)

  com <- filter(spb, type == "survey")
  plot_grid(as.matrix((com[, -c(1:2)])), col_surv)

  mtext(text = "Survey samples",
    side = 3, adj = 0, line = 0.4,
    col = axis_col, cex = 0.8)

  dev.off()
}


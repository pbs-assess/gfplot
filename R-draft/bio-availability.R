summarize_bio_avail <- function(dbio, dbio_c) {
  library(tidyverse)
  
  dbio <- dbio[!duplicated(dbio$specimen_id), ] # critical!!
  
  dbio <- dbio %>%
    select(species_common_name, species_science_name, year,
      age, length, weight, maturity_code)
  
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
  
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

round_nice <- function(x) {
  out <- plyr::round_any(x, 100)
  if (out == 0) out <- x
  out
}

plot_grid <- function(mat_dat, col, labels, axis_col, label_max_only = FALSE,
  grid_lwd = 1.3, box_col = "grey55", years, grid_col = "grey75") {
  if (sum(mat_dat) == 0)
    col <- "white"
  
  labs <- intersect(seq(1800, 3000, 2), years)
  graphics::image(x = years, y = seq_len(4), z = log(mat_dat+1), axes = FALSE,
    xlab = "", ylab = "", col = col)
  axis(1, at = labs, labels = labs, tick = 0, line = -0.65, col.axis = axis_col)
  axis(2, at = seq_len(4), labels = labels, las = 1, tick = 0, line = -0.4,
    col.axis = axis_col)
  abline(v = c(years, max(years)+1)-.5, col = grid_col, lwd = grid_lwd)
  abline(h = seq_len(5)-.5, col = grid_col, lwd = grid_lwd)
  box(col = box_col, lwd = grid_lwd)
  
  if (label_max_only) {
    max_val <- ifelse(all(is.na(mat_dat)), NA, max(mat_dat, na.rm = TRUE))
    if (!is.na(max_val) & max_val != 0) {
      max_ind <- which(mat_dat == max_val, arr.ind = TRUE)
      max_ind <- max_ind[1,,drop = FALSE]
      text(years[max_ind[,"row"][[1]]], max_ind[,"col"][[1]],
        round_nice(max_val), col = "white", cex = 0.6)
    }
  } else {
    for (i in seq_len(nrow(mat_dat))) {
      for (j in seq_len(ncol(mat_dat))) {
        text(years[i], j,
          round_nice(mat_dat[i, j]), col = "#FFFFFF95", cex = 0.6)
      }
    }
  }
}

plot_bio_avail <- function(common_name, bio, label_max_only = FALSE,
  years = seq(1996, 2016),
  col_com = colorRampPalette(RColorBrewer::brewer.pal(9, "Greys")[-9])(100),
  col_surv = colorRampPalette(RColorBrewer::brewer.pal(9, "Greys")[-9])(100),
  axis_col = "grey30") {
  
  library(tidyverse)
  
  spb <- filter(bio, species_common_name %in% common_name, year %in% years) %>%
    select(-species_common_name, -species_science_name)
  
  spb <- rename(spb,
    `# Maturity` = n_maturity,
    `# Weight` = n_weight,
    `# Length` = n_length,
    `# Age` = n_age
  )
  
  # fill missing:
  all <- expand.grid(year = years, type = c("commercial", "survey"), 
    stringsAsFactors = FALSE)
  spb <- left_join(all, spb, by = c("year", "type"))
  
  for (i in 3:6) {
    temp <- spb[,i]
    temp[is.na(temp)] <- 0
    spb[,i] <- temp
  }
  
  layout(c(rep(1, 16), rep(2, 16)))
  par(cex = 0.8)
  par(mar = c(2, 4.75, 1.5, .5), oma = c(0, .5, 0.25, .5))
  
  com <- filter(spb, type == "commercial")
  plot_grid(as.matrix((com[, -c(1:2)])), col_com, labels = names(spb)[-c(1, 2)],
    label_max_only = label_max_only, axis_col = axis_col, years = years)
  mtext(text = "Commercial samples",
    side = 3, adj = 0, line = 0.4, col = axis_col, cex = 0.8)
  
  com <- filter(spb, type == "survey")
  plot_grid(as.matrix((com[, -c(1:2)])), col_surv, labels = names(spb)[-c(1, 2)],
    label_max_only = label_max_only, axis_col = axis_col, years = years)
  mtext(text = "Survey samples",
    side = 3, adj = 0, line = 0.4, col = axis_col, cex = 0.8)
  
}
plot_bio_avail(torun[1], bio)

plot_spatial_cpue <- function(dat, species, bin_width = 6, n_minimum_vessels = 3L,
  pal_function = viridisLite::viridis) {
  # hexagon bin size in UTM kms
  
  library(tidyverse)
  library(PBSmapping)
  library(PBSdata)
  data("isobath")
  
  dcpue <- dat
  names(dcpue) <- tolower(names(dcpue))
  dat <- filter(dcpue, year %in% c(2013:4000), 
    species_common_name %in% toupper(species)) %>% 
    rename(X = lon, Y = lat)
  
  # UTMs:
  attr(dat, "projection") <- "LL"
  attr(dat, "zone") <- 9
  xlim_ll <- range(dat$X) + c(-2, 2)
  ylim_ll <- range(dat$Y) + c(-2, 2)
  dat <- suppressMessages(PBSmapping::convUL(dat))
  
  # use ggplot to compute hexagons:
  g <- ggplot(dat, aes(X, Y)) +
    coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
    stat_summary_hex(aes(x = X, y = Y, z = log(cpue)), data = dat,
      binwidth = bin_width, fun = function(x) mean(x, na.rm = TRUE))
  
  g_count <- ggplot(dat, aes(X, Y)) +
    coord_equal(xlim = range(dat$X), ylim = range(dat$Y)) +
    stat_summary_hex(aes(x = X, y = Y, z = vessel_registration_number), data = dat,
      binwidth = bin_width, fun = function(x) length(unique(x)))
  
  gd <- ggplot_build(g) # extract data from ggplot
  gd1 <- gd$data[[1]] # hexagons
  
  gd_count <- ggplot_build(g_count) # to count observations per cell
  assertthat::are_equal(nrow(gd$data[[1]]), nrow(gd_count$data[[1]]))
  gd1 <- gd1[gd_count$data[[1]]$value >= n_minimum_vessels, ] # privacy
  
  n_col_bins <- 200L
  value_range <- range(gd1$value)
  vals <- seq((value_range[1]), (value_range[2]), length.out = n_col_bins)
  pal <- pal_function(n_col_bins)
  # pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBu"))(n_col_bins)
  gd1$custom_fill <- pal[findInterval((gd1$value), vals)]
  
  # legend data for later:
  leg <- data.frame(vals = vals, col = pal,
    stringsAsFactors = FALSE)
  leg$raw_vals <- exp(leg$vals)
  leg$i <- seq(0, 1, length.out = nrow(leg))
  
  # pdf("pop-index-example.pdf", width = 3, height = 2.8)
  par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
  
  xlim = range(dat$X) + c(-10, -5)
  ylim = range(dat$Y) + c(-10, 10)
  
  data("nepacLLhigh")
  np <- clipPolys(nepacLLhigh, xlim = xlim_ll, ylim = ylim_ll)
  attr(np, "zone") <- 9
  nepacUTM <- suppressMessages(convUL(np))
  
  plotMap(nepacUTM, xlim = xlim, ylim = ylim, axes = FALSE, type = "n",
    plt = c(0, 1, 0, 1), xlab = "", ylab = "")
  
  rect(xleft = -160, xright = -110, ybottom = 40, ytop = 60,
    col = "grey90")
  
  # hexagons:
  gd1$custom_fill <- paste0(substr(gd1$custom_fill, 1, 7), "FF") # add transparency?
  dx <- ggplot2::resolution(gd1$x, FALSE)
  dy <- resolution(gd1$y, FALSE) / 2 * 1.15
  plyr::a_ply(gd1, 1, function(i)
    hexagon(i$x, i$y, dx, dy, col = i$custom_fill, border = i$custom_fill, 
      lwd = 0.02))
  
  zlev <- c(100, 200, 500)
  isobath_UTM <- suppressMessages(convUL(clipPolys(filter(isobath, PID %in% zlev), 
    xlim = xlim_ll, ylim = ylim_ll)))
  PBSmapping::addLines(isobath_UTM, col = rev(c("#00000060", "#00000045", "#00000030")), lwd = 0.6)
  
  plyr::d_ply(nepacUTM, "PID", function(i)
    polygon(i$X, i$Y, col = "grey90", border = "grey70", lwd = 0.4))
  
  # # spark lines:
  # plyr::l_ply(pos$survey_id, function(xx) {
  #   dd <- filter(dat_indices, species == "sebastes alutus", survey_id == xx)
  #   dd <- left_join(dd, pos)
  #   dd <- arrange(dd, year) #%>% filter(biomass != 0)
  #   # dd <- left_join(expand.grid(year = min(dd$year):max(dd$year)), dd)
  #   min_year_main <- 2003
  #   # label_year <- ifelse(xx == "wcvi", TRUE, FALSE)
  #   label_year <- TRUE
  #   min_year <- ifelse(xx %in% c("phma_n", "phma_s"), 2006, min_year_main)
  #   survey_label <- ifelse(xx %in% c("phma_n", "phma_s"),
  #     toupper(gsub("_", " ", xx)), "")
  #   survey_spark(x = as.numeric(na.omit(unique(dd$lon))), y = as.numeric(na.omit(unique(dd$lat))),
  #     ts_x = dd$year, ts_y = dd$biomass, scale_x = 10, scale_y = 40,
  #     ts_y_upper = dd$lowerci, ts_y_lower = dd$upperci, label_year = label_year,
  #     min_year = min_year, survey_label = survey_label)
  # })
  
  # colour legend:
  
  legend_image <- raster::as.raster(matrix(rev(leg$col), ncol = 1)) # fixme
  leg_top <- ylim[[2]] - 100 # how far legend is from top
  leg_bottom <- leg_top - diff(range(ylim)) * 0.2
  leg_left <- xlim[[2]] - 200 # longitude of legend left point
  
  for (lab in c(1, 10, 100, 1000)) {
    rasterImage(legend_image, leg_left, leg_bottom, leg_left + 25, leg_top)
    text_y <- leg_bottom + (leg_top-leg_bottom) * (leg[leg$raw_vals >= lab, "i"][1])
    text(leg_left + 15, text_y, lab, cex = 0.8, col = "grey20", pos = 4)
    # segments(-133.7, text_y, -133.8, text_y, lwd = 0.2, col = "white") # tick marks
  }
  text(leg_left + 15, leg_top + 10, "CPUE (kg/hr)", cex = 0.8, col = "grey20", pos = 4)
  # text(-128.2, leg_top + 0.2, "British Columbia", cex = 1, col = "grey85", pos = 4)
  # rect(xleft = xlim[[1]] - 20, xright = xlim[[1]] + 20.6,
  # ybottom = ylim[[1]] - 20, ytop = ylim[[1]] + 20.5, col = NA, border = "grey60",
  # lwd = 0.7)
  # dev.off()
}

# dcpue <- readRDS("~/Dropbox/dfo/data/all-spatial-cpue.rds")
# plot_spatial_cpue(dcpue, "PACIFIC OCEAN PERCH", bin_width = 7)
# plot_spatial_cpue(dcpue, "PACIFIC COD", bin_width = 7)
# plot_spatial_cpue(dcpue, "LINGCOD", bin_width = 7)


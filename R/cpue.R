hexagon <- function (x, y, unitcell_x = 1, unitcell_y = 1, ...) {
  polygon(
    hexbin::hexcoords(unitcell_x)$x + x,
    hexbin::hexcoords(unitcell_y)$y + y, ...)
}

plot_spatial_cpue <- function(dat, species, bin_width = 6, n_minimum_vessels = 3L,
  pal_function = viridisLite::viridis, xlim_ll = c(-134.1, -123.0), 
  ylim_ll = c(48.4, 54.25)) {
  
  # hexagon bin size in UTM kms
  library(ggplot2)
  library(dplyr)
  library(PBSmapping)
  library(PBSdata)
  data("isobath")
  
  dcpue <- dat
  names(dcpue) <- tolower(names(dcpue))
  dat <- filter(dcpue, year %in% c(2013:4000), 
    species_common_name %in% toupper(species)) %>% 
    rename(X = lon, Y = lat)
  
  plot_hexagons <- TRUE
  if (nrow(dat) == 0) plot_hexagons <- FALSE
  
  if (plot_hexagons) {
    
    attr(dat, "projection") <- "LL"
    attr(dat, "zone") <- 8
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
    
    if (nrow(gd1) < 5) plot_hexagons <- FALSE
    
    if (plot_hexagons) {
      
      n_col_bins <- 200L
      value_range <- range(gd1$value)
      vals <- seq((value_range[1]), (value_range[2]), length.out = n_col_bins)
      pal <- pal_function(n_col_bins)
      gd1$custom_fill <- pal[findInterval((gd1$value), vals)]
      
      # legend data for later:
      leg <- data.frame(vals = vals, col = pal,
        stringsAsFactors = FALSE)
      leg$raw_vals <- exp(leg$vals)
      leg$i <- seq(0, 1, length.out = nrow(leg))
    }
  }
  
  lims <- data.frame(X = sort(xlim_ll), Y = sort(ylim_ll))
  attr(lims, "projection") <- "LL"
  attr(lims, "zone") <- 8
  lims <- suppressMessages(convUL(lims))
  xlim <- lims$X
  ylim <- lims$Y
  
  par(mar = c(0, 0, 0, 0), oma = c(.5, .5, .5, .5), cex = 0.6)
  
  data("nepacLLhigh")
  np <- clipPolys(nepacLLhigh, xlim = xlim_ll + c(-2, 2), ylim = ylim_ll + c(-2, 2))
  attr(np, "zone") <- 8
  nepacUTM <- suppressMessages(PBSmapping::convUL(np))
  
  plotMap(nepacUTM, xlim = xlim, ylim = ylim, axes = FALSE, type = "n",
    plt = c(0, 1, 0, 1), xlab = "", ylab = "")
  
  if (plot_hexagons) {
    gd1$custom_fill <- paste0(substr(gd1$custom_fill, 1, 7), "FF") # add transparency (FF)?
    dx <- ggplot2::resolution(gd1$x, FALSE)
    dy <- resolution(gd1$y, FALSE) / 2 * 1.15
    plyr::a_ply(gd1, 1, function(i)
      hexagon(i$x, i$y, dx, dy, col = i$custom_fill, border = i$custom_fill, 
        lwd = 0.02))
  }
  
  zlev <- c(100, 200, 500)
  isobath <- clipPolys(filter(isobath, PID %in% zlev), xlim = xlim_ll + c(-3, 3), 
    ylim = ylim_ll + c(-3, 3))
  
  isobath <- filter(isobath, PID %in% zlev)
  
  attr(isobath, "zone") <- 8
  isobath_UTM <- suppressMessages(convUL(isobath))
  PBSmapping::addLines(isobath_UTM, 
    col = rev(c("#00000060", "#00000045", "#00000030")), lwd = 0.6)
  
  plyr::d_ply(nepacUTM, "PID", function(i)
    polygon(i$X, i$Y, col = "grey90", border = "grey70", lwd = 0.4))
  
  # colour legend:
  if (plot_hexagons) {
    legend_image <- raster::as.raster(matrix(rev(leg$col), ncol = 1)) # fixme
    leg_top <- ylim[[2]] - 100 # how far legend is from top
    leg_bottom <- leg_top - diff(range(ylim)) * 0.2
    leg_left <- xlim[[2]] - 200 # longitude of legend left point
    
    for (lab in c(1, 10, 100, 1000, 10000)) {
      rasterImage(legend_image, leg_left, leg_bottom, leg_left + 25, leg_top)
      text_y <- leg_bottom + (leg_top-leg_bottom) * (leg[leg$raw_vals >= lab, "i"][1])
      text(leg_left + 15, text_y, lab, cex = 0.8, col = "grey20", pos = 4)
      # segments(-133.7, text_y, -133.8, text_y, lwd = 0.2, col = "white") # tick marks
    }
    text(leg_left + 15, leg_top + 12, "CPUE (kg/hr)", cex = 0.8, col = "grey20", pos = 4)
  }
  
  box(col = "grey60")
  
  boxes <- readRDS("data/boxes.rds")
  
  cols <- c(
    RColorBrewer::brewer.pal(9, "Blues")[5],
    RColorBrewer::brewer.pal(9, "Greens")[5],
    RColorBrewer::brewer.pal(9, "Reds")[5],
    RColorBrewer::brewer.pal(9, "Purples")[5]
  )
  
  for (i in seq_len(4)) {
    rect(xleft = boxes[[i]]$xlim[1]*10, xright = boxes[[i]]$xlim[2]*10, 
      ytop = boxes[[i]]$ylim[1]*10, ybottom = boxes[[i]]$ylim[2]*10,
      border = cols[i], col = NA, lwd = 1.25)
  }
}


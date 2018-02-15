hex_coords <- function (x, y, unitcell_x = 1, unitcell_y = 1) {
  data.frame(x = hexbin::hexcoords(unitcell_x)$x + x,
    y = hexbin::hexcoords(unitcell_y)$y + y)
}

ll2utm <- function(x, utm_zone = 9) {
  attr(x, "projection") <- "LL"
  attr(x, "zone") <- utm_zone
  suppressMessages(PBSmapping::convUL(x))
}

load_coastline <- function(xlim_ll, ylim_ll, utm_zone) {
  data("nepacLLhigh", package = "PBSmapping", envir = environment())
  np <- PBSmapping::clipPolys(nepacLLhigh,
    xlim = xlim_ll + c(-2, 2),
    ylim = ylim_ll + c(-2, 2))
  ll2utm(np, utm_zone = utm_zone)
}

load_isobath <- function(xlim_ll, ylim_ll, bath, utm_zone) {
  data("isobath", package = "PBSdata", envir = environment())
  isobath <- filter(isobath, .data$PID %in% bath)
  isobath <- PBSmapping::clipPolys(isobath,
    xlim = xlim_ll + c(-3, 3),
    ylim = ylim_ll + c(-3, 3))
  ll2utm(isobath, utm_zone = utm_zone)
}

#' Title TODO
#'
#' @param dat TODO
#' @param bin_width TODO
#' @param n_minimum_vessels TODO
#' @param pal_function TODO
#' @param xlim_ll TODO
#' @param ylim_ll TODO
#' @param utm_zone TODO
#' @param bath TODO
#' @param fill_scale TODO
#' @param add_survey_boxes TODO
#' @param surv_cols TODO
#'
#' @export
#' @importFrom utils data
#'
# d <- readRDS("data-cache/pbs-cpue.rds") %>%
#   filter(species_common_name == "pacific ocean perch") %>%
#   as_tibble()
# plot_cpue_map(d)

plot_cpue_map <- function(dat, bin_width = 7, n_minimum_vessels = 3,
  pal_function = viridisLite::viridis, xlim_ll = c(-134.1, -123.0),
  ylim_ll = c(48.4, 54.25), utm_zone = 9, bath = c(100, 200, 500),
  fill_scale = viridis::scale_fill_viridis(trans = "log10", option = "C"),
  add_survey_boxes = FALSE,
  surv_cols = c(
    "WCHG" = "#6BAED6",
    "HS" = "#74C476",
    "QCS" = "#FB6A4A",
    "WCVI" = "#9E9AC8")) {

  dat <- rename(dat, X = .data$lon, Y = .data$lat) %>%
    filter(!is.na(.data$cpue))
  plot_hexagons <- if (nrow(dat) == 0) FALSE else TRUE

  coastline_utm <- load_coastline(xlim_ll = xlim_ll, ylim_ll = ylim_ll,
    utm_zone = utm_zone)
  isobath_utm <- load_isobath(xlim_ll = xlim_ll, ylim_ll = ylim_ll,
    bath = bath, utm_zone = utm_zone)

  lims <- data.frame(X = sort(xlim_ll), Y = sort(ylim_ll))
  lims <- ll2utm(lims, utm_zone = utm_zone)
  xlim <- lims$X
  ylim <- lims$Y

  if (plot_hexagons) {

    dat <- ll2utm(dat, utm_zone = utm_zone)

    # count unique vessels per hexagon cell for privacy:
    g_count <- ggplot(dat, aes_string("X", "Y")) +
      coord_equal(xlim = xlim, ylim = ylim) +
      stat_summary_hex(aes_string(x = "X", y = "Y",
        z = "vessel_registration_number"),
        data = dat, binwidth = bin_width,
        fun = function(x) length(unique(x)))

    # the actual CPUE hexagon binning:
    g <- ggplot(dat, aes_string("X", "Y")) +
      coord_equal(xlim = xlim, ylim = ylim) +
      stat_summary_hex(
        aes_string(x = "X", y = "Y", z = "cpue"), # TODO log
        data = dat, binwidth = bin_width,
        fun = function(x) exp(mean(log(x), na.rm = FALSE)))

    # enact the privacy rule:
    gdat <- ggplot2::ggplot_build(g)$data[[1]]
    gdat_count <- ggplot2::ggplot_build(g_count)$data[[1]]
    stopifnot(nrow(gdat) == nrow(gdat_count))
    gdat <- gdat[gdat_count$value >= n_minimum_vessels, , drop = FALSE]

    if (nrow(gdat) == 0) {
      plot_hexagons <- FALSE
    } else {
      # compute hexagon x-y coordinates for geom_polygon()
      dx <- ggplot2::resolution(gdat$x, FALSE)
      dy <- ggplot2::resolution(gdat$y, FALSE) / 2 * 1.15
      public_dat <- lapply(seq_len(nrow(gdat)), function(i)
        data.frame(hex_id = i, cpue = gdat[i, "value"],
          hex_coords(gdat[i, "x"], gdat[i, "y"], dx, dy))) %>%
        bind_rows()
    }
  }

  g <- ggplot() +
    geom_path(data = isobath_utm, aes_string(x = "X", y = "Y",
      group = "paste(PID, SID)"),
      inherit.aes = FALSE, lwd = 0.3, col = "grey40", alpha = 0.3) +
    geom_polygon(data = coastline_utm,
      aes_string(x = "X", y = "Y", group = "PID"),
      inherit.aes = FALSE, lwd = 0.3, fill = "grey90", col = "grey70") +
    coord_equal(xlim = xlim, ylim = ylim) +
    theme_pbs() + labs(fill = "CPUE (kg/hr)", y = "Northing", x = "Easting")

  if (!plot_hexagons)
    return(g)

  g <- g + geom_polygon(data = public_dat, aes_string(x = "x", y = "y",
    fill = "cpue", group = "hex_id"), inherit.aes = FALSE) + fill_scale

  # `boxes` is from R/sysdata.rda
  if (add_survey_boxes) {
    g <- g + ggplot2::geom_rect(data = boxes, aes_string(
      xmin = "xmin", ymin = "ymin", ymax = "ymax", xmax = "xmax",
      colour = "survey"), inherit.aes = FALSE, fill = NA) +
      scale_colour_manual(values = surv_cols)
  }

  g
}

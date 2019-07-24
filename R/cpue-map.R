#' Plot a map of commercial CPUE
#'
#' @param dat Data from [get_cpue_spatial()] or [get_cpue_spatial_ll()]
#' @param start_year Starting year.
#' @param bin_width Width of hexagons in km.
#' @param n_minimum_vessels Minimum number of unique vessels before a hexagon is
#'   shown. Defaults to 3 to satisfy privacy requirements.
#' @param xlim X axis limits in UTM units.
#' @param ylim Y axis limits in UTM units.
#' @param utm_zone UTM zone.
#' @param bath A numeric vector of depths to show bathymetry countours at.
#' @param fill_scale A ggplot `scale_fill_*` function to control colour shading.
#' @param colour_scale A ggplot `scale_colour_*` function to control border of
#'   hexagon colours. This should likely match `fill_scale`.
#' @param rotation_angle Angle to rotate the entire map. Used in the synopsis
#'   report to rotate the coast 40 degrees to fit more plots on the page.
#' @param rotation_center The center in UTM coordinates around which to rotate
#'   the coast if it is rotated at all.
#' @param fill_lab Label for the color legend.
#' @param show_historical Should the historical extent of fishing (before
#'   `start_year`) be shown?
#' @param return_data Logical for whether to return the data instead of the plot.
#' @param min_cells The minimum number of cells needed before the hexagons are
#'   shown.
#' @param french Logical for French or English.
#' @param percent_excluded_xy If not `NULL`, should be a numeric vector of length 2
#'   corresponding to the x and y location (as fraction from the bottom left)
#'   of text describing the percentage of fishing events excluded due to the privacy rule.
#' @param percent_excluded_text The text to associate with the annotation showing
#'   the percentage of fishing events excluded due to the privacy rule.
#'
#' @export
#' @importFrom utils data
#'
#' @examples
#' ## fake data demo:
#' xlim <- c(-134.1, -123.0)
#' ylim <- c(48.4, 54.25)
#' d <- dplyr::tibble(lat = runif(1000, min(ylim), max(ylim)),
#'   lon = runif(length(lat), min(xlim), max(xlim)),
#'   species_common_name = "fake species", fishing_event_id = 1,
#'   year = 2013, cpue = rlnorm(length(lat), log(1000), 0.6),
#'   vessel_registration_number = rep(seq_len(100), each = 10))
#' plot_cpue_spatial(d, bin_width = 15, n_minimum_vessels = 1)

plot_cpue_spatial <-
  function(dat,
             start_year = 2013,
             bin_width = 7,
             n_minimum_vessels = 3,
             xlim = c(122, 890),
             ylim = c(5373, 6027),
             utm_zone = 9, bath = c(100, 200, 500),
             fill_scale = ggplot2::scale_fill_viridis_c(trans = "sqrt", option = "D"),
             colour_scale = ggplot2::scale_colour_viridis_c(trans = "sqrt", option = "D"),
             rotation_angle = 0,
             rotation_center = c(500, 5700),
             fill_lab = "CPUE (kg/hr)",
             show_historical = FALSE,
             return_data = FALSE,
             min_cells = 1,
             french = FALSE,
             percent_excluded_xy = NULL,
             percent_excluded_text = "Fishing events excluded due to Privacy Act") {

    dat <- filter(dat, !is.na(.data$cpue))
    dat <- filter(dat, !is.na(vessel_registration_number)) # for privacy rule

    pre_footprint_dat <- filter(dat, year < start_year)
    if (nrow(pre_footprint_dat) <= 1) show_historical <- FALSE
    # pre_footprint_dat <- dat
    dat <- filter(dat, year >= start_year)
    plot_hexagons <- if (nrow(dat) <= 1) FALSE else TRUE

    ll_range <- utm2ll(cbind(X = xlim, Y = ylim))
    coastline_utm <- load_coastline(
      xlim_ll = ll_range[, "X"] + c(-5, 5),
      ylim_ll = ll_range[, "Y"] + c(-5, 5),
      utm_zone = utm_zone
    )
    isobath_utm <- load_isobath(
      xlim_ll = ll_range[, "X"] + c(-5, 5),
      ylim_ll = ll_range[, "Y"] + c(-12, 12),
      bath = bath, utm_zone = utm_zone
    )

    dat <- rename(dat, X = .data$lon, Y = .data$lat)
    pre_footprint_dat <- rename(pre_footprint_dat, X = .data$lon, Y = .data$lat)

    if (plot_hexagons) {
      dat <- ll2utm(dat, utm_zone = utm_zone)

      if (show_historical)
        pre_footprint_dat <- ll2utm(pre_footprint_dat, utm_zone = utm_zone)

      privacy_out <- enact_privacy_rule(dat, bin_width = bin_width,
        n_minimum_vessels = n_minimum_vessels, xlim = xlim, ylim = ylim)
      gdat <- privacy_out$data

      if (show_historical) {
        privacy_out_historical <- enact_privacy_rule(pre_footprint_dat,
          bin_width = bin_width,
          n_minimum_vessels = n_minimum_vessels, xlim = xlim, ylim = ylim)
        if (nrow(privacy_out_historical$data) <= 1) show_historical <- FALSE
      }

      if (return_data) {
        return(gdat)
      } else {
        if (nrow(gdat) < min_cells) {
          plot_hexagons <- FALSE
        } else {
          public_dat <- compute_hexagon_xy(privacy_out$data, bin_width = bin_width)
          if (show_historical)
            public_dat_historical <-
              compute_hexagon_xy(privacy_out_historical$data, bin_width = bin_width)
        }
      }
    }
    # rotate if needed:

    isobath_utm <- rotate_df(isobath_utm, rotation_angle, rotation_center)
    coastline_utm <- rotate_df(coastline_utm, rotation_angle, rotation_center)

    g <- ggplot()

    if (plot_hexagons) {
      public_dat <- rotate_df(public_dat, rotation_angle, rotation_center)

      if (show_historical) {
        public_dat_historical <- rotate_df(public_dat_historical,
          rotation_angle, rotation_center)
        g <- g + geom_polygon(data = public_dat_historical, aes_string(
          x = "X", y = "Y", group = "hex_id"
        ), inherit.aes = FALSE, fill = "grey95", colour = "grey45", lwd = 0.2)
      }
      g <- g + geom_polygon(data = public_dat, aes_string(
        x = "X", y = "Y",
        fill = "cpue", colour = "cpue", group = "hex_id"
      ), inherit.aes = FALSE, lwd = 0.2) + fill_scale + colour_scale

    }

    suppressWarnings({
      suppressMessages({
        g <- g + geom_path(
          data = isobath_utm, aes_string(
            x = "X", y = "Y",
            group = "paste(PID, SID)"
          ),
          inherit.aes = FALSE, lwd = 0.4, col = "grey70", alpha = 0.4
        )
      })})

    g <- g + geom_polygon(
      data = coastline_utm,
      aes_string(x = "X", y = "Y", group = "PID"),
      inherit.aes = FALSE, lwd = 0.2, fill = "grey90", col = "grey70"
    ) +
      coord_equal(xlim = xlim, ylim = ylim) +
      theme_pbs() + labs(fill = fill_lab, colour = fill_lab, y = "Northing", x = "Easting")

    g <- g + theme(legend.justification = c(1, 1), legend.position = c(1, 1))

    if (!is.null(percent_excluded_xy) && exists("privacy_out")) {
      excluded_fe <- round(
        privacy_out$lost_fe_ids/privacy_out$total_fe_ids * 100, 0)
      if (excluded_fe == 0) excluded_fe <- "< 0.5%"
      g <- g + ggplot2::annotate("text",
        x = min(xlim) + percent_excluded_xy[1] * diff(range(xlim)),
        y = min(ylim) + percent_excluded_xy[2] * diff(range(ylim)),
        label = paste0(percent_excluded_text, ": ", excluded_fe, "%"),
        hjust = 0, colour = "grey30", angle = 90)
    }

    g
  }

hex_coords <- function(x, y, unitcell_x = 1, unitcell_y = 1) {
  data.frame(
    x = hexbin::hexcoords(unitcell_x)$x + x,
    y = hexbin::hexcoords(unitcell_y)$y + y
  )
}

utm2ll <- function(x, utm_zone = 9) {
  attr(x, "projection") <- "UTM"
  attr(x, "zone") <- utm_zone
  suppressMessages(PBSmapping::convUL(x))
}

ll2utm <- function(x, utm_zone = 9) {
  attr(x, "projection") <- "LL"
  attr(x, "zone") <- utm_zone
  suppressMessages(PBSmapping::convUL(x))
}

load_coastline <- function(xlim_ll, ylim_ll, utm_zone, buffer = 2) {
  data("nepacLLhigh", package = "PBSmapping", envir = environment())
  np <- PBSmapping::clipPolys(nepacLLhigh,
    xlim = xlim_ll + c(-buffer, buffer),
    ylim = ylim_ll + c(-buffer, buffer)
  )
  ll2utm(np, utm_zone = utm_zone)
}

load_isobath <- function(xlim_ll, ylim_ll, bath, utm_zone) {
  data("isobath", package = "PBSdata", envir = environment())
  isobath <- filter(isobath, .data$PID %in% bath)
  isobath <- PBSmapping::clipPolys(isobath,
    xlim = xlim_ll + c(-3, 3),
    ylim = ylim_ll + c(-3, 3)
  )
  ll2utm(isobath, utm_zone = utm_zone)
}

# Rotate coords
#
# @param x X coordinates.
# @param y Y coordinates.
# @param rotation_angle The rotation angle.
# @param rotation_center The coordinates about which to rotate.
#
# @examples
# x <- c(1:100, rep(100, 100), 100:1, rep(1, 100))
# y <- c(rep(1, 100), 1:100, rep(100, 100), 100:1)
# plot(x, y, asp = 1)
# points(50, 50, col = "red")
# z <- rotate_coords(x = x, y = y, rotation_angle = 24,
#   rotation_center = c(50, 50))
# plot(z$x, z$y, asp = 1)
# points(50, 50, col = "red")
rotate_coords <- function(x, y, rotation_angle, rotation_center) {
  assertthat::assert_that(identical(class(rotation_center), "numeric"))
  assertthat::assert_that(identical(class(rotation_angle), "numeric"))
  assertthat::assert_that(identical(length(rotation_center), 2L))
  assertthat::assert_that(identical(length(rotation_angle), 1L))
  assertthat::assert_that(identical(length(x), length(y)))

  rot <- -rotation_angle * pi / 180
  newangles <- atan2(y - rotation_center[2], x - rotation_center[1]) + rot
  mags <- sqrt((x - rotation_center[1])^2 + (y - rotation_center[2])^2)
  x <- rotation_center[1] + cos(newangles) * mags
  y <- rotation_center[2] + sin(newangles) * mags
  dplyr::tibble(x = x, y = y)
}

rotate_df <- function(df, rotation_angle, rotation_center) {
  r <- rotate_coords(df$X, df$Y,
    rotation_angle = rotation_angle,
    rotation_center = rotation_center
  )
  df$X <- r$x
  df$Y <- r$y
  df
}

enact_privacy_rule <- function(dat, bin_width, n_minimum_vessels, xlim, ylim) {
  # count unique vessels per hexagon cell for privacy:

  # Fake data to make sure that the hexagons overlap perfectly.
  # This extends the X and Y to extreme but identical limits every time.
  fake_rows <- dat[1:2, , drop = FALSE]
  fake_rows$fishing_event_id <- c(-999L, -998L)
  fake_rows$vessel_registration_number <- c(-999L, -998L)
  fake_rows$X <- c(-1000, 20000)
  fake_rows$Y <- c(-1000, 20000)
  dat <- bind_rows(dat, fake_rows)

  g_count <- ggplot(dat, aes_string("X", "Y")) +
    coord_equal(xlim = xlim, ylim = ylim) +
    stat_summary_hex(aes_string(
      x = "X", y = "Y",
      z = "vessel_registration_number"
    ),
      data = dat, binwidth = bin_width,
      fun = function(x) length(unique(x))
    )

  # count fishing events per hexagon cell to keep track of how many not shown:
  g_fe_id_count <- ggplot(dat, aes_string("X", "Y")) +
    coord_equal(xlim = xlim, ylim = ylim) +
    stat_summary_hex(aes_string(
      x = "X", y = "Y",
      z = "fishing_event_id"
    ),
      data = dat, binwidth = bin_width,
      fun = function(x) length(unique(x))
    )

  # the actual CPUE hexagon binning:
  g <- ggplot(dat, aes_string("X", "Y")) +
    coord_equal(xlim = xlim, ylim = ylim) +
    stat_summary_hex(
      aes_string(x = "X", y = "Y", z = "cpue"),
      data = dat, binwidth = bin_width,
      fun = function(x) exp(mean(log(x), na.rm = FALSE))
    )

  # enact the privacy rule:
  gdat <- ggplot2::ggplot_build(g)$data[[1]]
  gdat_count <- ggplot2::ggplot_build(g_count)$data[[1]]
  gdat_fe_id_count <- ggplot2::ggplot_build(g_fe_id_count)$data[[1]]

  # resolution <- c(ggplot2::resolution(gdat$x, FALSE), ggplot2::resolution(gdat$y, FALSE))

  # sanity check:
  stopifnot(identical(nrow(gdat), nrow(gdat_count)))
  stopifnot(identical(nrow(gdat), nrow(gdat_fe_id_count)))
  # Number of hexagon cells for vessel count and CPUE didn't match.
  # Stopping because the privacy rule might not remain valid in this case.

  gdat <- gdat[gdat_count$value >= n_minimum_vessels, , drop = FALSE]

  lost_fe_id_df <- gdat_fe_id_count[gdat_count$value < n_minimum_vessels, ,
    drop = FALSE]
  lost_fe_ids <- sum(lost_fe_id_df$value)
  total_fe_ids <- sum(gdat_fe_id_count$value)

  list(data = gdat, lost_fe_ids = lost_fe_ids, total_fe_ids = total_fe_ids)
}

compute_hexagon_xy <- function(gdat, bin_width) {
  # compute hexagon x-y coordinates for geom_polygon()
  dx <- bin_width/2
  dy <- bin_width/2
  public_dat <- lapply(seq_len(nrow(gdat)), function(i)
    data.frame(
      hex_id = i, cpue = gdat[i, "value"],
      hex_coords(gdat[i, "x"], gdat[i, "y"], dx, dy)
    )) %>%
    bind_rows()
  public_dat$X <- public_dat$x
  public_dat$Y <- public_dat$y
  public_dat
}

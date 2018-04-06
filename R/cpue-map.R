#' Title TODO
#'
#' @param dat TODO
#' @param bin_width TODO
#' @param n_minimum_vessels TODO
#' @param xlim_ll TODO
#' @param ylim_ll TODO
#' @param utm_zone TODO
#' @param bath TODO
#' @param fill_scale TODO
#' @param surv_cols TODO
#' @param fill_lab TODO
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
#'   species_common_name = "fake species",
#'   cpue = rlnorm(length(lat), log(1000), 0.6),
#'   vessel_registration_number = rep(seq_len(100), each = 10))
#' plot_cpue_spatial(d, bin_width = 15, n_minimum_vessels = 1)

plot_cpue_spatial <- function(dat, bin_width = 7, n_minimum_vessels = 3,
                              xlim_ll = c(-134.1, -123.0),
                              ylim_ll = c(48.4, 54.25), utm_zone = 9, bath = c(100, 200, 500),
                              fill_scale = viridis::scale_fill_viridis(trans = "sqrt", option = "C"),
                              surv_cols = c(
                                "WCHG" = "#6BAED6",
                                "HS" = "#74C476",
                                "QCS" = "#FB6A4A",
                                "WCVI" = "#9E9AC8"
                              ),
                              rotation_angle = 0,
                              rotation_center = c(500, 5700),
                              fill_lab = "CPUE (kg/hr)") {
  dat <- rename(dat, X = .data$lon, Y = .data$lat) %>%
    filter(!is.na(.data$cpue))
  plot_hexagons <- if (nrow(dat) == 0) FALSE else TRUE

  coastline_utm <- load_coastline(
    xlim_ll = xlim_ll, ylim_ll = ylim_ll,
    utm_zone = utm_zone
  )
  isobath_utm <- load_isobath(
    xlim_ll = xlim_ll, ylim_ll = ylim_ll,
    bath = bath, utm_zone = utm_zone
  )

  lims <- data.frame(X = sort(xlim_ll), Y = sort(ylim_ll))
  lims <- ll2utm(lims, utm_zone = utm_zone)
  xlim <- lims$X
  ylim <- lims$Y

  if (plot_hexagons) {
    dat <- ll2utm(dat, utm_zone = utm_zone)

    # count unique vessels per hexagon cell for privacy:
    g_count <- ggplot(dat, aes_string("X", "Y")) +
      coord_equal(xlim = xlim, ylim = ylim) +
      stat_summary_hex(aes_string(
        x = "X", y = "Y",
        z = "vessel_registration_number"
      ),
      data = dat, binwidth = bin_width,
      fun = function(x) length(unique(x))
      )

    # the actual CPUE hexagon binning:
    g <- ggplot(dat, aes_string("X", "Y")) +
      coord_equal(xlim = xlim, ylim = ylim) +
      stat_summary_hex(
        aes_string(x = "X", y = "Y", z = "cpue"), # TODO log
        data = dat, binwidth = bin_width,
        fun = function(x) exp(mean(log(x), na.rm = FALSE))
      )

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
        data.frame(
          hex_id = i, cpue = gdat[i, "value"],
          hex_coords(gdat[i, "x"], gdat[i, "y"], dx, dy)
        )) %>%
        bind_rows()
    }
  }

  # rotate if needed:
  isobath_utm <- rotate_df(isobath_utm, rotation_angle, rotation_center)
  coastline_utm <- rotate_df(coastline_utm, rotation_angle, rotation_center)

  g <- ggplot() + geom_path(
    data = isobath_utm, aes_string(
      x = "X", y = "Y",
      group = "paste(PID, SID)"
    ),
    inherit.aes = FALSE, lwd = 0.3, col = "grey40", alpha = 0.3
  )

  if (plot_hexagons) {
    public_dat$X <- public_dat$x
    public_dat$Y <- public_dat$y
    public_dat <- rotate_df(public_dat, rotation_angle, rotation_center)
    g <- g + geom_polygon(data = public_dat, aes_string(
      x = "X", y = "Y",
      fill = "cpue", group = "hex_id"
    ), inherit.aes = FALSE) + fill_scale
  }

  g <- g + geom_polygon(
    data = coastline_utm,
    aes_string(x = "X", y = "Y", group = "PID"),
    inherit.aes = FALSE, lwd = 0.2, fill = "grey90", col = "grey70"
  ) +
    coord_equal(xlim = xlim, ylim = ylim) +
    theme_pbs() + labs(fill = fill_lab, y = "Northing", x = "Easting")

##  # `boxes` is from R/sysdata.rda
##  if (add_survey_boxes) {
##    g <- g + ggplot2::geom_rect(data = boxes, aes_string(
##      xmin = "xmin", ymin = "ymin", ymax = "ymax", xmax = "xmax",
##      colour = "survey"
##    ), inherit.aes = FALSE, fill = NA) +
##      scale_colour_manual(values = surv_cols)
##  }

  g <- g + theme(legend.justification = c(1, 1), legend.position = c(1, 1))
  g
}

hex_coords <- function(x, y, unitcell_x = 1, unitcell_y = 1) {
  data.frame(
    x = hexbin::hexcoords(unitcell_x)$x + x,
    y = hexbin::hexcoords(unitcell_y)$y + y
  )
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

#' Rotate coords
#'
#' @param x TODO
#' @param y TODO
#' @param rotation_angle TODO
#' @param rotation_center TODO
#'
#' @examples
#' x <- c(1:100, rep(100, 100), 100:1, rep(1, 100))
#' y <- c(rep(1, 100), 1:100, rep(100, 100), 100:1)
#' plot(x, y, asp = 1)
#' points(50, 50, col = "red")
#' z <- rotate_coords(x = x, y = y, rotation_angle = 24,
#'   rotation_center = c(50, 50))
#' plot(z$x, z$y, asp = 1)
#' points(50, 50, col = "red")
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

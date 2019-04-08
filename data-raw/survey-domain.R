extract_synoptic_survey_domain <- function(dat = gfdata::get_active_survey_blocks(),
                                           survey_abbreviation = "QCS",
                                           diagnostic_plots = TRUE) {
  library(ggplot2)
  library(dplyr)

  shape <- dat
  shape <- dplyr::filter(shape, grepl(survey_abbreviation, substr(survey_series_name, 1, 5))) %>%
    dplyr::rename(depth = survey_site_depth_m) %>%
    dplyr::mutate(depth = -depth)

  message(unique(shape$survey_series_name))

  shape <- rename(shape, X = pt1_lon, Y = pt1_lat) %>%
    gfplot:::ll2utm(utm_zone = 9) %>%
    rename(pt1_lon = X, pt1_lat = Y) %>%
    rename(X = pt2_lon, Y = pt2_lat) %>%
    gfplot:::ll2utm(utm_zone = 9) %>%
    rename(pt2_lon = X, pt2_lat = Y) %>%
    rename(X = pt3_lon, Y = pt3_lat) %>%
    gfplot:::ll2utm(utm_zone = 9) %>%
    rename(pt3_lon = X, pt3_lat = Y) %>%
    rename(X = pt4_lon, Y = pt4_lat) %>%
    gfplot:::ll2utm(utm_zone = 9) %>%
    rename(pt4_lon = X, pt4_lat = Y) %>%
    group_by(block_designation) %>%
    mutate(longitude_centre = mean(c(pt1_lon, pt2_lon, pt3_lon, pt4_lon))) %>%
    mutate(latitude_centre = mean(c(pt1_lat, pt2_lat, pt3_lat, pt4_lat))) %>%
    rename(X = longitude_centre, Y = latitude_centre) %>%
    ungroup() %>%
    select(X, Y, depth, survey_series_name)

  perfect_x <- seq(min(shape$X), max(shape$X), 2)
  perfect_y <- seq(min(shape$Y), max(shape$Y), 2)

  .diffx <- perfect_x[1] - floor(perfect_x[1])
  .diffy <- perfect_y[1] - floor(perfect_y[1])

  pred_grid <- shape

  if (diagnostic_plots) {
    g3 <- ggplot(pred_grid, aes(X, Y, colour = depth)) +
      geom_point() +
      scale_colour_viridis_c(option = "C", direction = -1, trans = "sqrt") +
      coord_fixed() +
      geom_vline(xintercept = seq(min(pred_grid$X), max(pred_grid$X), 2), colour = "#00000050") +
      geom_hline(yintercept = seq(min(pred_grid$Y), max(pred_grid$Y), 2), colour = "#00000050")
  }

  pred_grid$X <- round(pred_grid$X - .diffx) + .diffx
  pred_grid$Y <- round(pred_grid$Y - .diffy) + .diffy

  if (diagnostic_plots) {
    g1 <- ggplot(pred_grid, aes(X, Y, fill = depth)) +
      geom_raster() +
      scale_fill_viridis_c(option = "C", direction = -1, trans = "sqrt") +
      coord_fixed()

    g2 <- ggplot(pred_grid, aes(X, Y, colour = depth)) +
      geom_point() +
      scale_colour_viridis_c(option = "C", direction = -1, trans = "sqrt") +
      coord_fixed() +
      geom_vline(xintercept = seq(min(pred_grid$X), max(pred_grid$X), 2), colour = "#00000050") +
      geom_hline(yintercept = seq(min(pred_grid$Y), max(pred_grid$Y), 2), colour = "#00000050")

    gridExtra::grid.arrange(g3, g2, g1, ncol = 2)
  }

  out <- list()
  out$grid <- pred_grid
  out$cell_area <- 4
  out
}

# d <- readRDS("~/Downloads/active_survey_blocks.rds")
d <- gfdata::get_active_survey_blocks()

syn_grid <- list()
syn_grid$qcs <- extract_synoptic_survey_domain(d, "QCS")
syn_grid$hs <- extract_synoptic_survey_domain(d, "HS")
syn_grid$wcvi <- extract_synoptic_survey_domain(d, "WCVI")
syn_grid$wchg <- extract_synoptic_survey_domain(d, "WCHG")

.names <- c("qcs", "hs", "wcvi", "wchg")
syn_grid_df <- purrr::map2_df(
  syn_grid, .names,
  ~ data.frame(
    survey = paste("SYN", toupper(.y)),
    .x$grid,
    cell_area = 4,
    utm_zone = 9,
    survey_domain_year = 2017,
    stringsAsFactors = FALSE
  )
)

synoptic_grid <- syn_grid_df

# There is one missing depth value
# we will interpolate that here:
.missing <- synoptic_grid[is.na(synoptic_grid$depth), ]
stopifnot(nrow(.missing) == 1L)
.buffer <- 2.1
interpolated <- dplyr::filter(
  synoptic_grid, X > (.missing$X - .buffer), X < (.missing$X + .buffer),
  Y > (.missing$Y - .buffer), Y < (.missing$Y + .buffer)
) %>%
  pull(depth) %>%
  mean(na.rm = T)
synoptic_grid$depth[is.na(synoptic_grid$depth)] <- interpolated

# Remove a couple cells that are at sea level which will cause problems when working with log depth:
synoptic_grid <- synoptic_grid[synoptic_grid$depth > 0, ]

synoptic_grid %>%
  dplyr::filter(survey == "SYN HS") %>%
  ggplot(aes(X, Y, fill = depth)) +
  geom_raster() +
  geom_raster(data = dplyr::filter(synoptic_grid, survey == "SYN QCS")) +
  geom_raster(data = dplyr::filter(synoptic_grid, survey == "SYN WCVI")) +
  geom_raster(data = dplyr::filter(synoptic_grid, survey == "SYN WCHG")) +
  scale_fill_viridis_c(option = "C", direction = -1, trans = "log") +
  coord_fixed()

usethis::use_data(synoptic_grid, internal = FALSE, overwrite = TRUE)

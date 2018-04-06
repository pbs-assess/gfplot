#' @param cell_width Cell width in units of the data (e.g. UTM km).
#' @param region Optional region abbreviation to search for a spatial polygon
#'   file. TODO
#' @param cache_folder TODO
#'
#' @export
#' @rdname survey-spatial-modelling
make_prediction_grid <- function(dat, cell_width = 2, survey = NULL,
                                 cache_folder = "prediction-grids",
                                 utm_zone = 9,
                                 survey_boundary = NULL) {
  # if (n != 150) stop("Grid is currently fixed at 150. Leave `n = 150`.")
  if (is.null(region)) {
    region <- "no-region"
    x <- dat$X
    y <- dat$Y
    z <- chull(x, y)
    coords <- cbind(x[z], y[z])
    coords <- rbind(coords, coords[1, ])
    sp_poly <- sp::SpatialPolygons(
      list(sp::Polygons(list(sp::Polygon(coords)), ID = 1))
    )
    sp_poly_df <- sp::SpatialPolygonsDataFrame(sp_poly,
      data = data.frame(ID = 1)
    )
    pred_grid <- expand.grid(
      X = seq(round_down_even(min(dat$X)), max(dat$X), cell_width),
      Y = seq(round_down_even(min(dat$Y)), max(dat$Y), cell_width),
      year = unique(dat$year)
    )
  } else {
    region <- gsub("SYN[ _]+", "", survey)

    if (!region %in% names(gfplot::survey_grids) && is.null(survey_boundary)) {
      stop(
        survey, " is not defined in `gfplot::survey_grids`.",
        " Please supply your own survey boundary to the make_prediction_grid()",
        " `survey_boundary` argument as a data frame with columns `X` and `Y`."
      )
    }

    shape_utm <- ll2utm(gfplot::survey_grids[[region]],
      utm_zone = utm_zone
    )
    sp_poly <- sp::SpatialPolygons(
      list(sp::Polygons(list(sp::Polygon(shape_utm)), ID = 1))
    )
    sp_poly_df <- sp::SpatialPolygonsDataFrame(sp_poly,
      data = data.frame(ID = 1)
    )
    pred_grid <- expand.grid(
      X = seq(round_down_even(min(shape_utm$X)), max(shape_utm$X), cell_width),
      Y = seq(round_down_even(min(shape_utm$Y)), max(shape_utm$Y), cell_width),
      year = unique(dat$year)
    )
  }

  cell_width <- cell_width
  cell_height <- cell_width
  cell_area <- cell_width * cell_height

  sp::coordinates(pred_grid) <- c("X", "Y")
  inside <- !is.na(sp::over(pred_grid, as(sp_poly_df, "SpatialPolygons")))
  pred_grid <- pred_grid[inside, ]
  pred_grid <- as.data.frame(pred_grid)

  xo <- sort(unique(pred_grid$X))
  yo <- sort(unique(pred_grid$Y))

  dir.create(cache_folder, showWarnings = FALSE)
  file_name <- paste0(
    cache_folder, "/", region,
    "pred-grid-interp-cell-width-", cell_width, ".rds"
  )

  if (file.exists(file_name) & !is.null(region)) {
    message("Preloading interpolated depth for prediction grid...")
    ii <- readRDS(file_name, compress = FALSE)
  }

  if (!file.exists(file_name) & !is.null(region)) {
    message("Interpolating depth for prediction grid...")
    bath <- load_bath(utm_zone = utm_zone) %>%
      filter(
        X < max(dat$X + 20),
        X > min(dat$X - 20),
        Y < max(dat$Y + 20),
        Y > min(dat$Y - 20),
        depth > 0
      )
    ii <- akima::interp(
      x = bath$X,
      y = bath$Y,
      z = log(bath$depth),
      xo = xo,
      yo = yo, extrap = TRUE, linear = TRUE
    )
    saveRDS(ii, file_name, compress = FALSE)
  }

  z <- reshape2::melt(ii$z)
  z$x <- ii$x[z$Var1]
  z$y <- ii$y[z$Var2]
  z <- filter(z, paste(x, y) %in% paste(pred_grid$X, pred_grid$Y))
  z <- rename(z, X = x, Y = y, akima_depth = value) %>%
    select(-Var1, -Var2)

  pred_grid <- left_join(pred_grid, z, by = c("X", "Y"))
  pred_grid <- mutate(pred_grid, akima_depth = exp(akima_depth))

  if (is.null(region)) {
    pred_grid <- filter(
      pred_grid, akima_depth >= min(dat$akima_depth),
      akima_depth <= max(dat$akima_depth)
    )
  }

  pred_grid <- filter(pred_grid, !is.na(akima_depth))
  pred_grid$depth_scaled <-
    (log(pred_grid$akima_depth) - dat$depth_mean[1]) / dat$depth_sd[1]
  pred_grid$depth_scaled2 <- pred_grid$depth_scaled^2
  list(grid = pred_grid, cell_area = cell_area)
}

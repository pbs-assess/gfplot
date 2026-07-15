#' Create coord_sf with automatic limits from sf object
#'
#' Creates a ggplot2 coord_sf object with axis limits automatically determined
#' from an sf object's bounding box, with optional buffering and geometry
#' simplification for performance.
#'
#' @param sf_obj An sf object to derive plot limits from
#' @param xlim Optional x-axis limits as vector c(xmin, xmax) (default: NULL, uses bbox)
#' @param ylim Optional y-axis limits as vector c(ymin, ymax) (default: NULL, uses bbox)
#' @param buffer Buffer distance in meters (default: 1000)
#' @param crs_buffer Coordinate system for buffering (default: 3156, BC Albers)
#' @param simplify Simplification tolerance in meters (default: NULL, no simplification).
#'   Only needed for complex boundaries if rendering is slow.
#' @param ... Additional arguments passed to ggplot2::coord_sf()
#'
#' @return A ggplot2 coord_sf object with appropriate limits
#' @export

coord_sf_auto <- function(sf_obj, xlim = NULL, ylim = NULL, buffer = 1000,
                          crs_buffer = 3156, simplify = NULL, ...) {

  stopifnot("sf_obj must be an sf object" = inherits(sf_obj, "sf"))

  # Apply buffer if specified
  if (!is.null(buffer) && buffer > 0) {
    crs_current <- sf::st_crs(sf_obj)

    if (identical(crs_current$units, "m")) {
      # buffer direclty if already in metres
      sf_obj <- sf::st_buffer(sf_obj, dist = buffer)
    } else {
      # simplify before transformation if specified (faster)
      if (!is.null(simplify)) {
        sf_obj <- sf::st_simplify(sf_obj, dTolerance = simplify)
      }

      # Transform to buffering CRS, then buffer
      sf_obj <- sf_obj |>
        sf::st_transform(crs = crs_buffer) |>
        sf::st_buffer(dist = buffer)
    }
  }

  bbox <- sf::st_bbox(sf_obj)

  if (is.null(xlim)) xlim <- bbox[c("xmin", "xmax")]
  if (is.null(ylim)) ylim <- bbox[c("ymin", "ymax")]

  ggplot2::coord_sf(xlim = xlim, ylim = ylim, crs = sf::st_crs(sf_obj), ...)
}

#' Rotate spatial features for plotting BC coastline
#'
#' Rotates sf objects using oblique Mercator projection to align BC's
#' northwest-southeast coastline with plot axes.
#'
#' @param sf_obj An sf object to rotate
#' @param angle Rotation angle in degrees (default: -40). Negative values rotate clockwise.
#' @param lonc Central meridian longitude (default: -129, roughly central for
#'   the BC coast). If you're layering multiple rotated sf objects on the same
#'   plot, keep `lonc` fixed across all of them -- each object's own bbox
#'   midpoint will generally differ, and rotating layers to different central
#'   meridians will misalign them. Pass `lonc = NULL` to compute that
#'   per-object midpoint instead (reported via message), e.g. to choose a better
#'   midpoint `lonc` value for your data.
#'
#' @return sf object in rotated oblique Mercator projection
#' @export
#'
#' @examples
#' \dontrun{
#' coast <- pacea::bc_coast
#' coast_rotated <- rotate_sf(coast, angle = -40)
#'
#' ggplot(coast_rotated) + geom_sf()
#' }
rotate_sf <- function(sf_obj, angle = -40, lonc = -129) {
  if (is.null(lonc)) {
    bbox_ll <- sf_obj |> sf::st_transform(4326) |> sf::st_bbox()
    lonc <- mean(bbox_ll[c("xmin", "xmax")])
    message(
      "Using lonc = ", round(lonc, 2), " (midpoint of sf_obj's longitude range). ",
      "If layering multiple sf objects on one plot, pass this same lonc to each ",
      "rotate_sf() call rather than leaving lonc = NULL, or the layers won't align."
    )
  }

  rotated_crs <- paste0("+proj=omerc +lat_0=0 +lonc=", lonc, " +gamma=", -angle)

  sf_obj |> sf::st_transform(rotated_crs)
}

#' Convert XY coordinates to sf object
#'
#' Converts `X`/`Y` coordinate columns, as produced by
#' `sdmTMB::add_utm_columns()` (typically in km), to an sf object. Also
#' works for general conversions of point data to sf.
#'
#' @param data Data frame containing coordinate columns.
#' @param coords Vector of coordinate column names (default: c("X", "Y")).
#' @param mult Multiplier applied to `coords` before constructing geometry
#'   (default: 1000, i.e. km -> m). Automatically set to 1 if `crs_from` is
#'   a geographic (lon/lat) CRS.
#' @param crs_from Source coordinate reference system. Defaults to
#'   EPSG:32609 (UTM zone 9N), matching Pacific region sdmTMB workflows -- pass
#'   explicitly for other regions.
#' @param crs_to Target coordinate reference system (default: 4326, WGS84).
#'
#' @return sf object with geometry in `crs_to`.
#' @export
XY_to_sf <- function(data, coords = c("X", "Y"),
                     mult = 1000,
                     crs_from = 32609, crs_to = 4326) {
  if (!all(coords %in% names(data))) {
    missing_cols <- coords[!coords %in% names(data)]
    stop("Coordinate column(s) not found: ", paste(missing_cols, collapse = ", "))
  }

  if (missing(crs_from)) {
    message(
      "Assuming crs_from = 32609 (UTM zone 9N). ",
      "Pass crs_from explicitly for other regions."
    )
  }

  if (sf::st_is_longlat(sf::st_crs(crs_from))) {
    if (!missing(mult) && mult != 1) {
      message("crs_from is in degrees; ignoring mult = ", mult, " (using 1).")
    }
    mult <- 1
  }

  data |>
    dplyr::mutate(
      x = .data[[coords[1]]] * mult,
      y = .data[[coords[2]]] * mult
    ) |>
    sf::st_as_sf(coords = c("x", "y"), crs = crs_from) |>
    sf::st_transform(crs = crs_to)
}
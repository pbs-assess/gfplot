# library(PBSdata)
# library(PBSmapping)
# data("locality", package = "PBSdata")
# head(locality.plus)
# library(ggplot2)
# library(dplyr)
#
# dh <- readRDS("~/src/pcod-scenarios-2018/data/cpue-historic.rds")
# loc_look <- select(dh, locality_code, locality_description) %>% unique()
# d <- readRDS("~/src/pcod-scenarios-2018/data/cpue-modern.rds")
#
# pbs_areas <- gfplot::pbs_areas[grep(
#   "^3C|^3D|^5A|^5B|^5C|^5D|^5E|^4B",
#   gfplot::pbs_areas$major_stat_area_description
# ), ]
# d <- dplyr::inner_join(d, pbs_areas, by = "major_stat_area_code")
# d <- left_join(d, loc_look, by = "locality_code")
#
# # top_loc <- d %>% filter(locality_description != "unknown") %>%
# #   filter(species_code == "222") %>%
# #   group_by(locality_description) %>% summarise(n = sum(landed_kg)) %>%
# #   arrange(-n)
#
# # %>% dplyr::top_n(30)

plot_dfo_localities <- function(localities = "all",
                                xlim = c(-134, -124), ylim = c(48, 55), french=FALSE) {
  data("locality", package = "PBSdata", envir = environment())
  x <- attributes(locality)$PolyData
  x$name_lower <- tolower(x$name)

  x$unique_locality_code <- paste(x$major, x$minor,
    x$locality, sep = "-")

  localities <- gsub("^0", "", localities)
  localities <- gsub("^([0-9]+-)0", "\\1", localities)

  if (localities[[1]] != "all") {
    if (any(!localities %in% x$unique_locality_code)) {
      message("Missing:")
      message(paste(sort(localities[!localities %in% x$unique_locality_code]), collapse = ", "))
    }
    x <- filter(x, .data$unique_locality_code %in% tolower(localities))
  }

  ll_range <- cbind(X = xlim, Y = ylim)
  utm_range <- ll2utm(ll_range, utm_zone = 9)

  coastline_utm <- load_coastline(
    xlim_ll = ll_range[, "X"] + c(-5, 5),
    ylim_ll = ll_range[, "Y"] + c(-5, 5),
    utm_zone = 9)

  locality_utm <- ll2utm(locality, utm_zone = 9)
  x_utm <- ll2utm(x, utm_zone = 9)

  locality_utm <- inner_join(locality_utm, select(x, -X, -Y), by = c("PID", "SID"))

  g <- ggplot2::ggplot(locality_utm, ggplot2::aes_string(
    x = "X", y = "Y",
    group = "PID", fill = "name")) +
    ggplot2::geom_polygon(col = "grey20")

  g <- g + ggplot2::geom_polygon(
    data = coastline_utm,
    ggplot2::aes_string(x = "X", y = "Y", group = "PID"),
    inherit.aes = FALSE, lwd = 0.2, fill = "grey87", col = "grey70") +
    ggplot2::coord_equal(xlim = utm_range$X, ylim = utm_range$Y) +
    ggplot2::scale_fill_manual(na.value = "white", values = rep("grey60", 1e3)) +
    theme_pbs() + ggplot2::guides(fill = "none", colour = "none") +
    ggplot2::labs(y = en2fr("Northing", translate=french), x = en2fr("Easting", translate=french))

  if (localities[[1]] != "all") {
    label_type <- if (french) "unique_locality_code" else "name"
    g <- g + ggrepel::geom_text_repel(
      data = x_utm,
      ggplot2::aes_string("X", "Y", label = label_type),
      size = 2.8, colour = "grey30",
      # point.padding = unit(1, "lines"),
      max.iter = 6e3, segment.size = 0.5)
  }
  g
}

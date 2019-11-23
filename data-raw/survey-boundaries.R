parse_shape <- function(prefix,
                        path = "inst/extdata/SynopticTrawlSurveyBoundaries/",
                        suffix = "_BLOB", type = c("syn", "iphc", "hbll")) {

  type <- match.arg(type)

  setwd(path)

  shape <- rgdal::readOGR(
    dsn = ".",
    layer = paste0(prefix, suffix), verbose = FALSE
  )
  setwd("../../../")

  if (type == "syn") {
    shape <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
  }
  if (type == "iphc") {
    d <- shape@data
    d <- dplyr::filter(d, LON < 0) # some 0 values
    shape <- dplyr::select(d, LON, LAT)
  }
  if (type == "hbll") {
    pts <- rgeos::gCentroid(shape, byid = TRUE)
    pts <- proj4::project(pts@coords,
      proj =  sp::proj4string(shape),
      inv = TRUE)
    shape <- as.data.frame(pts)
  }

  names(shape) <- c("X", "Y")
  shape
}

surv <- c("HS", "QCS", "WCHG", "WCVI")
out_syn <- lapply(surv, parse_shape)
names(out_syn) <- surv

# out_iphc <- parse_shape("IPHC", "inst/extdata/LLSurveyBoundaries",
#   "_Research_SitesII", type = "iphc")
# out_phma <- parse_shape("PHMA", "inst/extdata/LLSurveyBoundaries",
#   "_MASTER_GRID_copy", type = "hbll")

# out <- c(out_syn, list(HBLL = out_phma, IPHC = out_iphc))
survey_boundaries <- out_syn

usethis::use_data(survey_boundaries, internal = FALSE, overwrite = TRUE)

library(rgdal)
setwd("inst/extdata/HBLL-N-S/")
shape <- rgdal::readOGR(
  dsn = ".",
  layer = "PHMA_N_GRID", verbose = FALSE)
plot(shape)
head(shape@data)

library(dplyr)
hbll_grid <- select(shape@data, LONGITUDE, LATITUDE, DEPTH_M) %>%
  rename(X = LONGITUDE, Y = LATITUDE, depth = DEPTH_M)
hbll_grid <- mutate(hbll_grid, depth = -depth) %>%
  filter(depth > 0)
setwd("../../../")
hbll_n_grid <- list(grid = hbll_grid, cell_area = 2.0)
usethis::use_data(hbll_n_grid, internal = FALSE, overwrite = TRUE)

# South:
setwd("inst/extdata/HBLL-N-S/")
shape <- rgdal::readOGR(
  dsn = ".",
  layer = "PHMA_S_GRID", verbose = FALSE)
plot(shape)
hbll_grid <- select(shape@data, LONGITUDE, LATITUDE, DEPTH_M) %>%
  rename(X = LONGITUDE, Y = LATITUDE, depth = DEPTH_M)
hbll_grid <- mutate(hbll_grid, depth = -depth) %>%
  filter(depth > 0)
setwd("../../../")
hbll_s_grid <- list(grid = hbll_grid, cell_area = 2.0)
usethis::use_data(hbll_s_grid, internal = FALSE, overwrite = TRUE)

# # Inside HBLL N:
# setwd("inst/extdata/HBLL-inside/")
# shape <- rgdal::readOGR(
#   dsn = ".",
#   layer = "HBLL_INS_active_2019", verbose = FALSE)
# # plot(shape)
# hbll_ins_ngrid <- select(shape@data, LONGITUDE, LATITUDE, DEPTH_M) %>%
#   rename(X = LONGITUDE, Y = LATITUDE, depth = DEPTH_M)
# hbll_grid <- mutate(hbll_grid, depth = -depth) %>%
#   filter(depth > 0)
# setwd("../../../")
# hbll_s_grid <- list(grid = hbll_grid, cell_area = 2.0)
# usethis::use_data(hbll_s_grid, internal = FALSE, overwrite = TRUE)


#
#
# ####
# setwd("inst/extdata/LLSurveyBoundaries")
# shape <- rgdal::readOGR(
#   dsn = ".",
#   layer = "PHMA_MASTER_GRID_copy", verbose = FALSE)
# plot(shape)
# head(shape@data)
# x <- filter(shape@data, PHMA_NS_Zo == "N") %>% pull(Centroid)
# x <- mutate(x, lat = gsub("[0-9 "))

# HBLL inside:

setwd("inst/extdata/HBLL_inside_survey/")
shape <- rgdal::readOGR(
  dsn = ".",
  layer = "HBLL_Inside_North", verbose = TRUE)
plot(shape)
pts <- rgeos::gCentroid(shape, byid = TRUE)
hbll_inside_n_grid <- as.data.frame(pts) %>%
  rename(X = x, Y = y)
hbll_inside_n_grid <- list(grid = hbll_inside_n_grid, cell_area = 2.0)
usethis::use_data(hbll_inside_n_grid, internal = FALSE, overwrite = TRUE)

shape <- rgdal::readOGR(
  dsn = ".",
  layer = "HBLL_Inside_South", verbose = TRUE)
plot(shape)
pts <- rgeos::gCentroid(shape, byid = TRUE)
hbll_inside_s_grid <- as.data.frame(pts) %>%
  rename(X = x, Y = y)
hbll_inside_s_grid <- list(grid = hbll_inside_s_grid, cell_area = 2.0)
usethis::use_data(hbll_inside_s_grid, internal = FALSE, overwrite = TRUE)

setwd("../../../")

# dogfish ----------------------------

setwd("inst/extdata/dogfish")

library(sf)
library(ggplot2)
library(dplyr)

# read dogfish polygon data and transform to UTM
dogfish <- st_read(".") %>%
  st_transform(26909)

## Select boundary box based on survey area
minLat2 <- st_bbox(dogfish)[[2]]
maxLat2 <- st_bbox(dogfish)[[4]]
minLong2 <- st_bbox(dogfish)[[1]]
maxLong2 <- st_bbox(dogfish)[[3]]

library(raster)
library(fasterize)
# First generate grid for entire area that just excludes landmass
projCRS <- "+proj=utm +zone=9 +datum=WGS84"
coast <- rnaturalearth::ne_states("Canada", returnclass = "sf")
coastUTM <- st_transform(coast, crs = projCRS)
cropR <- raster(extent(minLong2, maxLong2, minLat2, maxLat2),
  crs = projCRS, res = 500 # meters
)
g <- fasterize(coastUTM, cropR)

## fast conversion pixel to polygons
p <- spex::polygonize(!is.na(g))
p <- p %>% st_transform(26909)

## layer is whether we are on land or not
# plot(subset(p, !layer)$geometry)
# plot(coastUTM$geometry, add = TRUE)

st_crs(dogfish)
st_crs(p)

fullGrid <- subset(p, !layer)$geometry %>%
  st_sf(ID = seq(1, length(.), by = 1))

inside <- fullGrid %>% st_join(dogfish, join = st_intersects, left = FALSE)
plot(st_geometry(inside))

dogfish_grid <- inside %>%
  st_coordinates(.)
gridOut <- data.frame(
  X = dogfish_grid[, "X"],
  Y = dogfish_grid[, "Y"]
)

dogfish_grid <- list(grid = gridOut, cell_area = 0.5)
usethis::use_data(dogfish_grid, internal = FALSE, overwrite = TRUE)

setwd("../../../")

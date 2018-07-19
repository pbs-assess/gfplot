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

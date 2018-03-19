


parse_shape <- function(region) {
  setwd("inst/extdata/SynopticTrawlSurveyBoundaries/")
  shape <- rgdal::readOGR(dsn = ".", layer = paste0(region, "_BLOB"), verbose = FALSE)
  setwd("../../../")
  shape <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
  names(shape) <- c("X", "Y")
  shape
}

surv <- c("HS", "QCS", "WCHG", "WCVI")
out <- lapply(surv, parse_shape)
names(out) <- surv
survey_grids <- out

usethis::use_data(survey_grids, internal = FALSE, overwrite = TRUE)

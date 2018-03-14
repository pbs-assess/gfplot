#' Join survey and commercial samples
#'
#' @param dat_comm TODO
#' @param dat_survey TODO
#' @export

# library(dplyr)
# dc <- readRDS("~/Dropbox/PBS synopsis/pbs-comm-samples.rds") %>%
#   filter(species_common_name == "redstripe rockfish")
# ds <- readRDS("~/Dropbox/PBS synopsis/pbs-survey-samples.rds") %>%
#   filter(species_common_name == "redstripe rockfish")
# join_samples(dc, ds)

join_samples <- function(dat_comm, dat_survey) {

  setdiff(names(dat_comm), names(dat_survey))
  setdiff(names(dat_survey), names(dat_comm))
  browser()

}

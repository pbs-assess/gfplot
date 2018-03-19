#' Join survey and commercial samples
#'
#' @param dat_comm TODO
#' @param dat_survey TODO
#' @export

bind_samples <- function(dat_comm, dat_survey) {
  dat_comm$data_source <- "commercial"
  dat_comm$data_source <- "survey"
  inter <- intersect(names(dat_comm), names(dat_survey))
  dplyr::bind_rows(
    dat_comm[, inter, drop = FALSE],
    dat_survey[, inter, drop = FALSE]
  )
}

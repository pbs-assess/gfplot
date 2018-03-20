#' Join survey and commercial samples
#'
#' @param dat_comm TODO
#' @param dat_survey TODO
#' @export
#' @examples
#' \dontrun{
#' dat_comm <- get_comm_samples("lingcod")
#' dat_survey <- get_survey_samples("lingcod")
#' dat <- bind_samples(dat_comm, dat_survey)
#' }

bind_samples <- function(dat_comm, dat_survey) {
  dat_comm$data_source <- "commercial"
  dat_comm$data_source <- "survey"
  inter <- intersect(names(dat_comm), names(dat_survey))
  dplyr::bind_rows(
    dat_comm[, inter, drop = FALSE],
    dat_survey[, inter, drop = FALSE]
  )
}
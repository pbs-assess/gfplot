#' Join survey and commercial samples
#'
#' @param dat_comm Output from [get_commercial_samples()]
#' @param dat_survey Output from [get_survey_samples()]
#' @export
#' @examples
#' \dontrun{
#' dat_comm <- get_commercial_samples("lingcod")
#' dat_survey <- get_survey_samples("lingcod")
#' dat <- bind_samples(dat_comm, dat_survey)
#' }

bind_samples <- function(dat_comm, dat_survey) {
  if ("survey_abbrev" %in% names(dat_comm))
    stop("Found `survey_abbrev` column in `dat_comm`. Did you reverse ",
      "the order of the arguments?")

  if ("keeper" %in% names(dat_survey))
    stop("Found `keeper` column in `dat_survey`. Did you reverse ",
      "the order of the arguments?")

  dat_comm$data_source <- "commercial"
  dat_survey$data_source <- "survey"
  inter <- intersect(names(dat_comm), names(dat_survey))
  dplyr::bind_rows(
    dat_comm[, inter, drop = FALSE],
    dat_survey[, inter, drop = FALSE]
  )
}

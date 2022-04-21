#' Join survey and commercial samples
#'
#' @param dat_comm Output from [gfdata::get_commercial_samples()]
#' @param dat_survey Output from [gfdata::get_survey_samples()]
#' @export
#' @examples
#' \dontrun{
#' dat_comm <- gfdata::get_commercial_samples("lingcod")
#' dat_survey <- gfdata::get_survey_samples("lingcod")
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

  dat_survey <- dat_survey %>%
    filter(maturity_code <= maturity_convention_maxvalue)

  inter <- intersect(names(dat_comm), names(dat_survey))
  dplyr::bind_rows(
    dat_comm[, inter, drop = FALSE],
    dat_survey[, inter, drop = FALSE]
  )
}

#' \pkg{pbssynopsis} package
#'
#' PBS Synopsis Report package
#'
#' See the README on
#' \href{https://github.com/seananderson/pbs-synopsis#readme}{GitHub}
#'
#' @docType package
#' @name pbssynopsis
#' @importFrom dplyr filter mutate summarise select group_by n arrange ungroup
#' @importFrom dplyr inner_join left_join right_join anti_join full_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "SURVEY_SERIES_TYPE_CODE", "trip_start_date", "SPECIES_CODE",
  "SPECIES_COMMON_NAME", "SPECIES_DESC", "SPECIES_SCIENCE_NAME",
  "SURVEY_SERIES_DESC", "SURVEY_SERIES_ID", "SURVEY_SERIES_TYPE_CODE",
  "discarded_kg", "discarded_pcs", "gear", "landed_kg",
  "landed_pcs", "species_common_name", "species_desc", "species_science_name",
  "trip_start_date", "year"))

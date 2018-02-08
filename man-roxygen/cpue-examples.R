#' @examples
#' \dontrun{
#' d <- get_cpue_index(gear = "bottom trawl")
#' walleye <- tidy_cpue_index(d, "walleye pollock",
#'   area_grep_pattern = "5[CDE]+")
#' m <- fit_cpue_index(walleye)
#'
#' plot_cpue_index_coefs(m)
#'
#' predict_cpue_index(m) %>%
#'   plot_cpue_index()
#'
#' jackknife_cpue_index(m)
#' }

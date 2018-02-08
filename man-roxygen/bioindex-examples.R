#' @family survey biomass index functions
#' @examples
#' \dontrun{
#' get_bioindex("lingcod") %>%
#'   tidy_bioindex() %>%
#'   plot_bioindex()
#'
#' # Or without pipes:
#' d <- get_bioindex("lingcod")
#' head(d)
#'
#' d_tidy <- tidy_bioindex(d)
#' head(d_tidy)
#'
#' plot_bioindex(d_tidy)
#' }

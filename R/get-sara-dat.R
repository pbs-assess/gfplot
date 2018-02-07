#' Get SARA and COSEWIC data
#'
#' @export
#' @family get PBS data functions
get_sara_dat <- function() {
  h <- xml2::read_html("http://www.registrelep-sararegistry.gc.ca/sar/index/default_e.cfm")
  d <- h %>% rvest::html_nodes("table") %>%
    .[[1]] %>%
    rvest::html_table() %>%
    .[-(1:2), ] %>%
    dplyr::as_tibble() %>%
    dplyr::filter(.data$Taxon %in% "Fishes") %>%
    dplyr::filter(!grepl("Salmon",  .data$`Common name *`))
  names(d) <- tolower(names(d))
  names(d) <- gsub(" ", "_", names(d))
  names(d) <- gsub("_\\*", "", names(d))
  d
}

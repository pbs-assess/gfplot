firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

round_nice <- function(x) {
  out <- plyr::round_any(x, 100)
  out[out == 0] <- x[out == 0]
  out[x == 0] <- ""
  out
}

#' Prepare PBS samples data for \code{\link{plot_samples}}
#'
#' @param path Path to cached data
#' @param year_range Either \code{NULL}, in which case all years are returned,
#'   or a numeric vector of length two giving the lower and upper years to
#'   include.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- prep_pbs_samples(year_range = c(1996, 2016))
#' }

prep_pbs_samples <- function(path = "data-cache",
  year_range = NULL) {
  dbio <- readRDS(file.path(path, "all-survey-bio.rds"))

  if (!is.null(year_range))
    dbio <- dbio[dbio$year >= year_range[[1]] & dbio$year <= year_range[[2]], ]

  dbio <- dbio[!duplicated(dbio$specimen_id), ] # critical!!

  dbio <- dbio %>%
    dplyr::select(.data$species_common_name, .data$year,
      .data$age, .data$length, .data$weight, .data$maturity_code)

  out <- dplyr::group_by(dbio,
    .data$species_common_name, .data$year) %>%
    dplyr::summarise(
      age = sum(!is.na(age) & age > 0),
      length = sum(!is.na(length) & length > 0),
      weight = sum(!is.na(weight) & weight > 0),
      maturity = sum(!is.na(maturity_code) & maturity_code > 0)
    ) %>% dplyr::ungroup()

  all_years <- expand.grid(year = seq(min(dbio$year), max(dbio$year), 1),
    species_common_name = unique(dbio$species_common_name),
    stringsAsFactors = FALSE)

  out <- dplyr::left_join(all_years, out, by = c("year", "species_common_name"))

  out <- reshape2::melt(out,
    id.vars = c("species_common_name", "year"),
    variable.name = "type", value.name = "n") %>%
    dplyr::as_tibble()
  out$n[is.na(out$n)] <- 0
  out
}


#' Plot sample availability
#'
#' @param dat A properly formatted data frame. For example, from
#'   \code{\link{prep_pbs_samples}}. The input data frame must have the columns
#'   (in any order): \code{year}, \code{type} (with the types of samples to
#'   plot, e.g. maturity, weight), and \code{n} (with the numbers of samples).
#'   The axis labels will be derived from \code{type} after capitalizing the
#'   first letter.
#'
#' @examples
#' d <- expand.grid(year = 1996:2016,
#'   type = c("maturity", "weight", "length", "age"), stringsAsFactors = FALSE)
#' d$n <- round(runif(nrow(d), 0, 800))
#' d$n[10] <- 0 # example zero
#' plot_samples(d)
#'
#' \dontrun{
#' x <- prep_pbs_samples(year_range = c(1996, 2016))
#' x <- dplyr::filter(x, species_common_name == "abyssal skate")
#' plot_samples(x)
#' }
#' @export

plot_samples <- function(dat) {

  dat$n_plot <- log(dat$n + 1)
  dat$n_text <- round_nice(dat$n)
  dat$type <- paste("#", firstup(as.character(dat$type)))

  ggplot(dat, aes_string("year", "type")) +
    ggplot2::geom_tile(aes_string(fill = "n_plot"), colour = "grey90", width = 1.5) +
    theme_pbs() +
    coord_cartesian(expand = FALSE, xlim = range(dat$year) + c(-0.75, 0.25)) +
    ggplot2::scale_fill_distiller(palette = "Greys",
      limits = c(log(1), max(dat$n_plot)), direction = 1) +
    ggplot2::scale_x_continuous(breaks = seq(min(dat$year), max(dat$year), 2)) +
    theme(axis.text.x = element_text(hjust = .75),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()) +
    ggplot2::guides(fill = FALSE) + xlab("") + ylab("") +
    geom_text(aes_string(x = "year - 0.25", label = "n_text"), colour = "white",
      size = 2.5, alpha = 0.8) +
    scale_y_discrete(position = "right")
}

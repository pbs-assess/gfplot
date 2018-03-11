#' Tidy PBS samples data for \code{\link{plot_sample_avail}}
#'
#' @param dat Input data frame from \code{\link{get_survey_samples}}.
#' @param year_range Either \code{NULL}, in which case all years are returned,
#'   or a numeric vector of length two giving the lower and upper years to
#'   include.
#' @param ageing_method TODO
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_survey_samples("lingcod")
#' tidy_sample_avail(d)
#'
#' d <- get_comm_samples("lingcod")
#' tidy_sample_avail(d)
#' }

tidy_sample_avail <- function(dat, year_range = NULL, ageing_method = c(3, 17)) {

  dat <- filter(dat, !is.na(year))

  if (!is.null(year_range))
    dat <- dat[dat$year >= min(year_range) &
        dat$year <= max(year_range), , drop = FALSE]

  dat <- dat[!duplicated(dat$specimen_id), ] # critical!!

  out <- group_by(dat, species_common_name, year) %>%
    summarise(
      age = sum(!is.na(age) & age > 0 &
          .data$ageing_method %in% ageing_method),
      length = sum(!is.na(length) & length > 0),
      weight = sum(!is.na(weight) & weight > 0),
      maturity = sum(!is.na(maturity_code) & maturity_code > 0)
    ) %>% ungroup()

  all_years <- expand.grid(year = seq(min(dat$year), max(dat$year), 1),
    species_common_name = unique(dat$species_common_name),
    stringsAsFactors = FALSE)

  out <- left_join(all_years, out, by = c("year", "species_common_name"))

  out <- reshape2::melt(out,
    id.vars = c("species_common_name", "year"),
    variable.name = "type", value.name = "n") %>%
    as_tibble()
  out$n[is.na(out$n)] <- 0
  out
}

#' Plot sample availability
#'
#' @param dat An input data frame from, for example,
#' \code{\link{tidy_sample_avail}}. The input data frame must have the columns:
#' \describe{
#'   \item{\code{year}}{The year.}
#'   \item{\code{type}}{The types of samples to plot, e.g. "maturity",
#'   "weight", "age". The axis labels will be derived from \code{type} after
#'   capitalizing the first letter.}
#'   \item{\code{n}}{The number of samples available for that sample type.}
#' }
#' @param year_range TODO
#' @param title A title for the plot. Use \code{title = ""} to omit.
#' @param palette TODO
#'
#' @examples
#' d <- expand.grid(year = 1996:2016,
#'   type = c("maturity", "weight", "length", "age"), stringsAsFactors = FALSE)
#' d$n <- round(runif(nrow(d), 0, 800))
#' d$n[10] <- 0 # example zero
#' plot_sample_avail(d)
#'
#' \dontrun{
#' d <- get_survey_samples("lingcod")
#' d <- tidy_sample_avail(d, year_range = c(1996, 2016))
#' plot_sample_avail(d, year_range = c(1996, 2016),
#'   title = "Survey samples")
#'
#' d <- get_comm_samples("lingcod")
#' d <- tidy_sample_avail(d, year_range = c(1996, 2016))
#' plot_sample_avail(d, year_range = c(1996, 2016),
#'   title = "Commercial samples")
#' }
#' @export

plot_sample_avail <- function(dat, year_range = NULL, title = "Biological samples",
  palette = "Greys") {

  dat$n_plot <- log(dat$n + 1)
  dat$n_text <- round_nice(dat$n)
  dat$type <- paste("#", firstup(as.character(dat$type)))

  year_min <- min(dat$year, na.rm = TRUE)
  year_max <- max(dat$year, na.rm = TRUE)
  if (is.null(year_range))
    year_range <- c(year_min, year_max)

  all <- expand.grid(
    type = unique(dat$type),
    year = seq(year_range[1], year_range[2]),
    stringsAsFactors = FALSE)
  dat <- full_join(dat, all, by = c("type", "year"))
  dat$n_plot[is.na(dat$n_plot)] <- 0

  ggplot(dat, aes_string("year", "type")) +
    ggplot2::geom_tile(aes_string(fill = "n_plot"), colour = "grey90") +
    theme_pbs() +
    coord_cartesian(expand = FALSE, xlim = year_range + c(-0.5, 0.5)) +
    ggplot2::scale_fill_distiller(palette = palette,
      limits = c(log(1), max(dat$n_plot)), direction = 1) +
    ggplot2::scale_x_continuous(
      breaks = seq(round_down_even(year_range[1]), year_range[2], 2)) +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()) +
    ggplot2::guides(fill = FALSE) + xlab("") + ylab("") +
    geom_text(aes_string(x = "year", label = "n_text"), colour = "white",
      size = 1.75, alpha = 0.85) +
    ggplot2::scale_y_discrete(position = "left") +
    ggplot2::ggtitle(title)
}

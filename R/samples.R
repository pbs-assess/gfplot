#' Tidy PBS samples data for [plot_sample_avail()]
#'
#' @param dat Input data frame from [gfdata::get_survey_samples()].
#' @param year_range Either `NULL`, in which case all years are returned,
#'   or a numeric vector of length two giving the lower and upper years to
#'   include.
#' @param ageing_method_codes A numeric vector of ageing method codes to filter
#'   on. Default to `NULL`, which brings in all valid ageing codes.
#'   See [gfdata::get_age_methods()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- gfdata::get_survey_samples("lingcod")
#' tidy_sample_avail(d)
#'
#' d <- gfdata::get_commercial_samples("lingcod")
#' tidy_sample_avail(d)
#' }

tidy_sample_avail <- function(dat, year_range = NULL,
                              ageing_method_codes = NULL) {
  dat <- filter(dat, !is.na(year))

  if (!is.null(year_range)) {
    dat <- dat[dat$year >= min(year_range) &
      dat$year <= max(year_range), , drop = FALSE]
  }

  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical!

  if (!is.null(ageing_method_codes)) {
    dat <- filter(dat, ageing_method %in% ageing_method_codes)
  }

  if (!"age_specimen_collected" %in% names(dat))
    dat <- mutate(dat, age_specimen_collected = NA)
  out <- group_by(dat, species_common_name, year) %>%
    summarise(
      age = sum(!is.na(age) & age > 0),
      ageing_structure = sum(!is.na(.data$age_specimen_collected) &
          .data$age_specimen_collected == 1),
      length = sum(!is.na(length) & length > 0),
      weight = sum(!is.na(weight) & weight > 0),
      maturity = sum(!is.na(maturity_code) & maturity_code > 0)
    ) %>%
    ungroup()

  year_range <- if (is.null(year_range)) range(dat$year) else year_range
  all_years <- expand.grid(
    year = seq(min(year_range), max(year_range), 1),
    stringsAsFactors = FALSE
  )

  out <- left_join(all_years, out, by = c("year"))

  out <- reshape2::melt(out,
    id.vars = c("species_common_name", "year"),
    variable.name = "type", value.name = "n"
  ) %>%
    as_tibble()
  out$n <- as.numeric(out$n)
  out$n[is.na(out$n)] <- 0
  out
}

#' Plot sample availability
#'
#' @param dat An input data frame from, for example,
#' [tidy_sample_avail()]. The input data frame must have the columns:
#' \describe{
#'   \item{\code{year}}{The year.}
#'   \item{\code{type}}{The types of samples to plot, e.g. "maturity",
#'   "weight", "age". The axis labels will be derived from \code{type} after
#'   capitalizing the first letter.}
#'   \item{\code{n}}{The number of samples available for that sample type.}
#' }
#' @param year_range An optional year range.
#' @param title A title for the plot.
#' @param palette A palette to pass to [ggplot2::scale_fill_distiller()].
#' @param trans A function to transform the counts before applying the color
#'   scale. E.g. `sqrt` or `log` or `I` if you don't want any transformation.
#' @param french Logical.
#' @param text_colour Colour for the value text labels
#'
#' @examples
#' set.seed(1)
#' d <- expand.grid(year = 1996:2016,
#'   type = c("maturity", "weight", "length", "age"), stringsAsFactors = FALSE)
#' d$n <- round(runif(nrow(d), 0, 800))
#' d$n[10] <- 0 # example zero
#' plot_sample_avail(d)
#'
#' \dontrun{
#' d <- gfdata::get_survey_samples("lingcod")
#' d <- tidy_sample_avail(d, year_range = c(1996, 2016))
#' plot_sample_avail(d, year_range = c(1996, 2016),
#'   title = "Survey samples")
#'
#' d <- gfdata::get_commercial_samples("lingcod")
#' d <- tidy_sample_avail(d, year_range = c(1996, 2016))
#' plot_sample_avail(d, year_range = c(1996, 2016),
#'   title = "Commercial samples")
#' }
#' @export

plot_sample_avail <- function(dat, year_range = NULL, title = "Biological samples",
                              palette = "Greys", trans = sqrt, french = FALSE, text_colour = "white") {
  dat$n_plot <- trans(dat$n)
  dat$n_text <- round_nice(dat$n)
  dat$type <- firstup(as.character(gsub("_", " ", dat$type)))
  dat <- dat %>% mutate(type = gsub("Ageing structure", "Structures", type))
  dat$type <- paste("#", dat$type, sep = " ")

  year_min <- min(dat$year, na.rm = TRUE)
  year_max <- max(dat$year, na.rm = TRUE)
  if (is.null(year_range)) {
    year_range <- c(year_min, year_max)
  }

  all <- expand.grid(
    type = unique(dat$type),
    year = seq(year_range[1], year_range[2]),
    stringsAsFactors = FALSE
  )
  dat <- full_join(dat, all, by = c("type", "year"))
  dat$n_plot[is.na(dat$n_plot)] <- 0
  dat$n_plot[dat$n_plot == 0] <- NA
  dat <- filter(dat, year >= min(year_range), year <= max(year_range))

  dat <- dat %>%
    mutate(type = factor(type,
      levels = rev(c(
        "# Length",
        "# Weight",
        "# Maturity",
        "# Age",
        "# Structures"
      ))))

  if (french){
    levels(dat$type)[levels(dat$type)=="# Length"] <- paste("#", en2fr("Length"))
    levels(dat$type)[levels(dat$type)=="# Weight"] <- paste("#", en2fr("Weight"))
    levels(dat$type)[levels(dat$type)=="# Maturity"] <- paste("#", en2fr("Maturity"))
    levels(dat$type)[levels(dat$type)=="# Age"] <- paste("#", en2fr("Age"))
    levels(dat$type)[levels(dat$type)=="# Structures"] <- paste("#", "Structure")
  }

  ggplot(dat, aes_string("year", "type")) +
    ggplot2::geom_tile(aes_string(fill = "n_plot"), colour = "grey90") +
    theme_pbs() +
    coord_cartesian(expand = FALSE, xlim = year_range + c(-0.5, 0.5)) +
    ggplot2::scale_fill_distiller(
      palette = palette,
      limits = c(0, max(dat$n_plot)), direction = 1
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(round_down_even(year_range[1]), year_range[2], 2)
    ) +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ggplot2::guides(fill = "none") + xlab("") + ylab("") +
    geom_text(aes_string(x = "year", label = "n_text"),
      colour = text_colour,
      size = 2.1, alpha = 1
    ) +
    ggplot2::scale_y_discrete(position = "left") +
    ggplot2::ggtitle(title)
}

#' Plot age frequency data
#'
#' Functions for plotting age frequency data.
#'
#' @details
#'
#' * [tidy_ages_raw()] or [tidy_ages_weighted()] prepare PBS data for `plot_ages()`.
#'   These work across one or multiple species.
#' * `plot_ages()` Plots age frequencies for each year for selected surveys for
#' a single species. Input data frame should come from [tidy_ages_raw()] or
#' [tidy_ages_weighted()] or follow the following format: The input data frame
#' must have the columns (in any order): `survey`, `year`, `sex` (coded as `"M"`
#' and `"F"`), `age`, `proportion`, `total` (for the total sample number label).
#' @param dat Input data frame. Should be from [tidy_ages_raw()] or
#'   [tidy_ages_weighted()] or be formatted similarly. See details.
#' @param max_size Maximum dot size (passed to [ggplot2::scale_size_area()]).
#' @param sex_gap Horizontal gap between male and female bubbles.
#' @param year_increment Increment between year labels on x axis.
#' @param ylab Y axis label.
#' @param year_range If not `NULL`, a the range of years to plot. Defaults to
#'   all years included in original data.
#' @param line_col A named character vector of colors for male and females.
#' @param survey_cols If not `NULL`, a named character vector for different
#'   colors for the various surveys.
#' @param alpha Transparency for the fill color.
#' @param grid_col Colour for the gridlines.
#' @param diagonal_lines A numeric a vector of years to start diagonal lines at
#'   to help trace cohorts. Note that these are passed to
#'   [ggplot2::geom_abline()] as intercepts.
#'
#' @family age- and length-frequency functions
#'
#' @examples
#' \dontrun{
#' # main age/length data:
#' rs_comm_samples <- get_comm_samples("redstripe rockfish")
#' rs_survey_samples <- get_survey_samples("redstripe rockfish")
#' #
#' # for weighting:
#' rs_catch <- get_catch("redstripe rockfish")
#' rs_survey_sets <- get_survey_sets("redstripe rockfish")
#'
#' # survey raw age frequencies:
#' tidy_ages_raw(rs_survey_samples,
#'   sample_type = "survey") %>%
#'   plot_ages()
#'
#' # survey weighted age frequencies:
#' tidy_ages_weighted(rs_survey_samples,
#'   sample_type = "survey",
#'   dat_survey_sets = rs_survey_sets) %>%
#'   plot_ages()
#'
#' # commercial raw age frequencies:
#' tidy_ages_raw(rs_comm_samples,
#'   sample_type = "commercial") %>%
#'   plot_ages()
#' }

#' @export
plot_ages <- function(dat, max_size = 5, sex_gap = 0.2, year_increment = 2,
                      ylab = "Age (years)", year_range = NULL,
                      line_col = c("M" = "#666666", "F" = "#f44256"),
                      survey_cols = NULL, alpha = 0.2, grid_col = "grey95",
                      diagonal_lines = seq(-2100, -1850, 10)) {
  if (nrow(dat) > 0) {
    age_max <- max(dat$age, na.rm = TRUE)
  } else {
    age_max <- 1
  }

  dat <- dat %>%
    mutate(year_jitter = ifelse(sex == "M",
      year - sex_gap / 2, year + sex_gap / 2
    )) %>%
    group_by(year, year_jitter, survey_abbrev) %>%
    ungroup()

  counts <- select(dat, total, year, survey_abbrev) %>% unique()

  age_range <- diff(range(dat$age, na.rm = TRUE))

  if (!is.null(survey_cols)) {
    survey_col_names <- names(survey_cols)
    col <- stats::setNames(survey_cols, paste("F", survey_col_names))
    col <- c(col, stats::setNames(
      rep("#888888", length(col)),
      paste("M", survey_col_names)
    ))
    fill_col <- rep("#FFFFFF10", length(col))
    line_col <- col
    dat$sex <- paste(dat$sex, dat$survey_abbrev)
  } else {
    fill_col <- paste0(substr(line_col, 1L, 7L), as.character(alpha * 100))
  }

  if (is.null(year_range)) {
    year_range <- c(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))
  }

  dat <- full_join(dat, tibble(survey_abbrev = factor(levels(dat$survey_abbrev),
    levels = levels(dat$survey_abbrev)
  )), by = "survey_abbrev")

  # empty plot:
  if (sum(!is.na(dat$age)) == 0) {
    dat$age <- 0
    age_range <- 1
  }

  dat$sex <- factor(dat$sex, levels = rev(sort(unique(dat$sex)))) # to get F bubbles shaded on top

  dat <- arrange(dat, year_jitter, survey_abbrev, sex)

  g <- ggplot(dat, aes_string("year_jitter", "age")) +
    facet_wrap(~survey_abbrev, nrow = 1) +
    scale_x_continuous(
      breaks =
        seq(round_down_even(min(year_range)), max(year_range), year_increment)
    ) +
    xlab("") +
    ylab(ylab) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(title = "Age frequencies", colour = "Sex", fill = "Sex") +
    geom_vline(
      xintercept = seq(year_range[1], year_range[2], 1),
      col = grid_col, lwd = 0.4
    ) +
    geom_hline(
      yintercept = seq(0, age_max, 10), col = grid_col,
      lwd = 0.4
    ) +
    coord_cartesian(
      xlim = year_range + c(-0.8 - sex_gap / 2, 0.8 + sex_gap / 2),
      ylim = c(0, age_max + 0.02 * age_range), expand = FALSE
    )

  if (sum(dat$age > 0, na.rm = TRUE) > 0) {
    g <- g +
      ggplot2::geom_abline(
        intercept = diagonal_lines, slope = 1,
        colour = grid_col
      ) +
      scale_fill_manual(values = fill_col, breaks = c("M", "F")) +
      scale_colour_manual(values = line_col, breaks = c("M", "F")) +
      scale_size_area(max_size = max_size) +
      guides(
        size = FALSE, colour = guide_legend(override.aes = list(size = 3.5))
      ) +
      geom_text(
        data = counts, y = age_max + 0.005 * age_range,
        aes_string(x = "year", label = "total"),
        inherit.aes = FALSE, colour = "grey50", size = 2.25, hjust = 1,
        angle = 90
      ) +
      geom_point(aes_string(
        size = "proportion", group = "sex", fill = "sex",
        colour = "sex"
      ), pch = 21)
  }

  if (!is.null(survey_cols)) {
    g <- g + guides(fill = FALSE, colour = FALSE)
  }

  g
}

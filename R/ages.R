#' Plot age frequency data
#'
#' Functions for plotting age frequency data.
#'
#' @details
#'
#' * [tidy_ages_raw()] or [tidy_ages_weighted()] prepare PBS data for `plot_ages()`. These work across one
#'   or multiple species.
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
#' @param fill_col TODO
#' @param line_col TODO
#' @param survey_cols TODO
#' @param alpha TODO
#'
#' @family age- and length-frequency functions
#'
#' @examples
#' \dontrun{
#' # # main age/length data:
#' # rs_comm_samples <- get_comm_samples("redstripe rockfish",
#' #   discard_keepers = TRUE)
#' # rs_survey_samples <- get_survey_samples("redstripe rockfish")
#' #
#' # # for weighting:
#' # rs_catch <- get_catch("redstripe rockfish")
#' # rs_survey_sets <- get_survey_sets("redstripe rockfish")
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
                      fill_col = c("M" = "grey50", "F" = "#f44256"),
                      line_col = c("M" = "grey50", "F" = "#f44256"),
                      survey_cols = NULL, alpha = 0.85) {

  if (nrow(dat) > 0) {
    age_max <- max(dat$age, na.rm = TRUE)
  } else {
    age_max <- 1
  }

  dat <- dat %>%
    mutate(year_jitter = ifelse(sex == "M",
      year + sex_gap / 2, year - sex_gap / 2
    )) %>%
    group_by(year, year_jitter, survey) %>%
    mutate(n_scaled = proportion / max(proportion)) %>%
    ungroup()

  counts <- select(dat, total, year, survey) %>% unique()

  age_range <- diff(range(dat$age, na.rm = TRUE))

  if (!is.null(survey_cols)) {
    survey_col_names <- names(survey_cols)
    col <- stats::setNames(survey_cols, paste("F", survey_col_names))
    col <- c(col, stats::setNames(
      rep("#888888", length(col)),
      paste("M", survey_col_names)
    ))
    fill_col <- paste0(substr(col, 1L, 7L), as.character(alpha * 100))
    line_col <- col
    dat$sex <- paste(dat$sex, dat$survey)
  }

  if (is.null(year_range)) {
    year_range <- c(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))
  }

  # dat <- full_join(dat, tibble(survey = factor(levels(dat$survey),
  #   levels = levels(dat$survey)
  # )), by = "survey")

  if (sum(!is.na(dat$age)) == 0) {
    dat$age <- 0
    age_range <- 1
  }

  g <- ggplot(dat, aes_string("year_jitter", "age")) +
    facet_wrap(~ survey, nrow = 1) +
    scale_x_continuous(
      breaks =
        seq(round_down_even(year_range[1]), year_range[2], year_increment)
    ) +
    xlab("") +
    ylab(ylab) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(title = "Age frequencies", colour = "Sex", fill = "Sex") +
    geom_vline(
      xintercept = seq(year_range[1], year_range[2], 1),
      col = "grey95", lwd = 0.4
    ) +
    geom_hline(
      yintercept = seq(0, age_max, 10), col = "grey95",
      lwd = 0.4
    ) +
    coord_cartesian(
      xlim = year_range + c(-0.8 - sex_gap / 2, 0.8 + sex_gap / 2),
      ylim = c(0, age_max + 0.02 * age_range), expand = FALSE
    )

  if (sum(dat$age > 0, na.rm = TRUE) > 0) {
    g <- g +
      scale_fill_manual(values = fill_col, breaks = c("M", "F")) +
      scale_colour_manual(values = line_col, breaks = c("M", "F")) +
      scale_size_area(max_size = max_size) +
      guides(
        size = FALSE, colour = guide_legend(override.aes = list(size = 3.5)),
        fill = guide_legend(override.aes = list(size = 3.5))
      ) +
      geom_text(
        data = counts, y = age_max + 0.005 * age_range,
        aes_string(x = "year", label = "total"),
        inherit.aes = FALSE, colour = "grey50", size = 2.25, hjust = 1,
        angle = 90
      ) +
      geom_point(aes_string(size = "n_scaled", group = "sex", colour = "sex"),
        pch = 21, alpha = 0.9
      )
  }

  if (!is.null(survey_cols)) {
    g <- g + guides(fill = FALSE, colour = FALSE)
  }

  g
}

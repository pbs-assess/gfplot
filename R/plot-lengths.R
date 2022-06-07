#' Plot length frequency data
#'
#' Functions for plotting length frequency data.
#'
#' @details
#' * [tidy_lengths_raw()] or [tidy_lengths_weighted()] prepares PBS data for
#' `plot_lengths()`. Works across one or multiple species.
#' * `plot_lengths()` Plots length frequencies for each year for selected
#' surveys for a single species.
#'
#' @param dat A data frame from [tidy_lengths_raw()] or [tidy_lengths_weighted()]
#'   or a similarly formatted data frame. Must have columns named `survey`,
#'   `year`, `sex` (`"M"` and `"F"`), `length_bin`, `proportion`, `total` (for
#'   the total number of samples for that year and survey/commercial
#'   combination).
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param fill_col Fill colours for histograms. A named vector with names
#' `"F"` and `"M"` for female and male.
#' @param line_col Line colours for histograms. A named vector with names
#' `"F"` and `"M"` for female and male.
#' @param survey_cols An optional named character vector of colors for the
#'   various surveys.
#' @param alpha Transparency for the fill color in the histograms.
#' @param bin_size Bin size. Should match the bin size used with the `tidy_*()`
#'   function.
#' @param min_total Minimum number of fish for a given survey and year needed
#'   before a histogram is shown.
#' @param show_year Which years to show a label for. Can be one of 'even' or 'odd'. Any
#'   other value will enable labels for all years.
#' @param french French
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # # main age/length data:
#' # rs_comm_samples <- gfdata::get_commercial_samples("redstripe rockfish")
#' # rs_survey_samples <- gfdata::get_survey_samples("redstripe rockfish")
#' #
#' # # for weighting:
#' # rs_catch <- gfdata::get_catch("redstripe rockfish")
#' # rs_survey_sets <- gfdata::get_survey_sets("redstripe rockfish")
#'
#' # survey raw length frequencies:
#' tidy_lengths_raw(rs_survey_samples,
#'   sample_type = "survey",
#'   bin_size = 2) %>%
#'   plot_lengths()
#'
#' # survey weighted length frequencies:
#' tidy_lengths_weighted(rs_survey_samples,
#'   sample_type = "survey",
#'   bin_size = 2,
#'   dat_survey_sets = rs_survey_sets) %>%
#'   plot_lengths()
#'
#' # commercial raw length frequencies:
#' tidy_lengths_raw(rs_comm_samples,
#'   sample_type = "commercial",
#'   bin_size = 2) %>%
#'   plot_lengths()
#' }

plot_lengths <- function(dat, xlab = "Length (cm)",
                         ylab = "Relative length frequency",
                         fill_col = c("M" = "grey80", "F" = "#FF000010"),
                         line_col = c("M" = "grey40", "F" = "red"),
                         survey_cols = NULL, alpha = 0.24,
                         bin_size = 2, min_total = 20,
                         show_year = "even",
                         french = FALSE) {

  if (!is.null(survey_cols)) {
    survey_col_names <- names(survey_cols)
    col <- stats::setNames(survey_cols, paste("F", survey_col_names))
    col <- c(col, stats::setNames(
      rep("#888888", length(col)),
      paste("M", survey_col_names)
    ))
    fill_col <- paste0(substr(col, 1L, 7L), as.character(alpha * 100))
    names(fill_col) <- c(survey_col_names, survey_col_names)
    line_col <- col
    names(fill_col) <- names(line_col)
    dat$sex <- paste(dat$sex, dat$survey_abbrev)
  }

  x_breaks <- pretty(dat$length_bin, 4L)
  N <- length(x_breaks)
  x_breaks <- x_breaks[seq(1, N - 1)]
  range_lengths <- diff(range(dat$length_bin, na.rm = TRUE))
  counts <- select(dat, survey_abbrev, year, total) %>% unique()

  format_french_1000s <- function(x) {
    format(as.numeric(x), big.mark = " ", scientific = FALSE, trim = TRUE)
  }
  if (french) counts$total <- format_french_1000s(counts$total)

  # make max value 1.0 each year-survey combo for plotting:
  dat <- group_by(dat, year, survey_abbrev) %>%
    mutate(proportion = proportion / max(proportion)) %>%
    ungroup()

  dat$sex <- factor(dat$sex, levels = rev(sort(unique(dat$sex)))) # to get F bars shaded on top
  dat <- arrange(dat, year, survey_abbrev, sex)

  dat <- mutate(dat, proportion = ifelse(total >= min_total, proportion, NA))

  g <- ggplot(dat, aes_string("length_bin", "proportion")) +
    geom_col(
      width = bin_size,
      aes_string(colour = "sex", fill = "sex"), size = 0.3,
      position = position_identity()
    ) +
    theme_pbs() +
    scale_fill_manual(values = fill_col, breaks = c("M", "F")) +
    scale_colour_manual(values = line_col, breaks = c("M", "F")) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    xlab(en2fr(xlab, french)) + ylab(en2fr(ylab, french)) +
    ylim(-0.04, 1.07) +
    theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(colour = en2fr("Sex", french), fill = en2fr("Sex", french)) +
    geom_text(
      data = counts,
      x = min(dat$length_bin, na.rm = TRUE) + 0.02 * range_lengths,
      y = 0.85, aes_string(label = "total"),
      inherit.aes = FALSE, colour = "grey50", size = 2.25, hjust = 0
    ) +
    labs(title = en2fr("Length frequencies", french)) +
    theme(panel.grid.major.x = ggplot2::element_line(colour = "grey93"))

  if(show_year == "even"){
    g <- g +
      facet_grid(forcats::fct_rev(as.factor(year)) ~ survey_abbrev,
                 labeller = ggplot2::labeller(.rows = is_even), drop = FALSE)
  }else if(show_year == "odd"){
    g <- g +
      facet_grid(forcats::fct_rev(as.factor(year)) ~ survey_abbrev,
                 labeller = ggplot2::labeller(.rows = is_odd), drop = FALSE)
  }else{
    g <- g +
      facet_grid(forcats::fct_rev(as.factor(year)) ~ survey_abbrev,
                 labeller = ggplot2::labeller(.rows = is_all), drop = FALSE)
  }
  if (!is.null(survey_cols)) {
    g <- g + guides(fill = "none", colour = "none")
  }

  g
}

is_even <- function(x) {
  ie <- as.numeric(as.character(x)) %% 2 == 0
  ifelse(ie, x, "")
}

is_odd <- function(x) {
  ie <- as.numeric(as.character(x)) %% 2 == 1
  ifelse(ie, x, "")
}

is_all <- function(x) {
  x
}

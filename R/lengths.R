#' Plot length frequency data
#'
#' Functions for plotting length frequency data. (Weighting not implimented
#' yet. TODO)
#'
#' @details
#'
#' * `tidy_lengths_raw()` Prepares PBS data for `plot_lengths()`. Works across one
#' or multiple species.
#' * `plot_lengths()` Plots length frequencies for each year for selected
#' surveys for a single species.
#'
#' @param dat Input data frame. For `tidy_lengths_raw()` should be from
#' [get_survey_samples()] and or [get_comm_samples()]. For `plot_lengths()` should
#' be from `tidy_length()` or formatted similarly. See details TODO.
#' @param bin_size Bin size.
#' @param min_specimens Minimum number of specimens for histogram data to be
#' computed/shown.
#' @param survey_series_desc A character vector of survey series to include.
#' @param surveys A character vector of shorter/cleaner survey names to use in
#' the same order as `survey_series_desc`. These are used in the plot.
#'
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param fill_col Fill colours for histograms. A named vector with names
#' `"F"` and `"M"` for female and male.
#' @param line_col Line colours for histograms. A named vector with names
#' `"F"` and `"M"` for female and male.
#' @param year_lim TODO
#'
#' @family age- and length-frequency functions
#' @name plot_lengths
NULL

#' @rdname plot_lengths
#' @export
tidy_lengths_raw <- function(dat, bin_size = 2,
  min_specimens = 20L,
  survey_series_desc = c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey",
    "PHMA Rockfish Longline Survey - Outside North",
    "PHMA Rockfish Longline Survey - Outside South",
    "IPHC Longline Survey"),
  surveys = c("WCHG",
    "HS",
    "QCS",
    "WCVI",
    "PHMA LL (N)",
    "PHMA LL (S)",
    "IPHC"),
  year_lim = c(1996, Inf)) {

  dat <- filter(dat, !is.na(survey_series_desc))
  dat <- filter(dat, survey_series_desc %in% survey_series_desc)
  dat <- filter(dat, year >= year_lim[[1]] & year <= year_lim[[2]])
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical!!
  dat <- dat %>%
    select(species_common_name, .data$species_science_name, .data$sample_id,
      .data$year, .data$age, .data$length, .data$weight, .data$maturity_code,
      .data$sex, .data$survey_series_desc)

  dat <- filter(dat, !is.na(length), !is.na(sex))

  su <- dplyr::tibble(survey = surveys,
    survey_series_desc = survey_series_desc)
  all <- expand.grid(survey = surveys, year = seq(min(dat$year), max(dat$year)),
    stringsAsFactors = FALSE)

  surv_year_counts <- dat %>%
    inner_join(su, by = "survey_series_desc") %>%
    group_by(.data$year, .data$survey) %>%
    summarise(total = n(), max_length = max(.data$length)) %>%
    ungroup() %>%
    full_join(all, by = c("year", "survey"))

  counts <- select(surv_year_counts, .data$total, .data$year,
    .data$survey, .data$max_length) %>% unique()

  max_length <- filter(counts, .data$total >= min_specimens) %>%
    dplyr::pull(.data$max_length) %>% max()

  lengths <- seq(0, max_length + 0.1, bin_size)

  out <- dat %>%
    inner_join(su, by = "survey_series_desc") %>%
    mutate(length_bin = lengths[findInterval(.data$length, lengths)]) %>%
    group_by(.data$year, .data$survey, .data$length_bin, .data$sex) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(.data$year, .data$survey) %>%
    mutate(max_n = max(.data$n)) %>%
    ungroup() %>%
    mutate(proportion = .data$n / .data$max_n) %>%
    mutate(sex = ifelse(.data$sex == 1, "M", "F")) %>%
    full_join(all, by = c("year", "survey")) %>%
    left_join(counts, by = c("year", "survey")) %>%
    mutate(proportion = ifelse(total >= min_specimens, proportion, NA)) %>%
    mutate(bin_size = bin_size)
  out <- out[!is.na(out$survey), ]

  list(data = out, counts = counts, surveys = su)
}

#' ggplot2-like colour scale in HCL space
#'
#' @param n Number of colours to return.
#' @param hue_min Minimum hue value in the range [0,360]
#' @param hue_max Maximum hue value in the range [0,360]
#' @param l Luminance in the range [0,100]
#' @param c Chroma of the colour.
#' @details See the [grDevices::hcl()] function for details.
#' @export
#' @examples
#' gg_color_hue(10)
#' plot(1:6, col = gg_color_hue(6), pch = 20, cex = 3)
gg_color_hue <- function(n, hue_min = 8, hue_max = 290, l = 52, c = 100) {
  hues <- seq(hue_min, hue_max, length = n + 1)
  grDevices::hcl(h = hues, l = l, c = c)[seq_len(n)]
}

#' @rdname plot_lengths
#' @export
plot_lengths <- function(dat, xlab = "Length (cm)",
  ylab = "Relative length frequency",
  fill_col = c("M" = "grey80", "F" = "#FF000010"),
  line_col = c("M" = "grey40", "F" = "red"),
  survey_cols = NULL, alpha = 0.24) {

  dat$data$sex[is.na(dat$data$sex)] <- "F" # for legend only; avoid "NAs"

  if (!is.null(survey_cols)) {
    survey_col_names <- names(survey_cols)
    col <- setNames(survey_cols, paste("F", survey_col_names))
    col <- c(col, setNames(rep("#888888", length(col)),
      paste("M", survey_col_names)))
    fill_col <- paste0(substr(col, 1L, 7L), as.character(alpha * 100))
    line_col <- col
    dat$data$sex <- paste(dat$data$sex, dat$data$survey)
  }

  x_breaks <- pretty(dat$data$length_bin, 4L)
  N <- length(x_breaks)
  x_breaks <- x_breaks[seq(1, N - 1)]
  range_lengths <- diff(range(dat$data$length_bin, na.rm = TRUE))

  g <- ggplot(dat$data, aes_string("length_bin", "proportion")) +
    geom_col(width = unique(dat$bin_size),
      aes_string(colour = "sex", fill = "sex"), size = 0.3,
      position = position_identity()) +
    facet_grid(
      forcats::fct_rev(as.character(year)) ~
        forcats::fct_relevel(survey, dat$survey$survey)) +
    theme_pbs() +
    scale_fill_manual(values = fill_col) +
    scale_colour_manual(values = line_col) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    xlab(xlab) + ylab(ylab) +
    ylim(-0.04, 1.07) +
    theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y =ggplot2::element_blank()) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(colour = "Sex", fill = "Sex") +
    geom_text(data = dat$counts,
      x = min(dat$data$length_bin, na.rm = TRUE) + 0.02 * range_lengths,
      y = 0.85, aes_string(label = "total"),
      inherit.aes = FALSE, colour = "grey50", size = 2.25, hjust = 0) +
    labs(title = "Length frequencies") +
    theme(panel.grid.major.x = ggplot2::element_line(colour = "grey93"))

  if (!is.null(survey_cols))
    g <- g + guides(fill = FALSE, colour = FALSE)

  g
}

#' Plot maturity frequency by month
#'
#' @name plot_maturity_months
NULL

#' @param months For [tidy_maturity_months()], data from [get_survey_samples()]
#'   or [get_comm_samples()] or [bind_samples()]. For [plot_maturity_months()],
#'   data from [tidy_maturity_months()].
#' @param months A vector of months to include. Defaults to all.
#' @export
#' @rdname plot_maturity_months
tidy_maturity_months <- function(dat, months = seq(1, 12)) {
  dat <- mutate(dat, month = lubridate::month(trip_start_date))
  dat <- filter(dat, month %in% months)
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical!
  dat <- dat %>%
    select(
      species_common_name,
      month,
      maturity_convention_code,
      maturity_code,
      maturity_name,
      sex
    )

  dat <- filter(dat, !is.na(maturity_name), !is.na(sex))

  file <- system.file("extdata", "maturity_short_names.csv",
    package = "gfplot"
  )

  mat_df <- readr::read_csv(file,
    col_types = readr::cols(
      maturity_convention_code = readr::col_integer(),
      maturity_convention_desc = readr::col_character(),
      `Maturity Convention Max Value` = readr::col_integer(),
      maturity_code = readr::col_integer(),
      sex = readr::col_integer(),
      maturity_name = readr::col_character(),
      maturity_desc = readr::col_character(),
      mature_at = readr::col_integer(),
      maturity_name_short = readr::col_character()
    )
  )

  dat <- left_join(dat,
    select(mat_df, sex, maturity_convention_code, maturity_code, maturity_name_short),
    by = c("sex", "maturity_convention_code", "maturity_code")
  )

  dat <- filter(dat, sex %in% c(1L, 2L))
  dat <- dat %>% mutate(sex = ifelse(sex == 2L, "F", "M"))

  mat_levels <- rev(c(
    "Immature",
    "Maturing",
    "Maturing-Small",
    "Maturing-Large",
    "Pre-Ripening (Mature)",
    "Mature",
    "Ripening (Mature)",
    "Gravid",
    "Fertilized",
    "Ripe",
    "Running Ripe",
    "Embryos",
    "Spent",
    "Resorbing",
    "Recovering",
    "Resting"
  ))

  mat_levels <- mat_levels[mat_levels %in% unique(dat$maturity_name_short)]
  dat$maturity_name_short <- factor(dat$maturity_name_short,
    levels = mat_levels
  )

  dat <- select(
    dat, species_common_name, month,
    maturity_name_short, sex
  ) %>%
    rename(maturity = maturity_name_short) %>%
    ungroup()

  dat
}

#' @param max_size  The maximum size of the circles.
#' @param sex_gap  A gap to separate the male and female circles.
#' @param fill_col  The fill color of the circles.
#' @param line_col  The line color of the circles.
#' @param alpha  The transparency of the circles.
#' @param title  A title to add to the plot.
#' @param n_label_pos A numeric vector of length 2 that gives the y position of
#'   the text describing the number of male and female samples within each month
#'   bin.
#'
#' @export
#' @rdname plot_maturity_months
#' @examples
#' # pop_samples <- get_survey_samples("pacific ocean perch")
#' tidy_maturity_months(pop_samples) %>%
#'   plot_maturity_months()
#' \dontrun{
#' d <- get_survey_samples("lingcod")
#' tidy_maturity_months(d) %>%
#'   plot_maturity_months()
#' }
plot_maturity_months <- function(dat,
                                 max_size = 11,
                                 sex_gap = 0.2,
                                 fill_col = c("M" = "grey70", "F" = "black"),
                                 line_col = c("M" = "grey70", "F" = "black"),
                                 alpha = 0.8,
                                 title = "Maturity frequencies",
                                 n_label_pos = c(0.7, 1)) {
  dat <- dat %>%
    filter(!is.na(maturity)) %>%
    mutate(
      month_jitter =
        ifelse(sex == "M", month + sex_gap / 2, month - sex_gap / 2)
    ) %>%
    group_by(sex, month, month_jitter, maturity) %>%
    summarise(.n = n()) %>%
    ungroup() %>%
    mutate(n_scaled = .n / max(.n)) %>%
    group_by(month, sex) %>%
    mutate(total_month = sum(.n)) %>%
    ungroup()

  counts <- select(dat, sex, total_month, month_jitter) %>% unique()
  counts <- mutate(counts, y = ifelse(sex == "F",
    max(as.numeric(dat$maturity) + n_label_pos[[2]]),
    max(as.numeric(dat$maturity) + n_label_pos[[1]])
  ))

  g <- ggplot(dat, aes_string("month_jitter", "maturity")) +
    geom_vline(xintercept = seq(1, 12), col = "grey95", lwd = 0.4) +
    geom_point(aes_string(size = "n_scaled", group = "sex", colour = "sex"),
      pch = 21, alpha = alpha
    ) +
    scale_fill_manual(values = fill_col) +
    scale_colour_manual(values = line_col) +
    scale_size_area(max_size = max_size) +
    scale_x_continuous(breaks = seq(1, 12), labels = month.abb) +
    ylab("") + xlab("") +
    guides(
      size = FALSE, colour = guide_legend(override.aes = list(size = 3.5)),
      fill = guide_legend(override.aes = list(size = 3.5))
    ) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_text(
      data = counts,
      aes_string(
        y = "y", x = "month_jitter", label = "total_month",
        colour = "sex"
      ), size = 2.25, hjust = 0.5, show.legend = FALSE
    ) +
    coord_cartesian(
      ylim = range(as.numeric(dat$maturity)) + c(-0.5, 1.5),
      expand = FALSE
    ) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(title = title, colour = "Sex", fill = "Sex")

  g
}

#' Plot maturity frequency by month
#'
#' @name plot_maturity_months
NULL

#' @param dat For [tidy_maturity_months()], data from [gfdata::get_survey_samples()]
#'   or [gfdata::get_commercial_samples()] or [bind_samples()]. For [plot_maturity_months()],
#'   data from [tidy_maturity_months()].
#' @param months A vector of months to include. Defaults to all.
#' @param usability_codes An optional vector of usability codes.
#'   All usability codes not in this vector will be omitted.
#'   Set to `NULL` to include all samples.
#' @param name_convention Name convention defaults to "corrected"
#'   will return the same names as "original", but with some corrections
#'   in how they are applied to fish with maturity_code == 3. Use "original"
#'   for the 2019-2023 version. Use "updated" for a new naming scheme that
#'   uses "developing" and "developed" in place of "mature" and "ripe", but
#'   note that rosettafish and other code may need to be updated to use these.
#' @param french Logical for French or English.
#' @export
#' @rdname plot_maturity_months
tidy_maturity_months <- function(dat, months = seq(1, 12),
  usability_codes = c(0, 1, 2, 6), name_convention = "corrected", french = FALSE) {

  if (!is.null(usability_codes)){
    dat <- filter(dat, .data$usability_code %in% usability_codes)
  }

  dat <- mutate(dat, month = lubridate::month(trip_start_date))
  dat <- filter(dat, month %in% months)
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical!
  dat <- dat %>% filter(maturity_convention_code != 9)
  dat <- dat %>% filter(maturity_convention_code != 9)
  if ("maturity_convention_maxvalue" %in% names(dat))
  dat <- dat %>% filter(maturity_code <= maturity_convention_maxvalue)
  dat <- dat %>%
    select(
      species_common_name,
      month,
      maturity_convention_code,
      maturity_code,
      sex
    )

  dat <- filter(dat, !is.na(sex))

  mat_df <- maturity_short_names # built-in package data

  if(name_convention == "original") {mat_df <- mat_df |> mutate(maturity_name_short = maturity_name_2019)}
  if(name_convention == "corrected") {mat_df <- mat_df |> mutate(maturity_name_short = maturity_name_short)}

  mat_levels <- rev(c(
    "Immature",
    "Maturing",
    "Mature",
    "Gravid",
    "Embryos",
    "Ripe",
    "Running Ripe",
    "Uterine Eggs",
    "Yolk Sac Pups",
    "Term Pups",
    "Spent",
    "Resting"
  ))

  if(name_convention == "updated") {
    mat_df <- mat_df |> mutate(maturity_name_short = maturity_name_updated)

  mat_levels <- rev(c(
    "Immature",
    "Maturing",
    "Mature",
    "Developing",
    "Developed",
    "Gravid",
    "Embryos",
    "Ripe",
    "Running Ripe",
    "Uterine Eggs",
    "Yolk Sac Pups",
    "Term Pups",
    "Spent",
    "Resorbing",
    "Resting"
  ))

  }

  dat <- left_join(dat,
    select(mat_df, sex, maturity_convention_code, maturity_code, maturity_name_short),
    by = c("sex", "maturity_convention_code", "maturity_code")
  )

  dat <- filter(dat, sex %in% c(1L, 2L))
  dat <- dat %>% mutate(sex = ifelse(sex == 2L, "F", "M"))


  mat_levels <- mat_levels[mat_levels %in% unique(dat$maturity_name_short)]
  dat <- dat[!is.na(dat$maturity_name_short), , drop = FALSE]

  if (french) {
    dat$maturity_name_short <- rosettafish::en2fr(dat$maturity_name_short)
    dat$maturity_name_short <- factor(dat$maturity_name_short,
      levels = rosettafish::en2fr(mat_levels)
    )
  } else {
    dat$maturity_name_short <- factor(dat$maturity_name_short,
      levels = mat_levels
    )
  }

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
#' @param min_fish The minimum number of fish for the circles to be plotted for
#'   a given month.
#'
#' @export
#' @rdname plot_maturity_months
#' @examples
#' \dontrun{
#' # pop_samples <- gfdata::get_survey_samples("pacific ocean perch")
#' tidy_maturity_months(pop_samples) %>%
#'   plot_maturity_months()
#' }
#' \dontrun{
#' d <- gfdata::get_survey_samples("lingcod")
#' tidy_maturity_months(d) %>%
#'   plot_maturity_months()
#' }
plot_maturity_months <- function(dat,
                                 max_size = 9,
                                 sex_gap = 0.2,
                                 fill_col = c("M" = "grey70", "F" = "black"),
                                 line_col = c("M" = "grey70", "F" = "black"),
                                 alpha = 0.8,
                                 title = "Maturity frequencies",
                                 n_label_pos = c(0.7, 1.3),
                                 min_fish = 0,
                                 french = FALSE) {
  dat <- dat %>%
    filter(!is.na(maturity)) %>%
    mutate(
      month_jitter =
        ifelse(sex == "M", month + sex_gap / 2, month - sex_gap / 2)
    ) %>%
    group_by(sex, month, month_jitter, maturity) %>%
    summarise(.n = n()) %>%
    ungroup() %>%
    group_by(month) %>%
    mutate(n_scaled = .n / max(.n)) %>%
    group_by(month, sex) %>%
    mutate(total_month = sum(.n),
      n_scaled = ifelse(total_month >= min_fish, n_scaled, 0)) %>%
    ungroup()

  counts <- select(dat, sex, total_month, month_jitter) %>% unique()
  counts <- mutate(counts, y = ifelse(sex == "F",
    max(as.numeric(dat$maturity) + n_label_pos[[2]]),
    max(as.numeric(dat$maturity) + n_label_pos[[1]])
  ))

  # not enough space!!
  # format_french_1000s <- function(x) {
  #   format(as.numeric(x), big.mark = " ", scientific = FALSE, trim = TRUE)
  # }
  # if (french) counts$total_month <- format_french_1000s(counts$total_month)

  g <- ggplot(dat, aes_string("month_jitter", "maturity")) +
    ylab("") + xlab("") +
    scale_x_continuous(breaks = seq(1, 12),
      labels = rosettafish::en2fr(month.abb, french), limits = c(1, 12)) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(title = title, colour = "Sex", fill = "Sex")

  if (nrow(dat) >= 10) {
    g <- g +
      geom_vline(xintercept = seq(1, 12), col = "grey95", lwd = 0.4) +
      geom_point(aes_string(size = "n_scaled", group = "sex", colour = "sex"),
        pch = 21, alpha = alpha
      ) +
      scale_fill_manual(values = fill_col) +
      scale_colour_manual(values = line_col) +
      scale_size_area(max_size = max_size) +
      guides(
        size = "none", colour = guide_legend(override.aes = list(size = 3.5)),
        fill = guide_legend(override.aes = list(size = 3.5))
      ) +
      geom_text(
        data = counts,
        aes_string(
          y = "y", x = "month_jitter", label = "total_month",
          colour = "sex"
        ), size = 2.25, hjust = 0.5, show.legend = FALSE
      ) +
      coord_cartesian(
        xlim = c(0.5, 12.5),
        ylim = range(as.numeric(dat$maturity)) + c(-0.5, 1.7),
        expand = FALSE
      )
  }

  g
}

#' Plot maturity frequency by month
#'
#' @name plot_maturity_months
NULL

#' @param months TODO
#' @param ssid TODO
#' @export
#' @rdname plot_maturity_months
tidy_maturity_months <- function(dat, months = seq(1, 12),
  ssid = NULL) {

  dat <- mutate(dat, month = lubridate::month(trip_start_date))
  dat <- filter(dat, month %in% months)
  dat <- dat[!duplicated(dat$specimen_id), ] # critical!
  dat <- dat %>%
    select(species_common_name,
      month,
      maturity_code,
      maturity_name,
      sex)

  dat <- filter(dat, !is.na(maturity_name), !is.na(sex))

  dat <- dat %>% mutate(sex = ifelse(sex == 2, "F", "M"))

  dat <- dat %>%
    mutate(maturity_name_short = replace(maturity_name, maturity_name %in%
        c("IMMATURE-1", "IMMATURE-2", "MATURING-SMALL"), "Immature")) %>%
    mutate(maturity_name_short = replace(maturity_name_short,
      maturity_name_short %in%
        c("MATURING-LARGE", "PRE-RIPENING", "PRE-RIPENING-R1",
          "PRE-RIPENING-R2", "RIPENING", "RIPENING-2R"), "Mature"))

  mat_levels <- rev(c("Immature", "Mature", "Ripe", "Spent", "Resting"))
  dat$maturity_name_short <- stringr::str_to_title(dat$maturity_name_short)
  dat$maturity_name_short <- factor(dat$maturity_name_short,
    levels = mat_levels)

  dat <- select(dat, species_common_name, month,
    maturity_name_short, sex) %>%
    rename(maturity = maturity_name_short) %>%
    ungroup()

  dat
}

#' @param dat TODO
#'
#' @param max_size  TODO
#' @param sex_gap  TODO
#' @param fill_col  TODO
#' @param line_col  TODO
#' @param alpha  TODO
#' @param title  TODO
#' @param n_label_pos  TODO
#'
#' @export
#' @rdname plot_maturity_months
#' @examples
#' \dontrun{
#' d <- get_survey_samples("lingcod")
#' tidy_maturity_months(d) %>%
#'   plot_maturity_months()
#' }

plot_maturity_months <- function(dat,
  max_size = 11,
  sex_gap = 0.2,
  fill_col = c("M" = "grey50", "F" = "#f44256"),
  line_col = c("M" = "grey50", "F" = "#f44256"),
  alpha = 0.8,
  title = "Maturity frequencies",
  n_label_pos = c(0.6, 0.8)) {

  dat <- dat %>%
    filter(!is.na(maturity)) %>%
    mutate(month_jitter =
        ifelse(sex == "M", month + sex_gap / 2, month - sex_gap / 2)) %>%
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
    max(as.numeric(dat$maturity) + n_label_pos[[1]])))

  g <- ggplot(dat, aes_string("month_jitter", "maturity")) +
    geom_vline(xintercept = seq(1, 12), col = "grey95", lwd = 0.4) +
    geom_point(aes_string(size = "n_scaled", group = "sex", colour = "sex"),
      pch = 21, alpha = alpha) +
    scale_fill_manual(values = fill_col) +
    scale_colour_manual(values = line_col) +
    scale_size_area(max_size = max_size) +
    scale_x_continuous(breaks = seq(1, 12), labels = month.abb) +
    ylab("") + xlab("") +
    guides(size = FALSE, colour = guide_legend(override.aes = list(size = 3.5)),
      fill = guide_legend(override.aes = list(size = 3.5))) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_text(data = counts,
      aes_string(y = "y", x = "month_jitter", label = "total_month",
        colour = "sex"), size = 2.25, hjust = 0.5, show.legend = FALSE) +
    coord_cartesian(ylim = range(as.numeric(dat$maturity)) + c(-0.5, 1),
      expand = FALSE) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(title = title, colour = "Sex", fill = "Sex")

  g
}

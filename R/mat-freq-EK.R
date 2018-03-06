# Maturity frequency by month

require(dplyr)
tidy_mat <- function(dat,
  months = seq(1, 12),
  ssid = NULL) {

  dat <- mutate(dat, month = lubridate::month(trip_start_date))
  dat <- filter(dat, month %in% months)
  dat <- dat[!duplicated(dat$specimen_id), ] # critical!
  dat <- dat %>%
    select(species_common_name, trip_start_date, survey_series_id,
      survey_series_desc, year, month, sample_id, specimen_id,
      maturity_convention_desc,
      maturity_code, maturity_name, sex, maturity_desc)

  dat <- filter(dat, !is.na(maturity_name), !is.na(sex)) %>%
    group_by(species_common_name) %>%
    mutate(n_maturities = n()) %>%
    ungroup()

  dat <- dat %>% mutate(sex = ifelse(sex == 2, "F", "M"))

  dat

}



require(ggplot2)
#' @export
plot_maturity_months <- function(dat, max_size = 5, sex_gap = 0.2,
  months = NULL,
  fill_col = c("M" = "grey50", "F" = "#f44256"),
  line_col = c("M" = "grey50", "F" = "#f44256"),
  survey_cols = NULL, alpha = 0.85) {

  month_min <- min(dat$month, na.rm = TRUE)
  month_max <- max(dat$month, na.rm = TRUE)
  month_label <- month.abb[seq(month_min, month_max)]

  dat <- dat %>% mutate(month_jitter = ifelse(sex == "M",
    month + sex_gap / 2, month - sex_gap / 2)) %>%
    group_by(month, .data$month_jitter, maturity_convention_desc,
      maturity_code, maturity_name, sex) %>%
    summarise(n = n()) %>%
    group_by(month) %>%
    mutate(n_scaled = n / max(n), total = sum(n)) %>%
    ungroup()

  counts <- select(dat, .data$total, .data$month) %>% unique()

  dat$sex[is.na(dat$sex)] <- "F" # just for legend or we'll have "NAs"

  if (!is.null(survey_cols)) {
    survey_col_names <- names(survey_cols)
    col <- setNames(survey_cols, paste("F", survey_col_names))
    col <- c(col, setNames(rep("#888888", length(col)),
      paste("M", survey_col_names)))
    fill_col <- paste0(substr(col, 1L, 7L), as.character(alpha * 100))
    line_col <- col
    dat$sex <- paste(dat$sex, dat$survey)
  }


  dat <-
    dat %>%
    mutate(maturity_name_short = replace(maturity_name, maturity_name %in%
        c("IMMATURE-1", "IMMATURE-2", "MATURING-SMALL"), "Immature")) %>%
    mutate(maturity_name_short = replace(maturity_name_short, maturity_name_short %in%
        c("MATURING-LARGE", "PRE-RIPENING", "PRE-RIPENING-R1", "PRE-RIPENING-R2","RIPENING", "RIPENING-2R"), "Mature"))

  dat$maturity_name_short <- stringr::str_to_title(dat$maturity_name_short)

  dat <- dat %>%
    mutate(maturity_name_jitter = replace(maturity_name_short, maturity_name_short == "Immature", 1)) %>%
    mutate(maturity_name_jitter = replace(maturity_name_jitter, maturity_name_jitter == "Mature", 2)) %>%
    mutate(maturity_name_jitter = replace(maturity_name_jitter, maturity_name_jitter == "Ripe", 3)) %>%
    mutate(maturity_name_jitter = replace(maturity_name_jitter, maturity_name_jitter == "Spent", 4)) %>%
    mutate(maturity_name_jitter = replace(maturity_name_jitter, maturity_name_jitter == "Resting", 5))
  dat$maturity_name_jitter <- jitter(as.numeric(dat$maturity_name_jitter))

  mat_levels <- c("Immature", "Mature", "Ripe", "Spent", "Resting")

  g <- ggplot(dat, aes_string("month_jitter", "maturity_name_jitter")) +
    geom_vline(xintercept = seq(month_min, month_max),
     col = "grey95", lwd = 0.4) +
    # geom_hline(yintercept = seq(0, 5, 10), col = "grey95",
    #   lwd = 0.4) +
    geom_point(aes_string(size = "n_scaled", group = "sex", colour = "sex"),
      pch = 21, alpha = 0.9) +
    scale_fill_manual(values = fill_col) +
    scale_colour_manual(values = line_col) +
    scale_y_continuous(breaks = seq(1,5,1), labels = mat_levels) +
    scale_x_continuous(breaks = seq(3,11,1), labels = month_label) +
    ylab("Maturity") +
    xlab("Month") +
    guides(size = FALSE, colour = guide_legend(override.aes = list(size = 3.5)),
      fill = guide_legend(override.aes = list(size = 3.5))) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_text(data = counts,
      y = max(dat$maturity_name_jitter + 0.15) ,
      aes_string(x = "month", label = "total"),
      inherit.aes = FALSE, colour = "grey50", size = 2.25, hjust = 1) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(title = "Maturity frequencies", colour = "Sex", fill = "Sex")

  if (!is.null(survey_cols))
    g <- g + guides(fill = FALSE, colour = FALSE)

  g
}

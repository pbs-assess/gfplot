# EAK Building maturity frequency by month based on plot age frequency code
# from SCA and fit mat code

tidy_mat <- function(dat,
  sample_id_re = FALSE,
  months = seq(1, 12),
  ageing_method = c(3, 17)) {

  dat <- mutate(dat, month = lubridate::month(trip_start_date))
  dat <- dplyr::filter(dat, month %in% months)
  dat <- dat[!duplicated(dat$specimen_id), ] # critical!
  dat <- dat %>%
    select(species_common_name, trip_start_date,
      survey_series_desc, year, month, sample_id, specimen_id,
      maturity_convention_desc, maturity_convention_maxvalue,
      maturity_code, maturity_name, sex, maturity_desc)

  # file <- system.file("extdata", "maturity_assignment.csv",
  #   package = "gfplot")
  #
  # mat_df <- readr::read_csv(file,
  #   col_types = readr::cols(
  #     maturity_convention_code = readr::col_integer(),
  #     maturity_convention_desc = readr::col_character(),
  #     sex = readr::col_integer(),
  #     maturity_convention_maxvalue = readr::col_integer(),
  #     mature_at = readr::col_integer()))
  # mat_df$maturity_convention_maxvalue <- NULL

  # dat <- left_join(dat, mat_df, by = c("sex", "maturity_convention_desc"))
  dat <- filter(dat, maturity_code <= maturity_convention_maxvalue) %>%
    select(-maturity_convention_maxvalue)
  # dat <- mutate(dat, mature = maturity_code >= mature_at)

  # .d <- switch(type,
  #   age = filter(dat, !is.na(mature), !is.na(age), !is.na(sex)) %>%
  #     rename(age_or_length = age),
  #   length = filter(dat, !is.na(mature), !is.na(length), !is.na(sex)) %>%
  #     rename(age_or_length = length))
  # .d <- mutate(.d, female = ifelse(sex == 2L, 1L, 0L))

  # if (sample_id_re) {
  #   m <- glmmTMB::glmmTMB(mature ~ age_or_length * female + (1 | sample_id),
  #     data = .d, family = binomial)
  #   b <- glmmTMB::fixef(m)[[1L]]
  # } else {
  #   m <- stats::glm(mature ~ age_or_length * female,
  #     data = .d, family = binomial)
  #   b <- stats::coef(m)
  # }

  # if (length(unique(.d$sample_id)) > 100L) {
  #   s_ids <- sample(unique(.d$sample_id), 100L)
  # } else {
  #   s_ids <- unique(.d$sample_id)
  # }

  # age_or_length <- seq(min(.d$age_or_length), max(.d$age_or_length),
  #   length.out = 300L)
  # nd <- expand.grid(age_or_length = age_or_length, sample_id = s_ids,
  #   female = c(0L, 1L), stringsAsFactors = FALSE)
  # if (sample_id_re)
  #   nd$glmm_re <- predict(m, newdata = nd, se.fit = FALSE)
  # nd$glmm_fe <- plogis(b[[1L]] + b[[3L]] * nd$female +
  #     b[[2L]] * nd$age_or_length + b[[4L]] * nd$age_or_length * nd$female)
  #
  # list(data = .d, pred_data = nd, model = m, sample_id_re = sample_id_re,
  #   type = type)

  surv <- tibble(survey_series_desc = survey_series_desc, survey = survey)
  dat$survey <- NULL # extra careful
  dat <- inner_join(dat, surv, by = "survey_series_desc")

tidy <- filter(dat, !is.na(maturity_name), !is.na(sex)) %>%
  group_by(species_common_name, survey_series_desc) %>%
  mutate(n_maturities = n()) %>%
  ungroup()

ds <- tidy %>% mutate(sex = ifelse(sex == 2, "F", "M"))

all_surveys <- tibble(survey = survey)

complete_df <- expand.grid(
  survey = survey,
  age = 1,
  month = seq(min(tidy$month), max(tidy$month), 1),
  sex = NA,
  stringsAsFactors = FALSE
)
if (nrow(ds) == 0)
  ds <- complete_df

ds <- full_join(ds, all_surveys, by = "survey") %>%
  select(-survey_series_desc)
ds$survey <- factor(ds$survey, levels = survey)

ds <- filter(ds, !is.na(maturity_name))
ds

}




# dat <- ds

#' @export
plot_maturity_months <- function(dat, max_size = 5, sex_gap = 0.2, month_increment = 2,
  ylab = "Maturity desc", months = NULL,
  fill_col = c("M" = "grey50", "F" = "#f44256"),
  line_col = c("M" = "grey50", "F" = "#f44256"),
  survey_cols = NULL, alpha = 0.85) {

  month_min <- min(dat$month, na.rm = TRUE)
  month_max <- max(dat$month, na.rm = TRUE)
  mat_max <- max(dat$maturity_name, na.rm = TRUE)

  dat <- dat %>% mutate(month_jitter = ifelse(sex == "M",
    month + sex_gap / 2, month - sex_gap / 2)) %>%
    group_by(month, .data$month_jitter, maturity_name, sex, survey) %>%
    summarise(n = n()) %>%
    group_by(month, survey) %>%
    mutate(n_scaled = n / max(n), total = sum(n)) %>%
    ungroup()

  counts <- select(dat, .data$total, .data$month,
    .data$survey) %>% unique()

  dat$sex[is.na(dat$sex)] <- "F" # just for legend or we'll have "NAs"
  # maturity_range <- diff(range(dat$maturity_code, na.rm = TRUE))

  if (!is.null(survey_cols)) {
    survey_col_names <- names(survey_cols)
    col <- setNames(survey_cols, paste("F", survey_col_names))
    col <- c(col, setNames(rep("#888888", length(col)),
      paste("M", survey_col_names)))
    fill_col <- paste0(substr(col, 1L, 7L), as.character(alpha * 100))
    line_col <- col
    dat$sex <- paste(dat$sex, dat$survey)
  }

  if (is.null(months))
    months <- c(month_min, month_max)

  g <- ggplot(dat, aes_string("month_jitter", "maturity_name")) +
    geom_vline(xintercept = seq(months[1], months[2], 1),
      col = "grey95", lwd = 0.4) +
    geom_hline(yintercept = seq(0, age_max, 10), col = "grey95",
      lwd = 0.4) +
    geom_point(aes_string(size = "n_scaled", group = "sex", colour = "sex"),
      pch = 21, alpha = 0.9) +
    facet_wrap(~survey, nrow = 1) +
    scale_fill_manual(values = fill_col) +
    scale_colour_manual(values = line_col) +
    scale_x_continuous(breaks =
        seq(round_down_even(year_range[1]), year_range[2], year_increment)) +
    xlab("") +
    ylab(ylab) +
    scale_size_area(max_size = max_size) +
    coord_cartesian(
      xlim = year_range + c(-0.8 - sex_gap / 2, 0.8 + sex_gap / 2),
      ylim = c(0, age_max + 0.02 * age_range), expand = FALSE) +
    guides(size = FALSE, colour = guide_legend(override.aes = list(size = 3.5)),
      fill = guide_legend(override.aes = list(size = 3.5))) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_text(data = counts, y = age_max + 0.005 * age_range,
      aes_string(x = "year", label = "total"),
      inherit.aes = FALSE, colour = "grey50", size = 2.25, hjust = 1,
      angle = 90) +
    theme(panel.spacing = unit(-0.1, "lines")) +
    labs(title = "Age frequencies", colour = "Sex", fill = "Sex")

  if (!is.null(survey_cols))
    g <- g + guides(fill = FALSE, colour = FALSE)

  g
}

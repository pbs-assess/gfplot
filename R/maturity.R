#' TODO
#'
#' @param dat TODO
#' @param type TODO
#'
#' @importFrom stats binomial plogis predict
#' @export
#' @family plotting functions

plot_maturity <- function(dat, type = "age") {

  # dbio <- readRDS("data-cache/all-survey-bio.rds")
  dbio <- dat[!duplicated(dat$specimen_id), ] # critical!
  dbio <- dbio %>%
    select(species_common_name, species_science_name,
      year, age, length, weight,
      maturity_code, sex, survey_series_desc,
      maturity_convention_desc, maturity_convention_maxvalue,
      specimen_id, sample_id, trip_start_date)

  dbio <- mutate(dbio, month = lubridate::month(trip_start_date))

  file <- system.file("extdata", "maturity_assignment.csv", package = "PBSsynopsis")

  mat_df <- readr::read_csv(file,
    col_types = readr::cols(
      maturity_convention_code = readr::col_integer(),
      maturity_convention_description = readr::col_character(),
      specimen_sex_code = readr::col_integer(),
      maturity_convention_maxvalue = readr::col_integer(),
      mature_at = readr::col_integer())) %>%
    dplyr::rename(sex = specimen_sex_code,
      maturity_convention_desc = maturity_convention_description) %>%
    select(-maturity_convention_maxvalue)

  dbio <- left_join(dbio, mat_df, by = c("sex", "maturity_convention_desc"))
  dbio <- mutate(dbio, mature = maturity_code >= mature_at)

  if (type == "age") {
    dbio <- dplyr::filter(dbio, !is.na(mature), !is.na(age), !is.na(sex))
    xx <- dbio %>% dplyr::rename(age_or_length = age)
  } else {
    dbio <- dplyr::filter(dbio, !is.na(mature), !is.na(length), !is.na(sex))
    xx <- dbio %>% dplyr::rename(age_or_length = length)
  }

  xx <- xx %>% mutate(female = ifelse(sex == 2, 1, 0))

  m_re <- glmmTMB::glmmTMB(mature ~ age_or_length * female + (1 | sample_id),
    data = xx, family = binomial)
  b <-  glmmTMB::fixef(m_re)[[1]]

  if (length(unique(xx$sample_id)) > 100L) {
    s_ids <- sample(unique(xx$sample_id), 100L)
  } else {
    s_ids <- unique(xx$sample_id)
  }

  age_or_length <- seq(min(xx$age_or_length), max(xx$age_or_length), length.out = 100)
  nd <- expand.grid(age_or_length = age_or_length, sample_id = s_ids, female = c(0, 1),
    stringsAsFactors = FALSE)
  nd$glmm <- predict(m_re, newdata = nd, se.fit = FALSE)
  nd$glmm_fe <- plogis(b[[1]] + b[[3]] * nd$female +
      b[[2]] * nd$age_or_length + b[[4]] * nd$age_or_length * nd$female)
  nd_fe <- filter(nd, sample_id == s_ids[[1]]) %>% # fake
    select(-glmm)

  logit_perc <- function(a, b, perc = 0.5) {
    -(log((1/perc) - 1) + a) / b
  }

  m_perc <- data.frame(p0.5 = logit_perc(a = b[[1]], b = b[[2]], perc = 0.5))
  m_perc$p0.95 <- logit_perc(a = b[[1]], b = b[[2]], perc = 0.95)
  m_perc$p0.05 <- logit_perc(a = b[[1]], b = b[[2]], perc = 0.05)

  f_perc <- data.frame(p0.5 = logit_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.5))
  f_perc$p0.95 <- logit_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.95)
  f_perc$p0.05 <- logit_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.05)

  labs <- tibble(p = c("05", "50", "95"),
    value = c(m_perc$p0.05, m_perc$p0.5, m_perc$p0.95),
    x = 0.7 * max(age_or_length),
    y = seq(0.4, 0.1, length.out = 3), sex = "M")

  labs_f <- tibble(p = c("05", "50", "95"),
    value = c(f_perc$p0.05, f_perc$p0.5, f_perc$p0.95),
    x = 0.7 * max(age_or_length),
    y = seq(0.9, 0.6, length.out = 3), sex = "F")

  labs <- bind_rows(labs, labs_f)

  nd_fe <- mutate(nd_fe, sex = ifelse(female == 1, "F", "M"))
  nd <- mutate(nd, sex = ifelse(female == 1, "F", "M"))

  ggplot(nd_fe, aes_string("age_or_length", "glmm_fe", colour = "sex")) +
    geom_line(data = nd,
      aes_string("age_or_length", "glmm", group = "paste(sample_id, sex)",
        colour = sex), inherit.aes = FALSE, alpha = 0.05) +
    geom_vline(data = labs, aes_string(xintercept = "value", colour = "sex"), lty = 2,
      show.legend = FALSE) +
    geom_line(size = 1.25) +
    scale_colour_manual(values = c("M" = "grey20", "F" = "red")) +
    xlab("Age (years)") + ylab("Probability mature") +
    geom_text(data = labs, aes_string(x = "x", y = "y",
      label = paste0(sex, " ", p, " = ", sprintf("%.1f", round(value, 1)), "y")),
      hjust = 0, show.legend = FALSE) +
    PBSsynopsis::theme_pbs() +
    coord_cartesian(expand = FALSE, ylim = c(-0.005, 1.005), xlim = c(0, max(nd_fe$age_or_length))) +
    labs(colour = "Sex")
}

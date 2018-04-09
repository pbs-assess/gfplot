#' Fit and plot maturity ogives
#'
#' @param dat TODO
#' @param sample_id_re TODO
#' @param months TODO
#' @param ageing_method_codes A numeric vector of ageing method codes to filter
#'   on. Default to `NULL`, which brings in all valid ageing codes.
#'   See [get_age_methods()].
#' @rdname plot_mat_ogive
#' @export
#' @examples
#' \dontrun{
#' d <- get_survey_samples("pacific ocean perch", ssid = 1)
#'
#' m <- fit_mat_ogive(d, sample_id_re = FALSE, months = 1:6)
#' plot_mat_ogive(m)
#'
#' m <- fit_mat_ogive(d, type = "length", sample_id_re = FALSE, months = 1:6)
#' plot_mat_ogive(m)
#'
#' ## with random intercepts for sample ID:
#' m <- fit_mat_ogive(d, type = "length", sample_id_re = TRUE, months = 1:6)
#' plot_mat_ogive(m)
#' }

fit_mat_ogive <- function(dat,
                          type = c("age", "length"),
                          sample_id_re = FALSE,
                          months = seq(1, 12),
                          ageing_method_codes = NULL) {
  dat <- mutate(dat, month = lubridate::month(trip_start_date))


  type <- match.arg(type)
  dat <- dat[dat$sex %in% c(1, 2), , drop = FALSE]
  dat <- dat[dat$month %in% months, , drop = FALSE]

  if (type == "age" && !is.null(ageing_method_codes)) {
    dat <- filter(dat, ageing_method %in% ageing_method_codes)
  }

  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical!
  dat <- dat %>%
    select(
      species_common_name,
      year, age, length, weight,
      maturity_code, sex,
      maturity_convention_desc,
      specimen_id, sample_id, trip_start_date
    )

  file <- system.file("extdata", "maturity_assignment.csv",
    package = "gfplot"
  )

  mat_df <- readr::read_csv(file,
    col_types = readr::cols(
      maturity_convention_code = readr::col_integer(),
      maturity_convention_desc = readr::col_character(),
      sex = readr::col_integer(),
      mature_at = readr::col_integer()
    )
  )

  dat <- left_join(dat, mat_df, by = c("sex", "maturity_convention_desc"))
  dat <- mutate(dat, mature = maturity_code >= mature_at)

  type <- match.arg(type)
  .d <- switch(type,
    age = filter(dat, !is.na(mature), !is.na(age), !is.na(sex)) %>%
      rename(age_or_length = age),
    length = filter(dat, !is.na(mature), !is.na(length), !is.na(sex)) %>%
      rename(age_or_length = length)
  )
  .d <- mutate(.d, female = ifelse(sex == 2L, 1L, 0L))

  if (sample_id_re) {
    m <- glmmTMB::glmmTMB(mature ~ age_or_length * female + (1 | sample_id),
      data = .d, family = binomial
    )
    b <- glmmTMB::fixef(m)[[1L]]
  } else {
    m <- stats::glm(mature ~ age_or_length * female,
      data = .d, family = binomial
    )
    b <- stats::coef(m)
  }

  if (length(unique(.d$sample_id)) > 100L) {
    s_ids <- sample(unique(.d$sample_id), 100L)
  } else {
    s_ids <- unique(.d$sample_id)
  }

  age_or_length <- seq(min(.d$age_or_length), max(.d$age_or_length),
    length.out = 300L
  )
  nd <- expand.grid(
    age_or_length = age_or_length, sample_id = s_ids,
    female = c(0L, 1L), stringsAsFactors = FALSE
  )
  if (sample_id_re) {
    nd$glmm_re <- predict(m, newdata = nd, se.fit = FALSE)
  }
  nd$glmm_fe <- plogis(b[[1L]] + b[[3L]] * nd$female +
    b[[2L]] * nd$age_or_length + b[[4L]] * nd$age_or_length * nd$female)

  list(
    data = .d, pred_data = nd, model = m, sample_id_re = sample_id_re,
    type = type
  )
}

#' @param object TODO
#' @param type TODO
#' @param xlab TODO
#' @param title TODO
#' @param rug TODO
#' @param rug_n TODO
#' @param x_max TODO as -fold of L or A95.
#'
#' @importFrom stats binomial plogis predict
#' @export
#' @family plotting functions
#' @rdname plot_mat_ogive

plot_mat_ogive <- function(object,
                           xlab = if (object$type[[1]] == "age") "Age (years)" else "Length (cm)",
                           title =
                             if (object$type[[1]] == "age") "Age at maturity" else "Length at maturity",
                           rug = TRUE, rug_n = 1500, x_max = 1.75) {
  nd_re <- object$pred_data

  if (object$sample_id_re) {
    b <- glmmTMB::fixef(object$model)[[1L]]
  } else {
    b <- stats::coef(object$model)
  }

  nd_fe <- filter(nd_re, sample_id == nd_re$sample_id[[1L]]) # fake; all same
  nd_fe$glmm_re <- NULL # also may not exist if no random effects

  logit_perc <- function(a, b, perc = 0.5) {
    -(log((1 / perc) - 1) + a) / b
  }

  m_perc <- data.frame(p0.5 = logit_perc(a = b[[1]], b = b[[2]], perc = 0.5))
  m_perc$p0.95 <- logit_perc(a = b[[1]], b = b[[2]], perc = 0.95)
  m_perc$p0.05 <- logit_perc(a = b[[1]], b = b[[2]], perc = 0.05)

  f_perc <- data.frame(
    p0.5 = logit_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.5)
  )
  f_perc$p0.95 <- logit_perc(
    a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.95
  )
  f_perc$p0.05 <- logit_perc(
    a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.05
  )

  labs_f <- tibble(
    p = c("05", "50", "95"),
    value = c(f_perc$p0.05, f_perc$p0.5, f_perc$p0.95),
    x = 0.75 * max(nd_re$age_or_length), # re-calculated below
    y = seq(0.75, 0.6, length.out = 3L), sex = "F"
  )

  labs_m <- tibble(
    p = c("05", "50", "95"),
    value = c(m_perc$p0.05, m_perc$p0.5, m_perc$p0.95),
    x = 0.75 * max(nd_re$age_or_length), # re-calculated below
    y = seq(0.4, 0.25, length.out = 3), sex = "M"
  )


  labs <- bind_rows(labs_m, labs_f)

  nd_fe <- mutate(nd_fe, sex = ifelse(female == 1L, "F", "M"))
  nd_re <- mutate(nd_re, sex = ifelse(female == 1L, "F", "M"))
  object$data <- mutate(object$data, sex = ifelse(female == 1L, "F", "M"))

  if (object$type[[1]] == "age") {
    labs <- mutate(labs,
      label =
        paste0(sex, " ", p, " = ", sprintf("%.1f", round(value, 1L)), "y")
    )
  } else {
    labs <- mutate(labs,
      label =
        paste0(sex, " ", p, " = ", sprintf("%.1f", round(value, 1L)), "cm")
    )
  }
  max_x <- min(c(max(labs$value) * x_max, max(nd_fe$age_or_length)))

  if (object$type[[1]] == "age") {
    labs <- mutate(labs, x = max_x * 0.7) # actual x position calculation
  } else {
    labs <- mutate(labs, x = max_x * 0.05) # actual x position calculation
  }

  g <- ggplot(nd_fe, aes_string("age_or_length", "glmm_fe", colour = "sex"))
  if ("glmm_re" %in% names(nd_re)) {
    g <- g + geom_line(
      data = nd_re,
      aes_string("age_or_length", "glmm_re",
        group = "paste(sample_id, sex)",
        colour = "sex"
      ), inherit.aes = FALSE, alpha = 0.05
    )
  }
  g <- g + geom_vline(
    data = labs,
    aes_string(xintercept = "value", colour = "sex"),
    lty = 2, show.legend = FALSE
  ) +
    geom_line(size = 1.25) +
    scale_colour_manual(values = c("M" = "grey30", "F" = "#d80d0d")) +
    xlab(xlab) + ylab("Probability mature") +
    geom_text(
      data = labs, aes_string(
        x = "x", y = "y",
        label = "label"
      ),
      hjust = 0, show.legend = FALSE, size = 3
    ) +
    theme_pbs() +
    coord_cartesian(
      expand = FALSE, ylim = c(-0.005, 1.005),
      xlim = c(0, max_x)
    ) +
    labs(colour = "Sex") +
    ggplot2::ggtitle(title)

  if (rug) {
    if (nrow(object$data) > rug_n) {
      temp <- object$data[sample(seq_len(nrow(object$data)), rug_n),
        ,
        drop = FALSE
      ]
    } else {
      temp <- object$data
    }
    position <- if (object$type == "age") "jitter" else "identity"
    g <- g + ggplot2::geom_rug(
      data = filter(temp, mature == 0L),
      sides = "b", position = position, alpha = 0.6,
      aes_string(x = "age_or_length", y = "as.numeric(mature)", colour = "sex")
    )
    g <- g + ggplot2::geom_rug(
      data = filter(temp, mature == 1L),
      sides = "t", position = position, alpha = 0.6,
      aes_string(x = "age_or_length", y = "as.numeric(mature)", colour = "sex")
    )
  }

  g
}

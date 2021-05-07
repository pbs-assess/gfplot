#' Fit and plot maturity ogives
#'
#' @param dat Data from [get_survey_samples()].
#' @param type Should this be an age or length fit?
#' @param sample_id_re If `TRUE` then the model will include random intercepts
#'   for sample ID.
#' @param year_re If `TRUE` the model will include random intercepts
#'   for year.
#' @param months A numeric vector indicating which months to include when
#'   fitting the maturity ogive. Defaults to all months.
#' @param ageing_method_codes A numeric vector of ageing method codes to filter
#'   on. Defaults to `NULL`, which brings in all valid ageing codes.
#'   See [get_age_methods()].
#' @param usability_codes An optional vector of usability codes.
#'   All usability codes not in this vector will be omitted.
#'   Set to `NULL` to include all samples.
#' @rdname plot_mat_ogive
#' @export
#' @examples
#' # d <- get_survey_samples("pacific ocean perch", ssid = 1)
#' d <- pop_samples
#'
#' m <- fit_mat_ogive(d, type = "age", sample_id_re = FALSE)
#' plot_mat_ogive(m)
#'
#' m <- fit_mat_ogive(d, type = "length", sample_id_re = FALSE)
#' plot_mat_ogive(m)
#' \dontrun{
#' ## with random intercepts for sample ID:
#' m <- fit_mat_ogive(d, type = "length", sample_id_re = TRUE)
#' plot_mat_ogive(m)
#' }
fit_mat_ogive <- function(dat,
                          type = c("age", "length"),
                          sample_id_re = FALSE,
                          year_re = FALSE,
                          months = seq(1, 12),
                          ageing_method_codes = NULL,
                          usability_codes = c(0, 1, 2, 6)) {
  dat <- mutate(dat, month = lubridate::month(trip_start_date))

  dat <- dat %>% filter(maturity_convention_code != 9)

  if ("maturity_convention_maxvalue" %in% names(dat)) {
    dat <- dat %>%
      filter(maturity_code <= maturity_convention_maxvalue)
  }
  if (!is.null(usability_codes)) {
    dat <- filter(dat, .data$usability_code %in% usability_codes)
  }

  type <- match.arg(type)
  dat <- dat[dat$sex %in% c(1, 2), , drop = FALSE]
  dat <- dat[dat$month %in% months, , drop = FALSE]

  if (type == "age" && !is.null(ageing_method_codes)) {
    dat <- filter(dat, ageing_method %in% ageing_method_codes)
  }

  if (nrow(dat) == 0) {
    warning("No data")
    return(NA)
  }

  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical!
  dat <- dat %>%
    select(
      species_common_name,
      year,
      age, length, weight,
      maturity_code, sex,
      maturity_convention_code,
      # survey_series_id,
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

  dat <- left_join(dat, mat_df, by = c("sex", "maturity_convention_code"))
  dat <- mutate(dat, mature = maturity_code >= mature_at)

  type <- match.arg(type)
  .d <- switch(type,
    age = filter(dat, !is.na(mature), !is.na(age), !is.na(sex)) %>%
      rename(age_or_length = age),
    length = filter(dat, !is.na(mature), !is.na(length), !is.na(sex), !is.na(year)) %>%
      rename(age_or_length = length)
  )
  .d <- mutate(.d, female = ifelse(sex == 2L, 1L, 0L))
  if (nrow(.d) == 0) {
    warning("No data")
    return(NA)
  }

  if (sample_id_re) {
    if (year_re) {
      m <- glmmTMB::glmmTMB(mature ~ age_or_length * female +
        (1 | sample_id) +
        # (1 | survey_series_id) +
        (1 | year),
      data = .d, family = binomial
      )
      b <- glmmTMB::fixef(m)[[1L]]
      ranef <- glmmTMB::ranef(m)
      re <- ranef$cond$year
    } else {
      m <- glmmTMB::glmmTMB(mature ~ age_or_length * female + (1 | sample_id),
        data = .d, family = binomial
      )

      b <- glmmTMB::fixef(m)[[1L]]
    }
  } else {
    if (year_re) {
      m <- glmmTMB::glmmTMB(mature ~ age_or_length * female + (1 | year),
        data = .d, family = binomial
      )
      b <- glmmTMB::fixef(m)[[1L]]
      ranef <- glmmTMB::ranef(m)
      re <- ranef$cond$year
    } else {
      m <- stats::glm(mature ~ age_or_length * female,
        data = .d, family = binomial
      )
      b <- stats::coef(m)
    }
  }

  age_or_length <- seq(min(.d$age_or_length), max(.d$age_or_length),
    length.out = 300L
  )

  if (year_re) {
    # builds nd using all sample ids to make sure full year distribution is represented
    year <- unique(.d$year)
    s_ids <- .d %>%
      select(year, sample_id) %>%
      distinct()

    nd1 <- expand.grid(
      age_or_length = age_or_length,
      sample_id = s_ids$sample_id,
      female = c(0L, 1L), stringsAsFactors = FALSE
    )

    nd <- left_join(nd1, s_ids)

    year_f <- as.character(nd$year)
  } else {
    if (length(unique(.d$sample_id)) > 100L) {
      s_ids <- sample(unique(.d$sample_id), 100L)
    } else {
      s_ids <- unique(.d$sample_id)
    }

    nd <- expand.grid(
      age_or_length = age_or_length, sample_id = s_ids,
      female = c(0L, 1L), stringsAsFactors = FALSE
    )
  }

  if (sample_id_re) {
    nd$glmm_re <- predict(m, newdata = nd, type = "response", se.fit = FALSE)
  } else {
    if (year_re) {
      year_f <- as.character(nd$year)
      nd$glmm_re <- predict(m, newdata = nd, type = "response", se.fit = FALSE)
      # nd$glmm_re2 <- plogis(b[[1L]] + re[year_f, ] + b[[3L]] * nd$female +
      #   b[[2L]] * nd$age_or_length + b[[4L]] * nd$age_or_length * nd$female)
    }
  }

  nd$glmm_fe <- plogis(b[[1L]] + b[[3L]] * nd$female +
    b[[2L]] * nd$age_or_length + b[[4L]] * nd$age_or_length * nd$female)

  if (year_re) {
    mat_perc <- extract_maturity_perc_re(b, re)
  } else {
    mat_perc <- extract_maturity_perc(b)
  }


  se_l50 <- tryCatch({
    delta_method(~ -(log((1 / 0.5) - 1) + x1 + x3) / (x2 + x4),
      mean = stats::coef(m), cov = stats::vcov(m)
    )
  }, error = function(e) NA)

  list(
    data = .d, pred_data = nd, model = m, sample_id_re = sample_id_re,
    year_re = year_re,
    type = type, mat_perc = mat_perc, se_50 = se_l50
  )
}

delta_method <- function(g, mean, cov) {
  # simplified from msm::deltamethod
  cov <- as.matrix(cov)
  n <- length(mean)
  g <- list(g)
  syms <- paste0("x", seq_len(n))
  for (i in seq_len(n)) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(stats::deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  sqrt(diag(new.covar))
}

sd2cv <- function(.sd) {
  sqrt(exp(.sd^2) - 1)
}


#' @param object Output from [fit_mat_ogive()].
#' @param xlab X axis label.
#' @param title Title for the plot.
#' @param rug Logical indicating whether rug lines should be added.
#' @param rug_n The number of rug lines to sample from the total number of fish.
#' @param x_max Used in determining the right axis limit.
#' @param prediction_type The prediction lines to show. Useful if you only want
#'   to show model fits when you have sufficient data.
#' @param french Translate to French?
#'
#' @importFrom stats binomial plogis predict
#' @export
#' @rdname plot_mat_ogive

plot_mat_ogive <- function(object,
                           xlab = if (object$type[[1]] == "age") "Age (years)" else "Length (cm)",
                           title =
                             if (object$type[[1]] == "age") "Age at maturity" else "Length at maturity",
                           rug = TRUE, rug_n = 1500, x_max = 1.75,
                           prediction_type = c("all", "male", "female", "none"),
                           french = FALSE) {
  if (object$sample_id_re) {
    b <- glmmTMB::fixef(object$model)[[1L]]
  } else {
    if (object$year_re) {
      b <- glmmTMB::fixef(object$model)[[1L]]
    } else {
      b <- stats::coef(object$model)
    }
  }

  nd_re <- object$pred_data
  nd_fe <- object$pred_data
  # nd_fe <- filter(nd_re, year == nd_re$year[[1L]]) # fake; all same
  nd_fe$glmm_re <- NULL # also may not exist if no random effects

  prediction_type <- match.arg(prediction_type)
  if (prediction_type == "male") {
    nd_fe <- filter(nd_fe, female == 0L)
  }
  if (prediction_type == "female") {
    nd_fe <- filter(nd_fe, female == 1L)
  }

  # if (prediction_type == "none") {
  #   nd_fe <- filter(nd_fe, !female %in% c(0L, 1L)) # i.e. nothing
  # }

  m_perc <- data.frame(
    p0.5 = logit_perc(a = b[[1]], b = b[[2]], perc = 0.5)
  )
  m_perc$p0.95 <- logit_perc(a = b[[1]], b = b[[2]], perc = 0.95)
  m_perc$p0.05 <- logit_perc(a = b[[1]], b = b[[2]], perc = 0.05)

  f_perc <- data.frame(
    p0.5 = logit_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.5)
  )
  f_perc$p0.95 <- logit_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.95)
  f_perc$p0.05 <- logit_perc(a = b[[1]] + b[[3]], b = b[[2]] + b[[4]], perc = 0.05)

  labs_f <- tibble(
    p = c("05", "50", "95"),
    value = c(f_perc$p0.05, f_perc$p0.5, f_perc$p0.95),
    x = 0.75 * max(nd_re$age_or_length), # re-calculated below
    y = seq(0.75, 0.6, length.out = 3L),
    sex = "F"
  )

  labs_m <- tibble(
    p = c("05", "50", "95"),
    value = c(m_perc$p0.05, m_perc$p0.5, m_perc$p0.95),
    x = 0.75 * max(nd_re$age_or_length), # re-calculated below
    y = seq(0.4, 0.25, length.out = 3),
    sex = "M"
  )
  labs <- bind_rows(labs_m, labs_f)

  if (prediction_type == "male") {
    labs <- filter(labs, sex == "M")
  }
  if (prediction_type == "female") {
    labs <- filter(labs, sex == "F")
  }

  nd_fe <- mutate(nd_fe, sex = ifelse(female == 1L, "F", "M"))
  nd_re <- mutate(nd_re, sex = ifelse(female == 1L, "F", "M"))
  object$data <- mutate(object$data, sex = ifelse(female == 1L, "F", "M"))

  if (object$type[[1]] == "age") {
    labs <- mutate(labs,
      label =
        paste0(
          sex, " ", p, " = ",
          sprintf("%.1f", round(value, 1L)), en2fr("y", translate = french)
        )
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

  nd_fe$sex <- factor(nd_fe$sex, levels = c("F", "M"))
  nd_re$sex <- factor(nd_re$sex, levels = c("F", "M"))
  labs$sex <- factor(labs$sex, levels = c("F", "M"))
  labs$sex <- factor(labs$sex, levels = c("F", "M"))

  g <- ggplot(nd_fe, aes_string("age_or_length", "glmm_fe", colour = "sex", lty = "sex"))

  if ("glmm_re" %in% names(nd_re)) {
    if (object$sample_id_re) {
      g <- g + geom_line(
        data = nd_re,
        aes_string("age_or_length", "glmm_re",
          group = "paste(sample_id, sex)",
          colour = "sex"
        ), inherit.aes = FALSE, alpha = 0.05
      )
    } else {
      if (object$year_re) {
        g <- g + geom_line(
          data = nd_re,
          aes_string("age_or_length", "glmm_re",
            group = "paste(year, sex)",
            colour = "sex"
          ), inherit.aes = FALSE, alpha = 0.05
        )
      }
    }
  }

  if (prediction_type != "none") {
    g <- g + geom_vline(
      data = filter(labs, p == "50"),
      aes_string(xintercept = "value", colour = "sex", lty = "sex"), lwd = 0.8,
      alpha = 0.6, show.legend = FALSE
    )
    g <- g + geom_line(size = 1.0)
    g <- g + geom_text(
      data = labs, aes_string(
        x = "x", y = "y", label = "label"
      ),
      hjust = 0, show.legend = FALSE, size = 3
    )
  }

  g <- g + scale_colour_manual(
    values = c("M" = "grey50", "F" = "black"),
    breaks = c("F", "M"), drop = FALSE
  ) +
    ggplot2::scale_linetype_discrete(breaks = c("F", "M"), drop = FALSE) +
    xlab(xlab) + ylab("Probability mature") +
    theme_pbs() +
    coord_cartesian(
      expand = FALSE, ylim = c(-0.005, 1.005),
      xlim = c(0, max_x)
    ) +
    labs(colour = "Sex", lty = "Sex") +
    ggplot2::ggtitle(title)

  if (rug) {
    if (nrow(object$data) > rug_n) {
      temp <- object$data[sample(seq_len(nrow(object$data)), rug_n), , drop = FALSE]
    } else {
      temp <- object$data
    }
    position <- if (object$type == "age") "jitter" else "identity"
    g <- g + ggplot2::geom_rug(
      data = filter(temp, mature == 0L),
      sides = "b", position = position, alpha = 0.5, lty = 1,
      aes_string(x = "age_or_length", y = "as.numeric(mature)", colour = "sex")
    )
    g <- g + ggplot2::geom_rug(
      data = filter(temp, mature == 1L),
      sides = "t", position = position, alpha = 0.5, lty = 1,
      aes_string(x = "age_or_length", y = "as.numeric(mature)", colour = "sex")
    )
  }

  g
}

extract_maturity_perc <- function(object) {
  m.p0.5 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.5)
  m.p0.95 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.95)
  m.p0.05 <- logit_perc(a = object[[1]], b = object[[2]], perc = 0.05)

  f.p0.5 <- logit_perc(
    a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.5
  )
  f.p0.95 <- logit_perc(
    a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.95
  )
  f.p0.05 <- logit_perc(
    a = object[[1]] + object[[3]],
    b = object[[2]] + object[[4]], perc = 0.05
  )
  list(
    m.p0.5 = m.p0.5, m.p0.95 = m.p0.95, m.p0.05 = m.p0.05, f.p0.5 = f.p0.5,
    f.p0.95 = f.p0.95, f.p0.05 = f.p0.05
  )
}

extract_maturity_perc_re <- function(betas, random_intercepts) {
  b <- betas
  re <- random_intercepts
  out <- list()
  for (i in rownames(re)) {
    m.p0.5 <- logit_perc(a = b[[1]] + re[i, ], b = b[[2]], perc = 0.5)
    m.p0.95 <- logit_perc(a = b[[1]] + re[i, ], b = b[[2]], perc = 0.95)
    m.p0.05 <- logit_perc(a = b[[1]] + re[i, ], b = b[[2]], perc = 0.05)

    f.p0.5 <- logit_perc(a = b[[1]] + b[[3]] + re[i, ], b = b[[2]] + b[[4]], perc = 0.5)
    f.p0.95 <- logit_perc(a = b[[1]] + b[[3]] + re[i, ], b = b[[2]] + b[[4]], perc = 0.95)
    f.p0.05 <- logit_perc(a = b[[1]] + b[[3]] + re[i, ], b = b[[2]] + b[[4]], perc = 0.05)
    out[[i]] <- list(
      m.p0.5 = m.p0.5, m.p0.95 = m.p0.95, m.p0.05 = m.p0.05,
      f.p0.5 = f.p0.5, f.p0.95 = f.p0.95, f.p0.05 = f.p0.05
    )
  }
  m.mean.p0.5 <- logit_perc(a = b[[1]], b = b[[2]], perc = 0.5)
  f.mean.p0.5 <- logit_perc(
    a = b[[1]] + b[[3]],
    b = b[[2]] + b[[4]], perc = 0.5
  )
  # nrow(re) + 1
  out[["mean"]] <- list(m.mean.p0.5 = m.mean.p0.5, f.mean.p0.5 = f.mean.p0.5)
  out
}

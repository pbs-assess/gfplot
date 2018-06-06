#' Convert groundfish PBS data to a DLMtool data object
#'
#' Takes the output from [cache_pbs_data()] and converts it into a DLMtool data
#' object.
#'
#' @param dat A list object output from [cache_pbs_data()].
#' @param name A name for the stock.
#' @param area The groundfish and statistical area to subset the catch by.
#' @param survey A survey abbreviation to include as the relative index of
#'   abundance.
#' @param max_year The most recent year of data to include.
#' @param min_mean_length The minimum number of samples to include a mean length
#'   measurement for a given year.
#' @param length_bin_interval An interval for the length bins.
#'
#' @importClassesFrom DLMtool Data
#' @return An S4 object of class DLMtool Data.
#' @export
#'
#' @examples
#' \dontrun{
#' cache_pbs_data("pacific ocean perch", unsorted_only = FALSE)
#' d <- readRDS("pacific-ocean-perch.rds")
#' pbs2dlmtool_data(d)
#' }

# dat <- readRDS("../gfsynopsis/report/data-cache3/pacific-ocean-perch.rds")

pbs2dlmtool_data <- function(dat, name = "", area = "3[CD]+",
                             survey = "SYN WCVI", max_year = 2017,
                             min_mean_length = 10,
                             length_bin_interval = 2) {

  # Setup ----------
  obj <- methods::new("Data")
  obj@Name <- name
  obj@Units <- "kg"

  dat$commercial_samples <- filter(dat$commercial_samples, year <= max_year)
  dat$survey_samples <- filter(dat$survey_samples, year <= max_year)
  dat$catch <- filter(dat$catch, year <= max_year)
  dat$survey_index <- filter(dat$survey_index, year <= max_year)

  # Catch ----------
  catch <- tidy_catch(dat$catch, areas = area)
  catch <- catch %>%
    group_by(year) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  last_year <- max_year
  ind <- tidy_survey_index(dat$survey_index, survey = survey)
  all_years <- tibble(year = seq(min(c(catch$year, ind$year)), last_year))
  catch <- left_join(all_years, catch, by = "year")

  obj@Cat <- t(matrix(catch$value))
  obj@Year <- catch$year
  obj@t <- length(obj@Cat)
  obj@AvC <- mean(obj@Cat, na.rm = TRUE)
  obj@CV_Cat <- mean(catch$value, na.rm = TRUE) / sd(catch$value, na.rm = TRUE)

  # Index of abundance ----------
  ind <- left_join(all_years, ind, by = "year")
  ind <- t(matrix(ind$biomass))
  obj@Ind <- ind / mean(ind, na.rm = TRUE) # standardise

  # Maturity ----------
  samps <- bind_samples(
    dat_comm = dat$commercial_samples,
    dat_survey = dat$survey_samples
  )
  m_mat <- fit_mat_ogive(samps, type = "length")
  mat_perc <- extract_maturity_perc(stats::coef(m_mat$model))
  se_l50 <- delta_method(~ -(log((1/0.5) - 1) + x1 + x3) / (x2 + x4),
    mean = stats::coef(m_mat$model), cov = stats::vcov(m_mat$model))

  obj@L50 <- mat_perc$f.p0.5 # TODO, female only?
  obj@L95 <- mat_perc$f.p0.95
  obj@CV_L50 <- se_l50 / obj@L50

  # VB model ----------
  mvb <- fit_vb(samps, sex = "female")
  fit <- mvb$model
  se <- sqrt(diag(solve(-fit$hessian)))
  cv <- stats::setNames(se, names(fit$par)) / abs(fit$par)

  obj@vbK <- mvb$model$par[["k"]]
  obj@vbLinf <- mvb$model$par[["linf"]]
  obj@vbt0 <- mvb$model$par[["t0"]]
  obj@LenCV <- sd2cv(mvb$model$par[["sigma"]])
  obj@CV_vbK <- cv[["k"]]
  obj@CV_vbLinf <- cv[["linf"]]
  obj@CV_vbt0 <- cv[["t0"]]

  # Length weight model ----------
  mlw <- fit_length_weight(samps, sex = "female")
  se <- summary(mlw$model)$coefficients[, "Std. Error"]

  obj@wla <- exp(stats::coef(mlw$model)[[1L]])
  obj@wlb <- stats::coef(mlw$model)[[2L]]
  obj@CV_wla <- sd2cv(se[["(Intercept)"]]) # log scale
  obj@CV_wlb <- se[["log(length)"]] / stats::coef(mlw$model)[[2L]]

  # Mean length timeseries ----------
  # TODO commercial; unsorted only?
  ml <- tidy_mean_length(dat$commercial_samples) %>%
    filter(n > min_mean_length, year <= max_year) %>%
    right_join(all_years, by = "year")

  obj@ML <- t(matrix(ml$mean_length))

  # Catch at age ----------
  obj@CAA <- tidy_caa(dat$commercial_samples, yrs = all_years$year)
  obj@MaxAge <- ncol(obj@CAA[1, , ])

  # Catch at length ----------
  # CAL Catch-at-length data. An array with dimensions nsim x nyears
  # x length(CAL_bins). Non-negative integers
  length_bins <- seq(0, 1e4, length_bin_interval)
  obj@CAL <- dat$commercial_samples %>%
    select(-age) %>%
    mutate(length_bin = length_bins[findInterval(length, length_bins)]) %>%
    rename(age = length_bin) %>% # hack
    tidy_caa(yrs = all_years$year, interval = length_bin_interval)
  # CAL_bins The values delimiting the length bins for the catch-at-length
  # data. Vector. Non-negative real numbers
  obj@CAL_bins <- seq(length_bin_interval, ncol(obj@CAL[1, , ]) *
    length_bin_interval) - length_bin_interval / 2 # mid points

  obj
}

# Generate mean-length time series
#
# @param dat Commercial or biological samples from [get_commercial_samples()] or
#   [get_survey_samples()].
# @param unsorted_only Logical for whether to only include the unsorted samples.
#   Only applies to the commercial data.
tidy_mean_length <- function(dat, unsorted_only = FALSE) {
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  if ("sampling_desc" %in% names(dat) && unsorted_only) {
    dat <- filter(dat, sampling_desc == "UNSORTED")
  }
  dat <- filter(dat, !is.na(sex), !is.na(length), sex %in% 2) # female only
  group_by(dat, year) %>%
    summarise(n = n(), mean_length = mean(length)) %>%
    ungroup()
}

# Generate catch-at-age or catch-at-length data
#
# @param dat Commercial or biological samples from [get_commercial_samples()] or
#   [get_survey_samples()].
# @param yrs A complete set of years to include in the matrix.
# @param unsorted_only Logical for whether to only include the unsorted samples.
#   Only applies to the commercial data.
# @param interval Interval for the complete set of ages or lengths. For example,
#   for length bins of interval 2, `interval = 2`.
#
# @return A catch at age or catch at length matrix as an array.
#   1 x nyears x nage/nlength
tidy_caa <- function(dat, yrs, unsorted_only = FALSE, interval = 1) {
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  if ("sampling_desc" %in% names(dat) && unsorted_only) {
    dat <- filter(dat, sampling_desc == "UNSORTED")
  }
  dat <- filter(dat, !is.na(sex), !is.na(age), sex %in% 2) # female only

  caa <- group_by(dat, year, age) %>%
    summarise(N = n()) %>%
    ungroup()

  caa <- left_join(
    expand.grid(age = seq(0, max(caa$age), interval), year = unique(caa$year)),
    caa,
    by = c("age", "year")
  )
  caa$N <- ifelse(is.na(caa$N), 0, caa$N)
  caa <- left_join(
    expand.grid(age = seq(0, max(caa$age), interval), year = yrs),
    caa,
    by = c("age", "year")
  )
  caa <- reshape2::dcast(caa, year ~ age, value.var = "N")[, -1L]
  array(as.numeric(as.matrix(caa)),
    dim = c(1L, nrow(caa), ncol(caa))
  ) # nsim x nyears x MaxAge
}

delta_method <- function(g, mean, cov) {
  # simplified from msm::deltamethod
  cov <- as.matrix(cov)
  n <- length(mean)
  g <- list(g)
  syms <- paste0("x", seq_len(n))
  for (i in seq_len(n)) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  sqrt(diag(new.covar))
}

sd2cv <- function(.sd) {
  sqrt(exp(.sd^2) - 1)
}

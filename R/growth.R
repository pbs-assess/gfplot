#' Fit growth TODO
#'
#' For use with data for a single species.
#'
#' @param dat TODO
#' @param sex TODO
#' @param method TODO
#' @param downsample TODO
#' @param chains TODO
#' @param iter TODO
#' @param cores TODO
#' @param allow_slow_mcmc TODO
#' @param est_method TODO
#' @param min_samples TODO
#' @param ageing_method TODO
#' @param ... TODO
#' @family growth functions
#' @importFrom stats median quantile rlnorm runif median
#'
#' @export
#' @examples
#' \dontrun{
#' ## with `rstan::optimizing()` for the mode of the posterior density:
#' x <- fit_vb(pop_samples, method = "mpd")
#' x$model
#'
#' ## with MCMC via Stan:
#' x <- fit_vb(pop_samples, method = "mcmc")
#' x$pars
#' x$predictions
#' x$data
#' x$model
#' posterior <- rstan::extract(x$model)
#' hist(posterior$linf)
#'
#' model_f <- fit_vb(pop_samples, sex = "female")
#' model_m <- fit_vb(pop_samples, sex = "male")
#' plot_vb(model_f, model_m)
#'
#' ## If less than `min_samples`, fit_vb() returns an empty object that
#' ## plot_vb() will correctly parse and produce an empty plot:
#' obj <- fit_vb(pop_samples[1:2,])
#' plot_vb(obj, obj)
#' }

fit_vb <- function(dat,
  sex = c("female", "male"),
  method = c("mpd", "mcmc"),
  downsample = Inf,
  chains = 4L,
  iter = 1000L,
  cores = parallel::detectCores(),
  allow_slow_mcmc = FALSE,
  est_method = median,
  min_samples = 50L,
  ageing_method = c(3, 17),
  ...) {

  if ("species_common_name" %in% names(dat))
    if (length(unique(dat$species_common_name)) != 1L)
      stop("Multiple species detected via the `species_common_name` column. ",
        "fit_vb() is for use with a single species. Filter the data yourself ",
        "first.", call. = FALSE)

  dat <- filter(dat, .data$ageing_method %in% ageing_method)
  dat <- dat[!duplicated(dat), , drop = FALSE]
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$age))

  dat <- switch(sex[[1]],
    "female" = filter(dat, sex == 2),
    "male" = filter(dat, sex == 1),
    stop("`sex` argument must be 'female' or 'male'.", call. = FALSE))

  if (nrow(dat) < min_samples) {
    return(list(
      predictions = tibble(ages = NA, length = NA),
      pars = list(k = NA, linf = NA, t0 = NA),
      data = dat,
      model = NA))
  }

  rstan::rstan_options(auto_write = TRUE)
  model_file <- system.file("stan", "vb.stan", package = "PBSsynopsis")
  mod <- rstan::stan_model(model_file)

  if (nrow(dat) > downsample)
    dat <- dat[sample(seq_len(nrow(dat)), downsample), , drop = FALSE]

  if (method[[1]] == "mpd") {

    vb_starts <- FSA::vbStarts(length ~ age, data = dat)
    mpd_init <- list(
      k = rlnorm(1, log(vb_starts$K), 0.01),
      linf = rlnorm(1, log(vb_starts$Linf), 0.01),
      t0 = rnorm(1, vb_starts$t0, 0.02),
      sigma = runif(1, 0.1, 0.2))

    m <- suppressMessages(rstan::optimizing(mod,
      data = list(
        N = nrow(dat), age = dat$age, length = dat$length,
        linf_upper_sd = quantile(dat$length, 0.99)[[1]] * 2),
      init = mpd_init, ...))

    if (m$return_code != 0L)
      warning("VB growth model did not converge!")

    pars <- as.list(m$par)
  }
  if (method[[1]] == "mcmc") {

    if (nrow(dat) > 50000 & !allow_slow_mcmc)
      stop("There are > 50,000 aged fish. ",
        "MCMC sampling may take a long time. Set `allow_slow_mcmc = TRUE` ",
        "if you still want to sample from the posterior.", call. = FALSE)

    m <- rstan::sampling(mod,
      data = list(
        N = nrow(dat), age = dat$age, length = dat$length,
        linf_upper_sd = quantile(dat$length, 0.99)[[1]] * 2),
      chains = chains, iter = iter, cores = cores, ...)

    pars <- lapply(rstan::extract(m), est_method)
  }

  vb <- function(ages, linf, k, t0) linf * (1 - exp(-k * (ages - t0)))
  ages <- seq(min(dat$age), max(dat$age), length.out = 200L)
  pred <- vb(ages, linf = pars$linf, k = pars$k, t0 = pars$t0)

  list(predictions = tibble(age = ages, length = pred),
    pars = pars, data = as_tibble(dat), model = m)
}

#' Fit length-weight TODO
#'
#' For use with data for a single species.
#'
#' @param dat TODO
#' @param sex TODO
#' @param downsample TODO
#' @param min_samples TODO
#' @param method TODO
#' @param scale_weight TODO
#'
#' @family growth functions
#' @export
#' @examples
#' model_f <- fit_length_wt(pop_samples)
#' model_f$model
#' model_f$predictions
#' model_f$pars
#' model_f$data
#'
#' model_m <- fit_length_wt(pop_samples, sex = "male")
#' plot_length_wt(model_f, model_m)

fit_length_wt <- function(dat,
  sex = c("female", "male"),
  downsample = Inf,
  min_samples = 50L,
  method = c("rlm", "lm"),
  scale_weight = 1 / 1000) {

  if ("species_common_name" %in% names(dat))
    if (length(unique(dat$species_common_name)) != 1L)
      stop("Multiple species detected via the `species_common_name` column. ",
        "fit_vb() is for use with a single species. Filter the data yourself ",
        "first.", call. = FALSE)

  dat <- dat[!duplicated(dat), , drop = FALSE]
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$weight))

  dat <- switch(sex[[1]],
    "female" = filter(dat, sex == 2),
    "male" = filter(dat, sex == 1),
    stop("`sex` argument must be 'female' or 'male'.", call. = FALSE))

  dat$weight <- dat$weight * scale_weight

  if (nrow(dat) < min_samples) {
    return(list(
      predictions = tibble(length = NA, weight = NA),
      pars = list(log_a = NA, b = NA),
      data = dat,
      model = NA))
  }

  m <- switch(method[[1]],
    "rlm" = MASS::rlm(log(weight) ~ log(length), data = dat),
    "lm" = stats::lm(log(weight) ~ log(length), data = dat),
    stop("`method` argument must be 'rlm' or 'lm'.", call. = FALSE))

  pred <- tibble(length = seq(min(dat$length), max(dat$length), length.out = 200L))
  pred$weight <- exp(predict(m, newdata = pred))

  pars <- as.list(coef(m))
  pars <- stats::setNames(pars, c("log_a", "b"))

  list(predictions = pred, pars = pars, data = as_tibble(dat), model = m)
}

#' Plot VB or length-weight fits TODO
#'
#' @param object_female TODO
#' @param object_male TODO
#' @param type TODO
#' @param downsample TODO
#' @param pt_alpha TODO
#' @param xlab TODO
#' @param ylab TODO
#' @param seed TODO
#' @param lab_x TODO
#' @param lab_y TODO
#' @param lab_x_gap TODO
#' @param lab_y_gap TODO
#' @param col TODO
#'
#' @export
#' @family growth functions
#' @rdname plot_growth
#'
#' @examples
#' \dontrun{
#' model_f <- fit_vb(pop_samples, sex = "female")
#' model_m <- fit_vb(pop_samples, sex = "male")
#' plot_vb(model_f, model_m)
#' }
#'
#' model_f <- fit_length_wt(pop_samples, sex = "female")
#' model_m <- fit_length_wt(pop_samples, sex = "male")
#' plot_length_wt(model_f, model_m)

plot_growth <- function(object_female, object_male,
  type = c("vb", "length-weight"),
  downsample = 2000L,
  pt_alpha = 0.2,
  xlab = "Age (years)",
  ylab = "Length (cm)",
  seed = 42,
  lab_x = 0.4,
  lab_y = 0.3,
  lab_x_gap = 0.3,
  lab_y_gap = 0.08,
  col = c("Female" = "red", "Male" = "grey20")) {

  xvar <- if (type[[1]] == "vb") "age" else "length"
  yvar <- if (type[[1]] == "vb") "length" else "weight"

  line_dat <- bind_rows(
    data.frame(object_female$predictions, sex = "Female",
      stringsAsFactors = FALSE),
    data.frame(object_male$predictions, sex = "Male",
      stringsAsFactors = FALSE))

  no_lines <- if (all(is.na(line_dat[[1]]))) TRUE else FALSE

  pt_dat <- bind_rows(object_female$data, object_male$data)
  pt_dat$sex <- dplyr::case_when(
    pt_dat$sex == 1 ~ "Male",
    pt_dat$sex == 2 ~ "Female",
    TRUE ~ "")
  no_pts <- if (nrow(pt_dat) == 0L) TRUE else FALSE

  xdat <- if (type[[1]] == "vb") pt_dat$age else pt_dat$length
  ydat <- if (type[[1]] == "vb") pt_dat$length else pt_dat$weight

  if (nrow(pt_dat) > downsample) {
    if (!is.null(seed)) set.seed(seed)
    pt_dat <- pt_dat[sample(seq_len(nrow(pt_dat)), downsample), , drop = FALSE]
  }

  g <- ggplot() +
    scale_colour_manual(values = col) +
    theme_pbs() + xlab(xlab) + ylab(ylab) +
    guides(colour = FALSE)

  if (!no_pts)
    g <- g + geom_point(data = pt_dat, aes_string(xvar, yvar),
      alpha = pt_alpha, colour = "grey50") +
    coord_cartesian(
      xlim = range(xdat),
      ylim = c(0, max(ydat)),
      expand = FALSE)

  if (!no_lines)
    g <- g + geom_line(data = line_dat, aes_string(xvar, yvar,
      colour = "sex"), size = 1)

  ann_func <- if (type[[1]] == "vb") ann_vb else ann_lw
  if (!is.na(object_female$pars[[1]])) {
    g <- ann_func(g, object_female$pars, "Females", col[["Female"]],
      x = lab_x * max(xdat),
      y = lab_y * max(ydat),
      gap = lab_y_gap * max(ydat))
  }

  if (!is.na(object_male$pars[[1]])) {
    g <- ann_func(g, object_male$pars, "Males", col[["Male"]],
      x = (lab_x + lab_x_gap) * max(xdat),
      y = lab_y * max(ydat),
      gap = lab_y_gap * max(ydat))
  }

  g
}

#' @inheritParams plot_growth
#' @param ... TODO
#' @export
#' @rdname plot_growth
plot_vb <- function(..., type = "vb") {
  plot_growth(..., type = type) +
    ggplot2::ggtitle("Growth")
}

#' @inheritParams plot_vb
#' @inheritParams plot_growth
#' @export
#' @rdname plot_growth
plot_length_wt <- function(..., type = "length-weight", xlab = "Length (cm)",
    ylab = "Weight (kg)", lab_x = 0.2, lab_y = 0.9, lab_x_gap = 0.25) {
  plot_growth(..., type = type, xlab = xlab,
    ylab = ylab, lab_x = lab_x, lab_y = lab_y, lab_x_gap = lab_x_gap) +
    ggplot2::ggtitle("Length-weight")
}

# annotation helpers:
ann_vb <- function(gg, pars, title, col, x, y, gap) {
  gg + ggplot2::annotate("text", label = title,
    x = x, y = y, hjust = 0, col = col) +
    ann("k", pars[["k"]], dec = 2, x, y - gap) +
    ann("linf", pars[["linf"]], dec = 1, x, y - gap * 2) +
    ann("t0", pars[["t0"]], dec = 2, x, y - gap * 3)
}

ann_lw <- function(gg, pars, title, col, x, y, gap) {
  gg + ggplot2::annotate("text", label = title,
    x = x, y = y, hjust = 0, col = col) +
    ann("log(a)", pars[["log_a"]], dec = 2, x, y - gap) +
    ann("b", pars[["b"]], dec = 2, x, y - gap * 2)
}

ann <- function(par_name, par_val, dec, x, y, col = "grey10") {
  ggplot2::annotate("text",
    label = paste0(
      par_name, " = ",
      sprintf(
        paste0("%.", dec, "f"),
        round(par_val, dec))),
    x = x,
    y = y,
    hjust = 0,
    col = col)
}

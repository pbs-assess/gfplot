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
#' @param ... TODO
#' @family growth functions
#' @importFrom stats median quantile rlnorm runif median
#'
#' @export
# TODO:
# @examples
# d <- readRDS("data-cache/pbs-survey-specimens.rds")
# d <- dplyr::filter(d, species_common_name == "pacific ocean perch")
# obj <- fit_vb(d, method = "mpd")
# obj$model
# obj <- fit_vb(d, method = "mcmc", downsample = 1000, chains = 3)
# obj$model
# obj$pars
# obj$predictions
# obj$data
#
# obj_f <- fit_vb(d, sex = "female")
# obj_m <- fit_vb(d, sex = "male")
# plot_vb(obj_f, obj_m)
#
## If less than `min_samples`, fit_vb() returns
## an empty object that plot_vb() will correctly parse
## and produce an empty plot:
# obj <- fit_vb(d[1:2,], method = "mpd")
# plot_vb(obj, obj)

fit_vb <- function(dat,
  sex = c("female", "male"),
  method = c("mpd", "mcmc"),
  downsample = Inf,
  chains = 4L,
  iter = 1000L,
  cores = parallel::detectCores(),
  allow_slow_mcmc = FALSE,
  est_method = median,
  min_samples = 100L,
  ...) {

  if ("species_common_name" %in% names(dat))
    if (length(unique(dat$species_common_name)) != 1L)
      stop("Multiple species detected via the `species_common_name` column. ",
        "fit_vb() is for use with a single species. Filter the data yourself ",
        "first.", call. = FALSE)

  dat <- dat[!duplicated(dat), , drop = FALSE]
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$age))

  dat <- switch(sex[[1]],
    "female" = filter(dat, sex == 2),
    "male" = filter(dat, sex == 1))

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


#' Plot growth TODO
#'
#' @param object_female TODO
#' @param object_male TODO
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
#'
# @examples
plot_vb <- function(object_female, object_male, downsample = 2000L,
  pt_alpha = 0.2, xlab = "Age (years)", ylab = "Length (cm)",
  seed = 42, lab_x = 0.4, lab_y = 0.3, lab_x_gap = 0.3, lab_y_gap = 0.08,
  col = c("Female" = "red", "Male" = "black")) {

  line_dat <- bind_rows(
    data.frame(object_female$predictions, sex = "Female",
      stringsAsFactors = FALSE),
    data.frame(object_male$predictions, sex = "Male",
      stringsAsFactors = FALSE))
  no_lines <- if (all(is.na(line_dat$age))) TRUE else FALSE

  pt_dat <- bind_rows(object_female$data, object_male$data)
  pt_dat$sex <- dplyr::case_when(
    pt_dat$sex == 1 ~ "Male",
    pt_dat$sex == 2 ~ "Female",
    TRUE ~ "")
  no_pts <- if (nrow(pt_dat) == 0L) TRUE else FALSE

  if (nrow(pt_dat) > downsample) {
    if (!is.null(seed)) set.seed(seed)
    pt_dat <- pt_dat[sample(seq_len(nrow(pt_dat)), downsample), , drop = FALSE]
  }

  g <- ggplot() +
    scale_colour_manual(values = col) +
    theme_pbs() + xlab(xlab) + ylab(ylab) +
    guides(colour = FALSE)

  if (!no_pts)
    g <- g + geom_point(data = pt_dat, aes_string("age", "length"),
      alpha = pt_alpha, colour = "grey50") +
    coord_cartesian(
      xlim = range(pt_dat$age),
      ylim = c(0, max(pt_dat$length)), expand = FALSE)

  if (!no_lines)
    g <- g + geom_line(data = line_dat, aes_string("age", "length",
      colour = "sex"), size = 1)

  if (!is.na(object_female$pars$linf)) {
    g <- ann_vb(g, object_female$pars, "Females", col[["Female"]],
      x = lab_x * max(pt_dat$age),
      y = lab_y * max(pt_dat$length),
      gap = lab_y_gap * max(pt_dat$length))
  }

  if (!is.na(object_male$pars$linf)) {
    g <- ann_vb(g, object_male$pars, "Males", col[["Male"]],
      x = (lab_x + lab_x_gap) * max(pt_dat$age),
      y = lab_y * max(pt_dat$length),
      gap = lab_y_gap * max(pt_dat$length))
  }

  g
}

ann_vb <- function(gg, pars, title, col, x, y, gap) {
  gg + ggplot2::annotate("text", label = title,
    x = x, y = y, hjust = 0, col = col) +
    ann("k", pars[["k"]], dec = 2, x, y - gap) +
    ann("linf", pars[["linf"]], dec = 1, x, y - gap * 2) +
    ann("t0", pars[["t0"]], dec = 2, x, y - gap * 3)
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

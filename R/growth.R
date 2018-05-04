#' Fit a von Bertalanffy growth model
#'
#' For use with data for a single species.
#'
#' @param dat Input data frame. Should be from [get_survey_samples()] or
#'   [get_comm_samples()].
#' @param sex Either "male" or "female".
#' @param method `"mpd"` for the mode of the posterior distribution (with
#'   [rstan::optimizing()]) or `"mcmc"` for full MCMC sampling with Stan (with
#'   [rstan::sampling()]).
#' @param downsample If not `Inf` this represents a number of fish specimens to
#'   sample prior to model fitting. Can be useful for large data sets that you
#'   want to fit with MCMC for testing purposes.
#' @param chains Number of Stan chains.
#' @param iter Number of Stan sampling iterations.
#' @param cores Number of cores for Stan.
#' @param allow_slow_mcmc Logical. If `TRUE` then the function will let you fit
#'   with MCMC to any number of fish. Defaults to `FALSE` to avoid accidentally
#'   fitting a model to a giant data set (stop if number of fish > 50,000).
#' @param est_method If MCMC this defines how to summarize the posterior. Should
#'   be a function such as `mean` or `median`.
#' @param min_samples The minimum number of fish before a model will be fit.
#' @param uniform_priors Logical. If true then uniform priors will be used.
#' @param ageing_method_codes A numeric vector of ageing method codes to filter
#'   on. Defaults to `NULL`, which brings in all valid ageing codes. See
#'   [get_age_methods()].
#' @param ... Any other arguments to pass on to [rstan::sampling()] or
#'   [rstan::optimizing()].
#' @family growth functions
#' @importFrom stats median quantile rlnorm runif median
#'
#' @details Note that in some cases you must load the rstan package first and
#'   you may choose to do so just in case. If the rstan package is not loaded
#'   first, you may get the warning:
#'
#' `Error in cpp_object_initializer(.self, .refClassDef, ...) :`
#' `could not find function "cpp_object_initializer"`
#'
#' @export
#' @examples
#' library(rstan) # must load first
#' # with `rstan::optimizing()` for the mode of the posterior density:
#' model_f <- fit_vb(pop_samples, sex = "female")
#' model_m <- fit_vb(pop_samples, sex = "male")
#' plot_vb(model_f, model_m)
#' model_f$model
#' model_f$predictions
#'
#' # with MCMC via Stan (slower):
#' x <- fit_vb(pop_samples, method = "mcmc",
#'   chains = 1, iter = 800) # just for a fast example
#' x$pars
#' x$predictions
#' x$data
#' x$model
#' posterior <- rstan::extract(x$model)
#' hist(posterior$linf)
#'
#' # If less than `min_samples`, fit_vb() returns an empty object that
#' # plot_vb() will correctly parse and produce an empty plot:
#' obj <- fit_vb(pop_samples[1:2,])
#' plot_vb(obj, obj)

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
                   too_high_quantile = 1.0,
                   uniform_priors = FALSE,
                   ageing_method_codes = NULL,
                   ...) {
  if ("species_common_name" %in% names(dat)) {
    if (length(unique(dat$species_common_name)) != 1L) {
      stop("Multiple species detected via the `species_common_name` column. ",
        "fit_vb() is for use with a single species. Filter the data yourself ",
        "first.",
        call. = FALSE
      )
    }
  }

  if (!is.null(ageing_method_codes)) {
    dat <- filter(dat, .data$ageing_method %in% ageing_method_codes)
  }
  dat <- dat[!duplicated(dat), , drop = FALSE] # critical
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$age))
  ql <- quantile(dat$length, probs = too_high_quantile)
  dat <- filter(dat, length <= ql)

  sex <- match.arg(sex)
  dat <- switch(sex,
    "female" = filter(dat, sex == 2L),
    "male" = filter(dat, sex == 1L)
  )

  if (nrow(dat) < min_samples) {
    return(list(
      predictions = tibble(
        ages = NA, length = NA
      ),
      pars = list(k = NA, linf = NA, t0 = NA),
      data = dat,
      model = NA
    ))
  }

  rstan::rstan_options(auto_write = TRUE)
  if (uniform_priors) {
    model_file <- system.file("stan", "vb-nopriors.stan", package = "gfplot")
  } else {
    model_file <- system.file("stan", "vb.stan", package = "gfplot")
  }

  mod <- rstan::stan_model(model_file)

  if (nrow(dat) > downsample) {
    dat <- dat[sample(seq_len(nrow(dat)), downsample), , drop = FALSE]
  }

  if (method[[1]] == "mpd") {
    vb_starts <- FSA::vbStarts(length ~ age, data = dat)
    mpd_init <- list(
      k = rlnorm(1, log(vb_starts$K), 0.01),
      linf = rlnorm(1, log(vb_starts$Linf), 0.01),
      t0 = rnorm(1, vb_starts$t0, 0.02),
      sigma = runif(1, 0.1, 0.2)
    )

    m <- suppressMessages(rstan::optimizing(mod,
      data = list(
        N = nrow(dat), age = dat$age, length = dat$length,
        linf_upper_sd = quantile(dat$length, 0.99)[[1]] * 2
      ),
      init = mpd_init, ...
    ))

    if (m$return_code != 0L) {
      warning("VB growth model did not converge!")
    }

    pars <- as.list(m$par)
  }
  if (method[[1]] == "mcmc") {
    if (nrow(dat) > 50000 & !allow_slow_mcmc) {
      stop("There are > 50,000 aged fish. ",
        "MCMC sampling may take a long time. Set `allow_slow_mcmc = TRUE` ",
        "if you still want to sample from the posterior.",
        call. = FALSE
      )
    }

    m <- rstan::sampling(mod,
      data = list(
        N = nrow(dat), age = dat$age, length = dat$length,
        linf_upper_sd = quantile(dat$length, 0.99)[[1]] * 2
      ),
      chains = chains, iter = iter, cores = cores, ...
    )

    pars <- lapply(rstan::extract(m), est_method)
  }

  vb <- function(ages, linf, k, t0) linf * (1 - exp(-k * (ages - t0)))
  ages <- seq(min(dat$age), max(dat$age), length.out = 200L)
  pred <- vb(ages, linf = pars$linf, k = pars$k, t0 = pars$t0)

  list(
    predictions = tibble(age = ages, length = pred),
    pars = pars, data = as_tibble(dat), model = m
  )
}

#' Fit a length-weight model
#'
#' For use with data for a single species.
#'
#' @param dat Input data frame. Should be from [get_survey_samples()] or
#'   [get_comm_samples()].
#' @param sex Either "male" or "female".
#' @param downsample If not `Inf` this represents a number of fish specimens to
#'   sample prior to model fitting. Can be useful for large data sets that you
#'   want to fit with MCMC for testing purposes.
#' @param min_samples The minimum number of fish before a model will be fit.
#' @param method `"rlm"` for [MASS::rlm()] or `"lm"` for [stats::lm()].
#' @param too_high_quantile A quantile above which to discard weights and
#'   lengths. Can be useful for outliers. Defaults to including all data.
#' @param est_method If MCMC this defines how to summarize the posterior. Should
#'   be a function such as `mean` or `median`.
#' @param scale_weight A value to multiply all weights by. Useful for changing
#'   units.
#'
#' @family growth functions
#' @export
#' @examples
#' \dontrun{
#' d <- get_survey_samples("pacific ocean perch")
#' model_f <- fit_length_weight(d, sex = "female")
#' model_m <- fit_length_weight(d, sex = "male")
#' plot_length_weight(object_female = model_f, object_male = model_m)
#' }

fit_length_weight <- function(dat,
                              sex = c("female", "male"),
                              downsample = Inf,
                              min_samples = 50L,
                              method = c("rlm", "lm"),
                              too_high_quantile = 1.0,
                              scale_weight = 1 / 1000) {
  if ("species_common_name" %in% names(dat)) {
    if (length(unique(dat$species_common_name)) != 1L) {
      stop("Multiple species detected via the `species_common_name` column. ",
        "fit_vb() is for use with a single species. Filter the data yourself ",
        "first.",
        call. = FALSE
      )
    }
  }

  dat <- dat[!duplicated(dat), , drop = FALSE]

  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$weight))
  ql <- quantile(dat$length, probs = too_high_quantile)
  qw <- quantile(dat$weight, probs = too_high_quantile)
  dat <- filter(dat, length <= ql, weight <= qw)

  dat <- switch(sex[[1]],
    "female" = filter(dat, sex == 2),
    "male" = filter(dat, sex == 1),
    stop("`sex` argument must be 'female' or 'male'.", call. = FALSE)
  )

  dat$weight <- dat$weight * scale_weight

  if (nrow(dat) < min_samples) {
    return(list(
      predictions = tibble(length = NA, weight = NA),
      pars = list(log_a = NA, b = NA),
      data = dat,
      model = NA
    ))
  }

  m <- switch(method[[1]],
    "rlm" = MASS::rlm(log(weight) ~ log(length), data = dat),
    "lm" = stats::lm(log(weight) ~ log(length), data = dat),
    stop("`method` argument must be 'rlm' or 'lm'.", call. = FALSE)
  )

  pred <- tibble(length = seq(min(dat$length), max(dat$length), length.out = 200L))
  pred$weight <- exp(predict(m, newdata = pred))

  pars <- as.list(coef(m))
  pars <- stats::setNames(pars, c("log_a", "b"))

  list(predictions = pred, pars = pars, data = as_tibble(dat), model = m)
}

#' Plot von Bertalanffy or length-weight fits
#'
#' @param object_female Output from [fit_length_weight()] or [fit_vb()].
#' @param object_male Output from [fit_length_weight()] or [fit_vb()].
#' @param type von Bertalanffy or length-weight fits?
#' @param downsample Downsample the individual fish to plot down to this number.
#' @param pt_alpha Transparency for the points.
#' @param xlab Label for the x axis.
#' @param ylab Label for the y axis.
#' @param seed A random seed value that only comes into play for downsampling.
#' @param lab_x Fraction from left to place text labels.
#' @param lab_y Fraction from bottom to place text labels.
#' @param lab_x_gap Horizontal gap between text labels.
#' @param lab_y_gap The vertical gap between text labels.
#' @param col A named character vector declaring the colors for female and male
#'   fish.
#'
#' @export
#' @family growth functions
#' @rdname plot_growth
#'
#' @examples
#' \dontrun{
#' d <- get_survey_samples("pacific ocean perch")
#' model_f <- fit_length_weight(d, sex = "female")
#' model_m <- fit_length_weight(d, sex = "male")
#' plot_length_weight(object_female = model_f, object_male = model_m)
#'
#' model_f <- fit_vb(d, sex = "female")
#' model_m <- fit_vb(d, sex = "male")
#' plot_vb(object_female = model_f, object_male = model_m)
#' }

plot_growth <- function(object_female, object_male,
                        type = c("vb", "length-weight"),
                        downsample = 2000L,
                        pt_alpha = 0.2,
                        xlab = "Age (years)",
                        ylab = "Length (cm)",
                        seed = 42,
                        lab_x = 0.45,
                        lab_y = 0.3,
                        lab_x_gap = 0.3,
                        lab_y_gap = 0.06,
                        col = c("Female" = "black", "Male" = "grey40")) {
  xvar <- if (type[[1]] == "vb") "age" else "length"
  yvar <- if (type[[1]] == "vb") "length" else "weight"

  line_dat <- bind_rows(
    data.frame(object_female$predictions,
      sex = "Female",
      stringsAsFactors = FALSE
    ),
    data.frame(object_male$predictions,
      sex = "Male",
      stringsAsFactors = FALSE
    )
  )

  no_lines <- if (all(is.na(line_dat[[1]]))) TRUE else FALSE

  pt_dat <- bind_rows(object_female$data, object_male$data)
  pt_dat$sex <- dplyr::case_when(
    pt_dat$sex == 1 ~ "Male",
    pt_dat$sex == 2 ~ "Female",
    TRUE ~ ""
  )
  no_pts <- if (nrow(pt_dat) == 0L) TRUE else FALSE

  xdat <- if (type[[1]] == "vb") pt_dat$age else pt_dat$length
  ydat <- if (type[[1]] == "vb") pt_dat$length else pt_dat$weight

  if (nrow(pt_dat) > downsample) {
    if (!is.null(seed)) set.seed(seed)
    pt_dat <- pt_dat[sample(seq_len(nrow(pt_dat)), downsample), , drop = FALSE]
  }

  g <- ggplot() +
    scale_colour_manual(values = col, labels = c("F", "M")) +
    ggplot2::scale_linetype_manual(values = c(1, 2), labels = c("F", "M")) +
    theme_pbs() + xlab(xlab) + ylab(ylab) +
    ggplot2::labs(colour = "Sex", lty = "Sex")

  if (!no_pts) {

    if (!no_lines) {
      xlim <- c(0, max(line_dat[,xvar]) * 1.03)
      ylim <- c(0, max(line_dat[,yvar]) * 1.20)
    } else {
      xlim <- c(0, max(pt_dat[,xvar]) * 1.03)
      ylim <- c(0, max(pt_dat[,yvar]) * 1.03)
    }

    g <- g + geom_point(
      data = pt_dat, aes_string(xvar, yvar),
      alpha = pt_alpha, colour = "grey70", pch = 21
    ) +
      coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
  }

  if (!no_lines) {
    g <- g + geom_line(data = line_dat, aes_string(xvar, yvar,
      colour = "sex", lty = "sex"
    ), size = 1.0)
  }

  ann_func <- if (type[[1]] == "vb") ann_vb else ann_lw
  if (!is.na(object_female$pars[[1]])) {
    g <- ann_func(g, object_female$pars, "Females", col[["Female"]],
      x = lab_x * max(xlim),
      y = lab_y * max(ylim),
      gap = lab_y_gap * max(ylim)
    )
  }

  if (!is.na(object_male$pars[[1]])) {
    g <- ann_func(g, object_male$pars, "Males", col[["Male"]],
      x = (lab_x + lab_x_gap) * max(xlim),
      y = lab_y * max(ylim),
      gap = lab_y_gap * max(ylim)
    )
  }

  g
}

#' @inheritParams plot_growth
#' @param ... Arguments to pass to [plot_growth()].
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
plot_length_weight <- function(..., type = "length-weight", xlab = "Length (cm)",
                               ylab = "Weight (kg)",
                               lab_x = 0.1, lab_y = 0.9,
                               lab_x_gap = 0.35) {
  plot_growth(...,
    type = type, xlab = xlab,
    ylab = ylab, lab_x = lab_x, lab_y = lab_y, lab_x_gap = lab_x_gap
  ) +
    ggplot2::ggtitle("Length-weight relationship")
}

# annotation helpers:
ann_vb <- function(gg, pars, title, col, x, y, gap) {
  gg + ggplot2::annotate("text",
    label = title,
    x = x, y = y, hjust = 0, col = col, size = 3
  ) +
    ann("k", pars[["k"]], dec = 2, x, y - gap) +
    ann("linf", pars[["linf"]], dec = 1, x, y - gap * 2) +
    ann("t0", pars[["t0"]], dec = 2, x, y - gap * 3)
}

ann_lw <- function(gg, pars, title, col, x, y, gap) {
  gg + ggplot2::annotate("text",
    label = title,
    x = x, y = y, hjust = 0, col = col, size = 3
  ) +
    ann("ln(a)", pars[["log_a"]], dec = 2, x, y - gap) +
    ann("b", pars[["b"]], dec = 2, x, y - gap * 2)
}

ann <- function(par_name, par_val, dec, x, y, col = "grey40") {
  ggplot2::annotate("text",
    label = paste0(
      par_name, " = ",
      sprintf(
        paste0("%.", dec, "f"),
        round(par_val, dec)
      )
    ),
    x = x,
    y = y,
    hjust = 0,
    col = col, size = 3
  )
}

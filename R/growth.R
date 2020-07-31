#' Fit a von Bertalanffy growth model
#'
#' For use with data for a single species.
#'
#' @param dat Input data frame. Should be from [get_survey_samples()] or
#'   [get_commercial_samples()].
#' @param sex Either "male" or "female".
#' @param method `"mpd"` for the mode of the posterior distribution (with
#'   [rstan::optimizing()]) or `"mcmc"` for full MCMC sampling with Stan (with
#'   [rstan::sampling()]). `"tmb"` for a TMB model.
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
#' @param too_high_quantile A quantile above which to discard weights and
#'   lengths. Can be useful for outliers. Defaults to including all data.
#' @param uniform_priors Logical. If true then uniform priors will be used.
#' @param ageing_method_codes A numeric vector of ageing method codes to filter
#'   on. Defaults to `NULL`, which brings in all valid ageing codes. See
#'   [get_age_methods()].
#' @param usability_codes An optional vector of usability codes.
#'   All usability codes not in this vector will be omitted.
#'   Set to `NULL` to include all samples.
#' @param check_convergence_tmb Logical.
#' @param tmb_inits A named list of initial parameter values for the TMB model.
#' @param ... Any other arguments to pass on to [rstan::sampling()] or
#'   [rstan::optimizing()].
#' @importFrom stats median quantile rlnorm runif median
#'
#' @export
#' @examples
#' \donttest{
#' # with `rstan::optimizing()` for the mode of the posterior density:
#' model_f <- fit_vb(pop_samples, sex = "female")
#' model_m <- fit_vb(pop_samples, sex = "male")
#' plot_vb(model_f, model_m)
#' model_f$model
#' model_f$predictions
#'
#' # You can also fit both sexes combined if you want.
#' # Just note that you need to specify the colours explicitly in the plot.
#' model_all <- fit_vb(pop_samples, sex = "all")
#' plot_vb(object_all = model_all, col = c("All" = "black"))
#'
#' # with MCMC via Stan (slower):
#' x <- fit_vb(pop_samples, method = "mcmc",
#'   chains = 1, iter = 800, seed = 123) # just for a fast example
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
#' }

fit_vb <- function(dat,
                   sex = c("female", "male", "all"),
                   method = c("tmb", "mpd", "mcmc"),
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
                   usability_codes = c(0, 1, 2, 6),
                   check_convergence_tmb = TRUE,
                   tmb_inits = list(k = 0.5, linf = 40, log_sigma = log(0.1), t0 = -1),
                   ...) {
  if ("species_common_name" %in% names(dat)) {
    if (length(unique(dat$species_common_name)) > 1L) {
      stop("Multiple species detected via the `species_common_name` column. ",
        "fit_vb() is for use with a single species. Filter the data yourself ",
        "first.",
        call. = FALSE
      )
    }
  }

  method <- match.arg(method)

  if (!is.null(usability_codes)) {
    dat <- filter(dat, .data$usability_code %in% usability_codes)
  }

  if (!is.null(ageing_method_codes)) {
    dat <- filter(dat, .data$ageing_method %in% ageing_method_codes)
  }
  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$age))
  ql <- quantile(dat$length, probs = too_high_quantile)
  dat <- filter(dat, length <= ql)

  sex <- match.arg(sex)
  dat <- switch(sex,
    "female" = filter(dat, sex == 2L),
    "male" = filter(dat, sex == 1L),
    "all" = dat
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

  if (nrow(dat) > downsample) {
    dat <- dat[sample(seq_len(nrow(dat)), downsample), , drop = FALSE]
  }

  if (method %in% c('mpd', 'mcmc')) {
    rstan::rstan_options(auto_write = TRUE)
    if (uniform_priors) {
      model_file <- system.file("stan", "vb-nopriors.stan", package = "gfplot")
      .f <- "vb-nopriors.stan"
    } else {
      model_file <- system.file("stan", "vb.stan", package = "gfplot")
      .f <- "vb.stan"
    }

    if (!file.exists(.f)) {
      file.copy(model_file, to = .f)
    }

    vb_mod_gfplot <- rstan::stan_model(.f)
    assign("vb_mod_gfplot", vb_mod_gfplot, envir = globalenv())
  }

  if (method == "mpd") {
    mpd_init <- list()
    mpd_init$K <- 0.2 # wild guess
    mpd_init$Linf <- 40 # wild guess
    mpd_init$t0 <- -1 # wild guess
    mpd_init$sigma <- 0.1

    m <- suppressMessages(rstan::optimizing(vb_mod_gfplot,
      data = list(
        N = nrow(dat), age = dat$age, length = dat$length,
        linf_upper_sd = quantile(dat$length, 0.99)[[1]] * 2
      ),
      init = mpd_init, hessian = TRUE, ...
    ))

    if (m$return_code != 0L && check_convergence_tmb) {
      stop("VB growth model did not converge!")
    }

    pars <- as.list(m$par)
  }
  if (method == "mcmc") {
    if (nrow(dat) > 50000 && !allow_slow_mcmc) {
      stop("There are > 50,000 aged fish. ",
        "MCMC sampling may take a long time. Set `allow_slow_mcmc = TRUE` ",
        "if you still want to sample from the posterior.",
        call. = FALSE
      )
    }
    m <- rstan::sampling(vb_mod_gfplot,
      data = list(
        N = nrow(dat), age = dat$age, length = dat$length,
        linf_upper_sd = quantile(dat$length, 0.99)[[1]] * 2
      ),
      chains = chains, iter = iter, cores = cores, ...
    )

    pars <- lapply(rstan::extract(m), est_method)
  }

  if (method == "tmb") {
    dlls <- getLoadedDLLs()
    if (!any(vapply(dlls, function(x) x[["name"]] == "vb", FUN.VALUE = TRUE))) {
      lib.f <- system.file("tmb", "vb.cpp", package = "gfplot")
      .f <- "vb_gfplot.cpp"
      if (!file.exists(.f)) {
        file.copy(lib.f, to = .f)
      }
      TMB::compile(.f)
      dyn.load(TMB::dynlib("vb_gfplot"))
    }
    data <- list(len = dat$length, age = dat$age)

    obj <- TMB::MakeADFun(data, tmb_inits, DLL = "vb_gfplot", silent = TRUE)
    opt <- stats::nlminb(obj$par, obj$fn, obj$gr)
    if (opt$convergence != 0L && check_convergence_tmb)
      stop("VB growth model did not converge!")
    pars <- as.list(opt$par)
    m <- obj
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
#'   [get_commercial_samples()].
#' @param sex Either "male" or "female".
#' @param downsample If not `Inf` this represents a number of fish specimens to
#'   sample prior to model fitting. Can be useful for large data sets that you
#'   want to fit with MCMC for testing purposes.
#' @param min_samples The minimum number of fish before a model will be fit.
#' @param method `"rlm"` for [MASS::rlm()] or `"lm"` for [stats::lm()].
#'   `"tmb"` for a regression fit with TMB that uses Student-t errors with
#'   a degrees of freedom defined by the argument `df`.
#' @param df The fixed degrees of freedom to use if `method = "tmb"`. Large
#'   values (say over 100) are effectively the normal distribution. Small
#'   values make the distribution more robust to outliers. The model may
#'   become unstable if `df < 2`.
#' @param too_high_quantile A quantile above which to discard weights and
#'   lengths. Can be useful for outliers. Defaults to including all data.
#' @param usability_codes An optional vector of usability codes.
#'   All usability codes not in this vector will be omitted.
#'   Set to `NULL` to include all samples.
#' @param scale_weight A value to multiply all weights by. Useful for changing
#'   units.
#'
#' @export
#' @examples
#' \donttest{
#' d <- get_survey_samples("pacific ocean perch")
#' model_f <- fit_length_weight(d, sex = "female")
#' model_m <- fit_length_weight(d, sex = "male")
#' plot_length_weight(object_female = model_f, object_male = model_m)
#' }

fit_length_weight <- function(dat,
                              sex = c("female", "male", "all"),
                              downsample = Inf,
                              min_samples = 50L,
                              method = c("tmb", "rlm", "lm"),
                              df = 3,
                              too_high_quantile = 1.0,
                              usability_codes = c(0, 1, 2, 6),
                              scale_weight = 1 / 1000) {
  if ("species_common_name" %in% names(dat)) {
    if (length(unique(dat$species_common_name)) != 1L) {
      stop("Multiple species detected via the `species_common_name` column. ",
        "fit_length_weight() is for use with a single species. Filter the data yourself ",
        "first.",
        call. = FALSE
      )
    }
  }

  if (!is.null(usability_codes)) {
    dat <- filter(dat, .data$usability_code %in% usability_codes)
  }

  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]

  dat <- filter(dat, !is.na(.data$sex), !is.na(.data$length), !is.na(.data$weight))
  ql <- quantile(dat$length, probs = too_high_quantile)
  qw <- quantile(dat$weight, probs = too_high_quantile)
  dat <- filter(dat, length <= ql, weight <= qw)

  dat <- switch(sex[[1]],
    "female" = filter(dat, sex == 2),
    "male" = filter(dat, sex == 1),
    "all" = dat,
    stop("`sex` argument must be 'female' or 'male' or 'all'.", call. = FALSE)
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

  method <- match.arg(method)

  if (method == "rlm") {
    m <- MASS::rlm(log(weight) ~ log(length), data = dat)
    pars <- as.list(coef(m))
    pars <- stats::setNames(pars, c("log_a", "b"))
  }

  if (method == 'lm') {
    m <- stats::lm(log(weight) ~ log(length), data = dat)
    pars <- as.list(coef(m))
    pars <- stats::setNames(pars, c("log_a", "b"))
  }

  if (method == "tmb") {
    dlls <- getLoadedDLLs()
    if (!any(vapply(dlls, function(x) x[["name"]] == "lw", FUN.VALUE = TRUE))) {
      lib.f <- system.file("tmb", "lw.cpp", package = "gfplot")
      .f <- "lw_gfplot.cpp"
      if (!file.exists(.f)) {
        file.copy(lib.f, to = .f)
      }
      TMB::compile(.f)
      dyn.load(TMB::dynlib("lw_gfplot"))
    }
    data <- list(len = log(dat$length), weight = log(dat$weight), df = df)
    parameters <- list(log_a = 0, b = 0, log_sigma = 0)
    obj <- TMB::MakeADFun(data, parameters, DLL = "lw_gfplot", silent = TRUE)
    opt <- stats::nlminb(obj$par, obj$fn, obj$gr)
    if (opt$convergence != 0L)
      stop("Length-weight model did not converge!")
    pars <- as.list(opt$par)
    m <- obj
  }

  pred <- tibble(length = seq(min(dat$length), max(dat$length), length.out = 200L))
  pred$weight <- exp(pars$log_a + pars$b * log(pred$length))

  list(predictions = pred, pars = pars, data = as_tibble(dat), model = m)
}

#' Plot von Bertalanffy or length-weight fits
#'
#' @param object_female Output from [fit_length_weight()] or [fit_vb()].
#' @param object_male Output from [fit_length_weight()] or [fit_vb()].
#' @param object_all Output from [fit_length_weight()] or [fit_vb()].
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
#' @param french Logical.
#'
#' @details You can include `object_female` and/or `object_male` or `object_all`
#' depending on whether the model was fit to female, male, or both sexes
#' combined.
#'
#' @export
#' @rdname plot_growth
#'
#' @examples
#' \donttest{
#' # d <- get_survey_samples("pacific ocean perch")
#' d <- pop_samples
#' model_f <- fit_length_weight(d, sex = "female")
#' model_m <- fit_length_weight(d, sex = "male")
#' plot_length_weight(object_female = model_f, object_male = model_m)
#'
#' model_f <- fit_vb(d, sex = "female")
#' model_m <- fit_vb(d, sex = "male")
#' plot_vb(object_female = model_f, object_male = model_m)
#' }

plot_growth <- function(object_female, object_male,
                        object_all,
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
                        col = c("Female" = "black", "Male" = "grey40"),
                        french = FALSE) {
  xvar <- if (type[[1]] == "vb") "age" else "length"
  yvar <- if (type[[1]] == "vb") "length" else "weight"

  if (missing(object_all)) {
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
  } else {
    line_dat <- data.frame(object_all$predictions,
      sex = "All",
      stringsAsFactors = FALSE
    )
  }

  no_lines <- if (all(is.na(line_dat[[1]]))) TRUE else FALSE

  if (missing(object_all)) {
    pt_dat <- bind_rows(object_female$data, object_male$data)
    pt_dat$sex <- dplyr::case_when(
      pt_dat$sex == 1 ~ "Male",
      pt_dat$sex == 2 ~ "Female",
      TRUE ~ ""
    )
  } else {
    pt_dat <- object_all$data
    pt_dat$sex <- "All"
  }
  no_pts <- if (nrow(pt_dat) == 0L) TRUE else FALSE

  xdat <- if (type[[1]] == "vb") pt_dat$age else pt_dat$length
  ydat <- if (type[[1]] == "vb") pt_dat$length else pt_dat$weight

  if (nrow(pt_dat) > downsample) {
    if (!is.null(seed)) set.seed(seed)
    pt_dat <- pt_dat[sample(seq_len(nrow(pt_dat)), downsample), , drop = FALSE]
  }

  labs <- if (missing(object_all)) c("F", "M") else "All"
  g <- ggplot() +
    scale_colour_manual(values = col, labels = labs) +
    ggplot2::scale_linetype_manual(values = c(1, 2), labels = labs) +
    theme_pbs() + xlab(xlab) + ylab(ylab) +
    ggplot2::labs(colour = en2fr("Sex", french), lty = en2fr("Sex", french))

  if (!no_pts) {
    if (!no_lines) {
      xlim <- c(0, max(line_dat[, xvar], na.rm = TRUE) * 1.03)
      ylim <- c(0, max(line_dat[, yvar], na.rm = TRUE) * 1.20)
    } else {
      xlim <- c(0, max(pt_dat[, xvar], na.rm = TRUE) * 1.03)
      ylim <- c(0, max(pt_dat[, yvar], na.rm = TRUE) * 1.03)
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

  if (missing(object_all)) {
    if (!is.na(object_female$pars[[1]])) {
      g <- ann_func(g, object_female$pars, en2fr("Females", french), col[["Female"]],
        x = lab_x * max(xlim),
        y = lab_y * max(ylim),
        gap = lab_y_gap * max(ylim)
      )
    }

    if (!is.na(object_male$pars[[1]])) {
      g <- ann_func(g, object_male$pars, en2fr("Males", french), col[["Male"]],
        x = (lab_x + lab_x_gap) * max(xlim),
        y = lab_y * max(ylim),
        gap = lab_y_gap * max(ylim)
      )
    }
  } else {
    g <- ann_func(g, object_all$pars, en2fr("Both sexes", french, allow_missing = TRUE), col[[1]],
      x = lab_x * max(xlim),
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
                               lab_x_gap = 0.35,
                               french = FALSE) {
  plot_growth(...,
    type = type, xlab = xlab,
    ylab = ylab, lab_x = lab_x, lab_y = lab_y, lab_x_gap = lab_x_gap, french = french
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

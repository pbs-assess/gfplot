#' Fit a growth model for length-at-age
#'
#' For use with data for a single species, fit either a Schnute growth curve or
#' a von Bertalanffy growth curve. `fit_vb(method = "tmb")` will use `fit_schnute()`
#' with `p = 1`. Otherwise, separate Stan files are used. The Richards function
#' is also nested in the Schnute function with `0 < p < 1`.
#'
#' @param dat Input data frame. Should be from [gfdata::get_survey_samples()] or
#'   [gfdata::get_commercial_samples()].
#' @param sex Either "male" or "female".
#' @param method `"mpd"` for the mode of the posterior distribution (with
#'   [rstan::optimizing()]) or `"mcmc"` for full MCMC sampling with Stan (with
#'   [rstan::sampling()]). `"tmb"` for a TMB model.
#' @param growth Whether to estimate the inflection parameter p (`"schnute"`) or
#'   fix the parameter p = 1 (`"vb"`) to reduce to the von Bertalanffy equation.
#' @param a1 The first age parameter in the Schnute function, the corresponding
#'   length `"L1"` will be estimated. You may want to specify the smallest age
#'   class in the data.
#' @param a2 The second age parameter in the Schnute function, the corresponding
#'   length `"L2"` will be estimated. You may want to specify the largest age
#'   class in the data. Set `"a2" = 999` to effectively estimate `linf`.
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
#'   [gfdata::get_age_methods()].
#' @param usability_codes An optional vector of usability codes.
#'   All usability codes not in this vector will be omitted.
#'   Set to `NULL` to include all samples.
#' @param check_convergence_tmb Logical.
#' @param tmb_inits A named list of initial parameter values for the TMB model.
#' @param ... Any other arguments to pass on to [rstan::sampling()] or
#'   [rstan::optimizing()].
#' @importFrom stats median quantile rlnorm runif median
#' @references
#' Schnute, J. 1981. A Versatile Growth Model with Statistically Stable Parameters.
#' Canadian Journal of Fisheries and Aquatic Sciences, 38(9), 1128–1140.
#' <https://doi.org/10.1139/f81-153>
#'
#' @export
#' @details
#' Linf and t0 are derived from the Schnute equations as:
#'
#' \deqn{
#' linf = \left(\dfrac{\exp(ka_2) L_2^p - \exp(ka_1)L_1^p}{\exp(ka_2) - \exp(ka_1)}\right)^{1/p}
#' }
#'
#' \deqn{
#' t_0 = a_1 + a_2 - \dfrac{1}{k}\log\left(\dfrac{\exp(ka_2) L_2^p - \exp(ka_1)L_1^p}{L_2^p - L_1^p}\right)
#' }
#'
#' `t_0` is undefined when `p < 0`.
#'
#' @examples
#' \dontrun{
#' # with `rstan::optimizing()` for the mode of the posterior density:
#' model_f <- fit_vb(pop_samples, sex = "female")
#' model_m <- fit_vb(pop_samples, sex = "male")
#' plot_vb(model_f, model_m)
#' model_f$model
#' model_f$predictions
#'
#' # Schnute functions
#' model_fs <- fit_schnute(pop_samples, sex = "female")
#' model_ms <- fit_schnute(pop_samples, sex = "male")
#' plot_schnute(model_fs, model_ms)
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
fit_schnute <- function(dat,
                        sex = c("female", "male", "all"),
                        method = c("tmb", "mpd", "mcmc"),
                        growth = c("schnute", "vb"),
                        a1 = 0,
                        a2 = 999,
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
                        tmb_inits = list(k = 0.5, L1 = 10, L2 = 40, log_sigma = log(0.1), p = 1),
                        ...) {

  method <- match.arg(method)
  growth <- match.arg(growth)

  if (growth == "vb" && method != "tmb") {
    stop("Use fit_vb(method = \"", method, "\")")
  }

  if ("species_common_name" %in% names(dat)) {
    if (length(unique(dat$species_common_name)) > 1L) {
      stop("Multiple species detected via the `species_common_name` column. ",
           "fit_schnute() is for use with a single species. Filter the data yourself ",
           "first.",
           call. = FALSE
      )
    }
  }

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
      pars = list(k = NA, L1 = NA, L2 = NA, log_sigma = NA, p = NA, linf = NA, t0 = NA),
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
      model_file <- system.file("stan", "schnute-nopriors.stan", package = "gfplot")
      .f <- "schnute-nopriors.stan"
    } else {
      model_file <- system.file("stan", "schnute.stan", package = "gfplot")
      .f <- "schnute.stan"
    }

    if (!file.exists(.f)) {
      file.copy(model_file, to = .f)
    }

    schnute_mod_gfplot <- rstan::stan_model(.f)
    assign("schnute_mod_gfplot", schnute_mod_gfplot, envir = globalenv())
  }

  if (method == "mpd") {
    mpd_init <- list()
    mpd_init$k <- 0.2 # wild guess
    mpd_init$L1 <- 10 # wild guess
    mpd_init$L2 <- 40 # wild guess
    mpd_init$sigma <- 0.1
    mpd_init$p <- 1

    m <- suppressMessages(rstan::optimizing(schnute_mod_gfplot,
                                            data = list(
                                              N = nrow(dat), age = dat$age, length = dat$length,
                                              len_upper_sd = quantile(dat$length, 0.99)[[1]] * 2,
                                              a1 = a1,
                                              a2 = a2
                                            ),
                                            init = mpd_init, hessian = TRUE, ...
    ))

    if (m$return_code != 0L && check_convergence_tmb) {
      stop("Schnute growth model did not converge!")
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
    m <- rstan::sampling(schnute_mod_gfplot,
                         data = list(
                           N = nrow(dat), age = dat$age, length = dat$length,
                           len_upper_sd = quantile(dat$length, 0.99)[[1]] * 2,
                           a1 = a1,
                           a2 = a2
                         ),
                         chains = chains, iter = iter, cores = cores, ...
    )

    pars <- lapply(rstan::extract(m), est_method)
  }

  if (method == "tmb") {
    dlls <- getLoadedDLLs()
    if (!any(vapply(dlls, function(x) x[["name"]] == "schnute", FUN.VALUE = TRUE))) {
      lib.f <- system.file("tmb", "schnute.cpp", package = "gfplot")
      .f <- "schnute_gfplot.cpp"
      if (!file.exists(.f)) {
        file.copy(lib.f, to = .f)
      }
      TMB::compile(.f)
      dyn.load(TMB::dynlib("schnute_gfplot"))
    }
    data <- list(len = dat$length, age = dat$age, a1 = a1, a2 = a2)
    map <- list()
    if (growth == "vb") map$p <- factor(NA)

    obj <- TMB::MakeADFun(data, tmb_inits, DLL = "schnute_gfplot", map = map, silent = TRUE)
    opt <- stats::nlminb(obj$par, obj$fn, obj$gr)
    if (opt$convergence != 0L && check_convergence_tmb)
      stop("Schnute growth model did not converge!")
    pars <- as.list(opt$par)
    pars$linf <- obj$report(obj$env$last.par.best)$linf
    pars$t0 <- obj$report(obj$env$last.par.best)$t0
    if (growth == "vb") pars$p <- tmb_inits$p
    m <- obj
  }

  if (is.na(pars[["t0"]])) warning("Derived t0 is NA.")

  ages <- seq(min(dat$age), max(dat$age), length.out = 200L)
  pred <- schnute(ages, par = pars, a1 = a1, a2 = a2)

  list(
    predictions = tibble(age = ages, length = pred),
    pars = pars, data = as_tibble(dat), model = m
  )
}

schnute <- function(ages, par, a1, a2) {
  L1 <- par[["L1"]]
  L2 <- par[["L2"]]
  k <- par[["k"]]
  p <- par[["p"]]
  if(is.null(p)) p <- 1

  tmp1 <- L1^p
  tmp2 <- (L2^p - tmp1)/(1 - exp(-k * (a2 - a1)))

  tmp <- tmp1 + tmp2 * (1 - exp(-k * (ages - a1)))
  invp <- 1/p

  tmp^invp
}

#' @rdname fit_schnute
#' @export
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

  method <- match.arg(method)

  if (method == "tmb") {

    m <- fit_schnute(
      dat = dat,
      sex = sex,
      method = method,
      growth = "vb",
      a1 = 0,
      a2 = 999,
      downsample = downsample,
      chains = chains,
      iter = iter,
      cores = cores,
      allow_slow_mcmc = allow_slow_mcmc,
      est_method = est_method,
      min_samples = min_samples,
      too_high_quantile = too_high_quantile,
      uniform_priors = uniform_priors,
      ageing_method_codes = ageing_method_codes,
      usability_codes = usability_codes,
      check_convergence_tmb = check_convergence_tmb,
      tmb_inits = list(
        k = tmb_inits$k, L1 = 0.1 * tmb_inits$linf,
        L2 = tmb_inits$linf, log_sigma = tmb_inits$log_sigma,
        p = 1
      ),
      ...
    )

    return(m)
  }

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
  if (method %in% c("mcmc", "mpd")) {
    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop("rstan must be installed to use this function with 'mpd' or 'mcmc' methods.", call. = FALSE)
    }
  }

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

  ages <- seq(min(dat$age), max(dat$age), length.out = 200L)
  pred <- vb(ages, linf = pars$linf, k = pars$k, t0 = pars$t0)

  list(
    predictions = tibble(age = ages, length = pred),
    pars = pars, data = as_tibble(dat), model = m
  )
}

vb <- function(ages, linf, k, t0) linf * (1 - exp(-k * (ages - t0)))



#' Fit a length-weight model
#'
#' For use with data for a single species.
#'
#' @param dat Input data frame. Should be from [gfdata::get_survey_samples()] or
#'   [gfdata::get_commercial_samples()].
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
#' \dontrun{
#' d <- gfdata::get_survey_samples("pacific ocean perch")
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
#' @param jitter Logical, whether to jitter data points.
#'
#' @details You can include `object_female` and/or `object_male` or `object_all`
#' depending on whether the model was fit to female, male, or both sexes
#' combined.
#'
#' @export
#' @rdname plot_growth
#'
#' @examples
#' \dontrun{
#' # d <- gfdata::get_survey_samples("pacific ocean perch")
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
                        type = c("schnute", "length-weight"),
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
                        french = FALSE,
                        jitter = FALSE) {

  if (type[1] == "vb") type <- "schnute"
  type <- match.arg(type)
  xvar <- if (type == "schnute") "age" else "length"
  yvar <- if (type == "schnute") "length" else "weight"

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

  xdat <- if (type == "schnute") pt_dat$age else pt_dat$length
  ydat <- if (type == "schnute") pt_dat$length else pt_dat$weight

  if (nrow(pt_dat) > downsample) {
    if (!is.null(seed)) set.seed(seed)
    pt_dat <- pt_dat[sample(seq_len(nrow(pt_dat)), downsample), , drop = FALSE]
  }

  labs <- if (missing(object_all)) c("F", "M") else "All"
  g <- ggplot() +
    scale_colour_manual(values = col, labels = labs) +
    ggplot2::scale_linetype_manual(values = c(1, 2), labels = labs) +
    theme_pbs() + xlab(en2fr(xlab, french)) + ylab(en2fr(ylab, french)) +
    ggplot2::labs(colour = en2fr("Sex", french), lty = en2fr("Sex", french))

  if (!no_pts) {
    if (!no_lines) {
      xlim <- c(0, max(line_dat[, xvar], na.rm = TRUE) * 1.03)
      ylim <- c(0, max(line_dat[, yvar], na.rm = TRUE) * 1.20)
    } else {
      xlim <- c(0, max(pt_dat[, xvar], na.rm = TRUE) * 1.03)
      ylim <- c(0, max(pt_dat[, yvar], na.rm = TRUE) * 1.03)
    }

    if (jitter) {
      g <- g + geom_jitter(
        data = pt_dat, aes_string(xvar, yvar),
        alpha = pt_alpha, colour = "grey70", pch = 21
      ) +
        coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
    } else {
      g <- g + geom_point(
        data = pt_dat, aes_string(xvar, yvar),
        alpha = pt_alpha, colour = "grey70", pch = 21
      ) +
        coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
    }
  }

  if (!no_lines) {
    g <- g + geom_line(data = line_dat, aes_string(xvar, yvar,
      colour = "sex", lty = "sex"
    ), size = 1.0)
  }

  ann_func <- if (type == "schnute") ann_schnute else ann_lw

  if (missing(object_all)) {
    if (!is.na(object_female$pars[[1]])) {
      g <- ann_func(g, object_female$pars, en2fr("Females", french), col[["Female"]],
        x = lab_x * max(xlim),
        y = lab_y * max(ylim),
        gap = lab_y_gap * max(ylim),
        french = french
      )
    }

    if (!is.na(object_male$pars[[1]])) {
      g <- ann_func(g, object_male$pars, en2fr("Males", french), col[["Male"]],
        x = (lab_x + lab_x_gap) * max(xlim),
        y = lab_y * max(ylim),
        gap = lab_y_gap * max(ylim),
        french = french
      )
    }
  } else {
    g <- ann_func(g, object_all$pars, en2fr("Both sexes", french), col[[1]],
      x = lab_x * max(xlim),
      y = lab_y * max(ylim),
      gap = lab_y_gap * max(ylim),
      french = french
    )
  }

  g
}

#' @param ... Arguments to pass to [plot_growth()].
#' @export
#' @rdname plot_growth
plot_vb <- function(..., type = "vb") {
  plot_growth(..., type = type) +
    ggplot2::ggtitle("Growth")
}

#' @export
#' @rdname plot_growth
plot_schnute <- function(..., type = "schnute") {
  plot_growth(..., type = type) +
    ggplot2::ggtitle("Growth")
}

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
    ggplot2::ggtitle(en2fr("Length-weight relationship", french))
}

# annotation helpers:
ann_schnute <- function(gg, pars, title, col, x, y, gap, french = FALSE) {
  gout <- gg + ggplot2::annotate("text",
    label = title,
    x = x, y = y, hjust = 0, col = col, size = 3
  ) +
    ann("k", pars[["k"]], dec = 2, x, y - gap, french = french) +
    ann("linf", pars[["linf"]], dec = 1, x, y - gap * 2, french = french)

  gap_counter <- 3
  if (!is.null(pars[["t0"]]) && !is.na(pars[["t0"]])) {
    gout <- gout +
      ann("t0", pars[["t0"]], dec = 2, x, y - gap * gap_counter, french = french)
    gap_counter <- gap_counter + 1
  }

  if (!is.null(pars[["p"]]) && !is.na(pars[["p"]]) && pars[["p"]] != 1) {
    gout <- gout +
      ann("p", pars[["p"]], dec = 2, x, y - gap * gap_counter, french = french)
  }

  gout
}

ann_vb <- ann_schnute

ann_lw <- function(gg, pars, title, col, x, y, gap, french = FALSE) {
  gg + ggplot2::annotate("text",
    label = title,
    x = x, y = y, hjust = 0, col = col, size = 3
  ) +
    ann("ln(a)", pars[["log_a"]], dec = 2, x, y - gap, french = french) +
    ann("b", pars[["b"]], dec = 2, x, y - gap * 2, french = french)
}

ann <- function(par_name, par_val, dec, x, y, col = "grey40", french = FALSE) {
  .text <- paste0(
    par_name, " = ",
    sprintf(
      paste0("%.", dec, "f"),
      round(par_val, dec)
    )
  )
  if (french) .text <- gsub("\\.", ",", .text)
  ggplot2::annotate("text",
    label = .text,
    x = x,
    y = y,
    hjust = 0,
    col = col, size = 3
  )
}

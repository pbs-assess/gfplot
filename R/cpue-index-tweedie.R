load_tmb <- function(file = "tweedie_cpue.cpp") {
  tmb_cpp <- system.file("tmb", file, package = "gfplot")
  TMB::compile(tmb_cpp)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp)))
}

#' Fit CPUE with glmmTMB
#'
#' @param dat Data
#' @param formula Formula
#' @param ... Other arguments to [glmmTMB::glmmTMB()]
#'
#' @export
fit_cpue_index_glmmtmb <- function(dat, formula = cpue ~ year_factor, ...) {
  glmmTMB::glmmTMB(as.formula(formula), data = dat, family = glmmTMB::tweedie(link = "log"),
    verbose = TRUE,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 2000, eval.max = 2000),
      profile = TRUE, collect = FALSE), ...)
}

#' Commercial CPUE index standardization (Tweedie version)
#'
#' @param dat A data frame to fit. Might be from [tidy_cpue_index()] or
#'   [tidy_cpue_historic()].
#' @param formula Formula A formula with up to 2 random intercepts in lme4
#'   syntax. Must contain a predictor named `year_factor` that contains the year
#'   as a factor.
#'
#' @export
#' @examples
#' set.seed(1)
#' d <- sim_cpue()
#' # assign factor levels with base as most common level:
#' d$vessel <- gfplot::f(d$vessel)
#' m1 <- fit_cpue_index_tweedie(d, cpue ~ year_factor + vessel)
#' plot_cpue_index_coefs(m1, type = "tweedie")
#'
#' # Random intercepts for vessel:
#' m2 <- fit_cpue_index_tweedie(d, cpue ~ year_factor + (1 | vessel))
#' plot_cpue_index_coefs(m2, type = "tweedie", re_name = c("vessel", NA))
#'
#' predict_cpue_index_tweedie(m2)
#'
#' predict_cpue_index_tweedie(m2, center = FALSE) %>%
#'   plot_cpue_index()
#' @rdname cpue-tweedie

fit_cpue_index_tweedie <- function(dat, formula = cpue ~ year_factor) {

  f_tw <- parse_formula(formula)

  re1 <- NULL
  re2 <- NULL
  if (length(f_tw$re) == 1L) {
    re1 <- f_tw$re[[1]]
  }
  if (length(f_tw$re) == 2L) {
    re1 <- f_tw$re[[1]]
    re2 <- f_tw$re[[2]]
  }

  if (!is.null(re1)) {
    re_id_k <- fct_to_tmb_num(dat[[as.character(re1)]])
  }
  if (!is.null(re2)) {
    re_id_g <- fct_to_tmb_num(dat[[as.character(re2)]])
  }

  mm1 <- model.matrix(f_tw$fe, data = dat)
  mm_pred1 <- make_pred_mm(mm1, years = unique(dat$year_factor))

  dlls <- getLoadedDLLs()

  if (!is.null(re1) && !is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "tweedie_cpue_2re", FUN.VALUE = TRUE))) {
    load_tmb("tweedie_cpue_2re.cpp")
  }
  if (!is.null(re1) && is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "tweedie_cpue_1re", FUN.VALUE = TRUE))) {
    load_tmb("tweedie_cpue_1re.cpp")
  }
  if (is.null(re1) && is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "tweedie_cpue", FUN.VALUE = TRUE))) {
    load_tmb("tweedie_cpue.cpp")
  }

  # defaults if no random effects:
  random <- NULL
  DLL <- "tweedie_cpue"

  parameters <- list(
    b1_j = rep(0, ncol(mm1)),
    log_phi = 0,
    logit_p = 0
  )

  if (!is.null(re1)) {
    y1_i <- get_response_lme4(formula, dat)
  } else {
    y1_i <- get_response(formula, dat)
  }

  data <- list(
    X1_ij = mm1,
    y1_i = y1_i,
    X1_pred_ij = mm_pred1
  )

  if (!is.null(re1)) {
    data$factor1k_i <- re_id_k
    data$nk1 <- length(unique(re_id_k))
    random <- c("z1_k")
    DLL <- "tweedie_cpue_1re"
    parameters$z1_k <- rep(0, length(unique(re_id_k)))
    parameters$log_sigma_zk1 <- log(0.2)
  }

  if (!is.null(re2)) {
    data$factor1g_i <- re_id_g
    data$ng1 <- length(unique(re_id_g))
    random <- c(random, "z1_g")
    DLL <- "tweedie_cpue_2re"
    parameters$z1_g <- rep(0, length(unique(re_id_g)))
    parameters$log_sigma_zg1 <- log(0.2)
  }

  obj <- TMB::MakeADFun(
    data = data,
    parameters = parameters,
    random = random,
    DLL = DLL,
    checkParameterOrder = TRUE
  )

  opt <- stats::nlminb(
    start = obj$par,
    objective = obj$fn,
    gradient = obj$gr,
    control = list(iter.max = 1000L, eval.max = 1000L)
  )

  message("Getting TMB::sdreport()")
  r <- TMB::sdreport(obj)

  out <- list(
    model = opt, sdreport = r, max_gradient = max(obj$gr(opt$par)),
    years = sort(unique(dat$year)), mm_bin = mm1,
    formula = formula,
    data = dat, parameters = parameters, random = random,
    DLL = DLL, re1 = NA, re2 = NA
  )
  if (!is.null(re1)) {
    out$re1 <- as.factor(as.character(dat[[re1]]))
  }
  if (!is.null(re2)) {
    out$re2 <- as.factor(as.character(dat[[re2]]))
  }
  out
}

#' @param object Output from [fit_cpue_index_tweedie()].
#' @param center Should the predictions be centered by dividing by the geometric
#'   mean?
#'
#' @export
#' @rdname cpue-tweedie
predict_cpue_index_tweedie <- function(object, center = FALSE) {
  if (class(object) == "glmmTMB") {
    sdr <- object$sdr
    yrs <- sort(unique(object$frame$year_factor))

    b <- data.frame(par = names(sdr$par.fixed), est = sdr$par.fixed, se = sqrt(diag(sdr$cov.fixed)))
    b <- b[grepl("^beta$", b$par), , drop = FALSE]
    b <- b[seq_along(yrs), , drop = FALSE]
    b$year <- as.numeric(as.character(yrs))
    b$par <- NULL
    b$se_year <- NA
    b$se_year[1] <- b$se[1]
    b$est_year[1] <- b$est[1]
    # b$se_year[seq(2, nrow(b))] <- sqrt(b$se[2:nrow(b)]^2 - b$se[1]^2)
    # b$est_year[seq(2, nrow(b))] <- b$est[2:nrow(b)] + b$est[1]
    df <- data.frame(year = b$year, Estimate = b$est,
      `Std. Error` = b$se, model = "Combined", stringsAsFactors = FALSE)
    names(df) <- gsub("Std..Error", "Std. Error", names(df))
  } else {
    report_sum <- TMB::summary.sdreport(object$sdreport)
    ii <- grep(
      "log_prediction",
      row.names(report_sum)
    )
    row.names(report_sum) <- NULL
    df <- as.data.frame(report_sum[ii, , drop = FALSE])
    df$year <- object$years
    df$model <- rep("Combined", nrow(df))
  }

  if (center) {
    df <- df %>%
      group_by(model) %>%
      mutate(Estimate = .data$Estimate - mean(.data$Estimate)) %>%
      ungroup()
  }

  df %>%
    rename(se_link = .data$`Std. Error`) %>%
    rename(est_link = .data$Estimate) %>%
    mutate(
      lwr = ifelse(model == "Binomial",
        plogis(est_link - 1.96 * se_link),
        exp(est_link - 1.96 * se_link)
      ),
      upr = ifelse(model == "Binomial",
        plogis(est_link + 1.96 * se_link),
        exp(est_link + 1.96 * se_link)
      ),
      est = ifelse(model == "Binomial",
        plogis(est_link),
        exp(est_link)
      )
    ) %>%
    select(year, model, est_link, se_link, est, lwr, upr)
}

#' @description * `tidy_cpue_index_coefs_tweedie()` extracts coefficients from a CPUE
#'   index standardization model
#' @param include_scale_pars Logical: include observation CV and random effect
#'   like standard deviations in parameter plot?
#' @param re_name Names for the possible random intercepts. Should start with
#'   `RE`.
#'
#' @export
#' @rdname cpue-tweedie

tidy_cpue_index_coefs_tweedie <- function(object, include_scale_pars = FALSE,
  re_name = c("RE 1", "RE 2")) {

  sm <- TMB::summary.sdreport(object$sdreport)
  pars <- row.names(sm)
  row.names(sm) <- seq_len(nrow(sm))
  sm <- as.data.frame(sm)
  sm$pars <- pars

  sm <- sm %>%
    rename(se = .data$`Std. Error`) %>%
    rename(est = .data$Estimate) %>%
    filter(!grepl("prediction", pars))

  sm$par_name <- sm$pars
  sm$par_name[seq(1, ncol(object$mm_bin))] <- c(colnames(object$mm_bin))

  sm$par_name <- sub("f\\(", "", sm$par_name)
  sm$par_name <- sub("\\)", "", sm$par_name)
  sm$par_name <- sub("([0-9.]+$)", " \\1", sm$par_name)
  sm$par_name <- sub("\\(", "", sm$par_name)

  # label REs:
  sm$par_name[grep("^z1_k$", sm$par_name)] <-
    paste(re_name[1], levels(object$re1))

  sm$par_name[grep("^z1_g$", sm$par_name)] <-
    paste(re_name[2], levels(object$re2))

  if (!include_scale_pars) {
    sm <- filter(sm, !grepl("sigma", .data$par_name))
    sm <- filter(sm, !grepl("log_cv", .data$par_name))
    sm <- filter(sm, !grepl("logit_p", .data$par_name))
    sm <- filter(sm, !grepl("log_phi", .data$par_name))
  }
  # sm$par_name <- gsub("log_sigma_zk 1", paste("log_sigma", object$re1, model_prefixes[1]), sm$par_name)
  # sm$par_name <- gsub("log_sigma_zk 2", paste("log_sigma", object$re1, model_prefixes[2]), sm$par_name)
  #
  # sm$par_name <- gsub("log_sigma_zg 1", paste("log_sigma", object$re2, model_prefixes[1]), sm$par_name)
  # sm$par_name <- gsub("log_sigma_zg 2", paste("log_sigma", object$re2, model_prefixes[2]), sm$par_name)

  sm$par_group <- sub("[0-9. ]+$", "", sm$par_name)
  sm$par_group <- sub("-[a-zA-Z]+$", "", sm$par_group)

  # sm$par_name <- gsub(model_prefixes[[1]], "", sm$par_name)
  # sm$par_name <- gsub(model_prefixes[[2]], "", sm$par_name)

  # sm$par_group <- gsub("log_sigma_zk", paste("log_sigma", object$re1), sm$par_group)
  # sm$par_group <- gsub("log_sigma_zg", paste("log_sigma", object$re2), sm$par_group)

  sm
}

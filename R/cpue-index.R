load_tmb_cpue <- function() {
  tmb_cpp <- system.file("tmb", "delta_gamma_cpue.cpp", package = "gfplot")
  TMB::compile(tmb_cpp)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp)))
}

load_tmb_2re_cpue <- function() {
  tmb_cpp_2re <- system.file("tmb", "delta_gamma_cpue_2re.cpp", package = "gfplot")
  TMB::compile(tmb_cpp_2re)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp_2re)))
}

load_tmb_1re_cpue <- function() {
  tmb_cpp_1re <- system.file("tmb", "delta_gamma_cpue_1re.cpp", package = "gfplot")
  TMB::compile(tmb_cpp_1re)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp_1re)))
}

# Random effect indices in C++:
fct_to_tmb_num <- function(x) {
  as.numeric(as.factor(as.character(x))) - 1
}

parse_formula <- function(f) {
  terms_ <- attributes(stats::terms(f))$term.labels
  re <- terms_[grepl("1 \\|", terms_)]
  re <- gsub("1 \\| ", "", re)
  fe <- terms_[!grepl("1 \\|", terms_)]
  response <- all.vars(stats::terms(f))[1]
  fe <- as.formula(paste(response, "~", paste(fe, collapse = " + ")))
  list(response = response, fe = fe, re = re)
}

# https://github.com/stan-dev/rstanarm/blob/55c91fefe8f39f00a68d37944357b96aef1de9f8/R/misc.R
make_glmerControl <- function(...) {
  lme4::glmerControl(check.nlev.gtreq.5 = "ignore",
    check.nlev.gtr.1 = "stop",
    check.nobs.vs.rankZ = "ignore",
    check.nobs.vs.nlev = "ignore",
    check.nobs.vs.nRE = "ignore", ...)
}

get_response <- function(form, data) {
  stats::model.response(stats::model.frame(form, data = data))
}

# https://github.com/stan-dev/rstanarm/blob/master/R/stan_glmer.R
get_response_lme4 <- function(formula, data) {
  call <- match.call(expand.dots = TRUE)
  mc <- match.call(expand.dots = FALSE)
  data <- data
  mc[[1]] <- quote(lme4::glFormula)
  mc$control <- make_glmerControl()
  mc$data <- data
  glmod <- eval(mc, parent.frame())
  X <- glmod$X
  y <- glmod$fr[, as.character(glmod$formula[2L])]
  if (is.matrix(y) && ncol(y) == 1L)
    y <- as.vector(y)
  y
}

#' Commercial CPUE index standardization
#'
#' @description * `fit_cpue_index()` fits a delta-Gamma commercial CPUE
#'   standardization model
#'
#' @param dat A data frame from [tidy_cpue_index()], or a similarly
#'   formatted data frame.
#' @param formula_binomial Formula for the binomial (presence-absence) model.
#'   See details.
#' @param formula_gamma Formula for the Gamma (positive) model. See details.
#' @details
#' Formulas should be specified in the way a GLM formula would be specified in R
#' or as a GLMM with **up to 2 random intercepts** would be specified in
#' [lme4::lmer()]. E.g. `pos_catch ~ year_factor + (1 | vessel)` or
#' `cpue ~ year_factor + (1 | vessel) + (1 | locality)`. Your formula
#' **must** have a predictor named `year_factor` for the other functions in this
#' package to interact with the output properly. The fixed effect components of
#' the binomial and Gamma components do not need to be the same, but the random
#' effect components must be the same.
#'
#' Because of how the formula parsing is performed, you **cannot** include any
#' operations in the response variable. For example, you can't use `catch/effort
#' ~ year_factor`. Instead you must create a new column, e.g. `cpue` and use
#' that as the response: `cpue ~ year_factor`.
#'
#' You may want to take advantage of the function [gfplot::f()] for conveniently
#' turning predictors into factors with their base level set to the most common
#' factor level and their factor names with consistent name lengths (e.g. '001',
#' '020').
#'
#' The model is internally fitted with TMB. As a result, you will have to have a
#' C++ compiler installed to compile the models. On Windows, search for Rtools.
#'
#' @examples
#' \donttest{
#' # A simulated example:
#' set.seed(1)
#' d <- sim_cpue()
#' d$cpue <- d$spp_catch / d$hours_fished
#' d$vessel <- f(d$vessel)
#'
#' m <- fit_cpue_index(d,
#'   formula_binomial = pos_catch ~ year_factor + vessel,
#'   formula_gamma = cpue ~ year_factor + vessel
#' )
#'
#' plot_cpue_index_coefs(m)
#'
#' predict_cpue_index(m) %>%
#'   plot_cpue_index()
#'
#' plot_cpue_index_jk(m, terms = "f(vessel)")
#' }
#' \donttest{
#' # An example with PBS data:
#' # (the data extraction will be slow)
#'
#' d <- get_cpue_index(gear = "bottom trawl")
#' walleye <- tidy_cpue_index(d, "walleye pollock",
#'   area_grep_pattern = "5[CDE]+")
#'
#' m <- fit_cpue_index(walleye)
#'
#' plot_cpue_index_coefs(m)
#'
#' predict_cpue_index(m) %>%
#'   plot_cpue_index()
#'
#' plot_cpue_index_jk(m)
#' }
#' @rdname cpue-index
#' @export
fit_cpue_index <- function(dat,
  formula_binomial = cpue ~ year_factor,
  formula_gamma = spp_catch / hours_fished ~ year_factor) {

  f_bin <- parse_formula(formula_binomial)
  f_pos <- parse_formula(formula_gamma)

  if (!identical(f_bin$re, f_pos$re))
    stop("Random effects for the binomial and positive components must be identical.")

  re1 <- NULL
  re2 <- NULL
  if (length(f_bin$re) == 1L) {
    re1 <- f_bin$re[[1]]
  }
  if (length(f_bin$re) == 2L) {
    re1 <- f_bin$re[[1]]
    re2 <- f_bin$re[[2]]
  }
  pos_dat <- dat[dat$pos_catch == 1, , drop = FALSE]

  if (!is.null(re1)) {
    bin_re_id_k <- fct_to_tmb_num(dat[[as.character(re1)]])
    pos_re_id_k <- fct_to_tmb_num(filter(dat, pos_catch == 1)[[as.character(re1)]])
  }
  if (!is.null(re2)) {
    bin_re_id_g <- fct_to_tmb_num(dat[[as.character(re2)]])
    pos_re_id_g <- fct_to_tmb_num(filter(dat, pos_catch == 1)[[as.character(re2)]])
  }

  mm1 <- model.matrix(f_bin$fe, data = dat)
  mm2 <- model.matrix(f_pos$fe, data = pos_dat)
  mm_pred1 <- make_pred_mm(mm1, years = unique(dat$year_factor))
  mm_pred2 <- make_pred_mm(mm2, years = unique(pos_dat$year_factor))

  dlls <- getLoadedDLLs()

  if (!is.null(re1) && !is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "delta_gamma_cpue_2re", FUN.VALUE = TRUE))) {
    load_tmb_2re_cpue()
  }
  if (!is.null(re1) && is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "delta_gamma_cpue_1re", FUN.VALUE = TRUE))) {
    load_tmb_1re_cpue()
  }
  if (is.null(re1) && is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "delta_gamma_cpue", FUN.VALUE = TRUE))) {
    load_tmb_cpue()
  }

  # defaults if no random effects:
  random <- NULL
  DLL <- "delta_gamma_cpue"
  DLL <- "tweedie_cpue"

  # Get some reasonable starting values for speed:
  m_bin <- tryCatch({speedglm::speedglm(f_bin$fe,
    data = dat,
    family = binomial(link = "logit"))}, error = function(e) NA)

  m_pos <- tryCatch({stats::glm(f_pos$fe,
    data = pos_dat,
    family = Gamma(link = "log"), control = list(maxit = 100))},
    error = function(e) NA)
  if (!is.na(m_bin)[[1]]) {
    b1_j_start <- coef(m_bin) + stats::rnorm(ncol(mm1), 0, 0.02)
  } else {
    b1_j_start <- stats::rnorm(ncol(mm1), 0, 0.1)
  }
  if (!is.na(m_pos)[[1]]) {
    b2_j_start <- coef(m_pos) + stats::rnorm(ncol(mm2), 0, 0.02)
    # CV = sqrt(1/gamma_shape):
    log_cv_start <-  log(1/sqrt(summary(m_pos)$dispersion))
  } else {
    b2_j_start <- stats::rnorm(ncol(mm2), 0, 0.1)
    log_cv_start <- log(1)
  }

  # ---------------------------
  parameters <- list(
    b1_j = b1_j_start,
    p = 1.5,
    log_phi = 0)

  data <- list(
    X1_ij = mm1,
    y1_i = dat$cpue,
    X1_pred_ij = mm_pred1
  )

  obj <- TMB::MakeADFun(
    data = data,
    parameters = parameters,
    random = random,
    DLL = DLL,
    checkParameterOrder = TRUE
  )

  lower = rep(-Inf, length(obj$par))
  upper = rep(Inf, length(obj$par))
  lower[names(obj$par) == "p"] <- 1
  upper[names(obj$par) == "p"] <- 2
  opt <- stats::nlminb(
    start = obj$par,
    objective = obj$fn,
    gradient = obj$gr,
    lower = lower,
    upper = upper,
    control = list(iter.max = 2000L, eval.max = 2000L)
  )
  # -----------------------------


  parameters <- list(
    b1_j = b1_j_start,
    b2_j = b2_j_start,
    log_cv = log_cv_start)

  if (!is.null(re1)) {
    y1_i <- get_response_lme4(formula_binomial, dat)
    y2_i <- get_response_lme4(formula_gamma, pos_dat)
  } else {
    y1_i <- get_response(formula_binomial, dat)
    y2_i <- get_response(formula_gamma, pos_dat)
  }

  data <- list(
    X1_ij = mm1,
    y1_i = y1_i,
    X2_ij = mm2,
    y2_i = y2_i,
    X1_pred_ij = mm_pred1,
    X2_pred_ij = mm_pred2
  )

  if (!is.null(re1)) {
    data$factor1k_i <- bin_re_id_k
    data$factor2k_i <- pos_re_id_k
    data$nk1 <- length(unique(bin_re_id_k))
    data$nk2 <- length(unique(pos_re_id_k))
    random <- c("z1_k", "z2_k")
    DLL <- "delta_gamma_cpue_1re"
    parameters$z1_k <- stats::rnorm(length(unique(bin_re_id_k)), 0, 0.2)
    parameters$z2_k <- stats::rnorm(length(unique(pos_re_id_k)), 0, 0.2)
    parameters$log_sigma_zk1 <- log(0.2)
    parameters$log_sigma_zk2 <- log(0.2)
  }

  if (!is.null(re2)) {
    data$factor1g_i <- bin_re_id_g
    data$factor2g_i <- pos_re_id_g
    data$ng1 <- length(unique(bin_re_id_g))
    data$ng2 <- length(unique(pos_re_id_g))
    random <- c(random, "z1_g", "z2_g")
    DLL <- "delta_gamma_cpue_2re"
    parameters$z1_g <- stats::rnorm(length(unique(bin_re_id_g)), 0, 0.2)
    parameters$z2_g <- stats::rnorm(length(unique(pos_re_id_g)), 0, 0.2)
    parameters$log_sigma_zg1 <- log(0.2)
    parameters$log_sigma_zg2 <- log(0.2)
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
    control = list(iter.max = 2000L, eval.max = 2000L)
  )

  message("Getting TMB::sdreport()")
  r <- TMB::sdreport(obj)

  out <- list(
    model = opt, sdreport = r, max_gradient = max(obj$gr(opt$par)),
    years = sort(unique(dat$year)), mm_bin = mm1, mm_pos = mm2,
    f_bin = formula_binomial, f_pos = formula_gamma,
    data = dat, parameters = parameters, random = random,
    DLL = DLL, re1_bin = NA, re1_pos = NA, re2_bin = NA, re2_pos = NA,
    re1 = NA, re2 = NA
  )
  if (!is.null(re1)) {
    out$re1_bin <- as.factor(as.character(dat[[re1]]))
    out$re1_pos <- as.factor(as.character(dat[[re1]]))
    out$re1 <- re1
  }
  if (!is.null(re2)) {
    out$re2_bin <- as.factor(as.character(dat[[re2]]))
    out$re2_pos <- as.factor(as.character(dat[[re2]]))
    out$re2 <- re2
  }
  out
}

#' @description * `predict_cpue_index()` predicts from a delta-Gamma
#'   commercial CPUE standardization model
#'
#' @param object Model output from [fit_cpue_index()].
#' @param center Should the index be centered by subtracting the mean in link space?
#'
#' @export
#'
#' @rdname cpue-index

predict_cpue_index <- function(object, center = FALSE) {
  report_sum <- TMB::summary.sdreport(object$sdreport)
  ii <- grep(
    "log_prediction|linear_prediction1_i|linear_prediction2_i",
    row.names(report_sum)
  )
  row.names(report_sum) <- NULL
  df <- as.data.frame(report_sum[ii, , drop = FALSE])
  df$year <- rep(object$years, 3L)
  df$model <- rep(c("Combined", "Binomial", "Gamma"), each = nrow(df) / 3L)
  df$model <- factor(df$model, levels = c("Combined", "Binomial", "Gamma"))

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

#' @description * `plot_cpue_index()` plots a delta-Gamma commercial CPUE
#'   standardization model
#'
#' @param predicted_dat Input data frame, for example from [predict_cpue_index()].
#' @param all_models Logical for whether to plot all models (`TRUE`) or just the
#'   combined model (`FALSE`).
#'
#' @export
#'
#' @rdname cpue-index

plot_cpue_index <- function(predicted_dat, all_models = TRUE) {
  if (!all_models) {
    predicted_dat <- filter(predicted_dat, model == "Combined")
  }

  g <- ggplot(predicted_dat, aes_string("year", "est", ymin = "upr", ymax = "lwr")) +
    ggplot2::geom_ribbon(fill = "grey70") +
    geom_line() +
    theme_pbs() +
    labs(y = "CPUE index", x = "") +
    ylim(0, NA)
  if (all_models) {
    g <- g + facet_wrap(~model, scales = "free_y")
  }
  g
}

#' @description * `tidy_cpue_index_coefs()` extracts coefficients from a CPUE
#'   index standardization model
#' @param include_scale_pars Logical: include observation CV and random effect
#'   like standard deviations in parameter plot?
#'
#' @export
#' @rdname cpue-index

tidy_cpue_index_coefs <- function(object, include_scale_pars = FALSE) {
  model_prefixes <- c("Binomial", "Gamma")

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
  sm$par_name[seq(1, ncol(object$mm_bin) + ncol(object$mm_pos))] <-
    c(paste(model_prefixes[[1]], colnames(object$mm_bin)),
      paste(model_prefixes[[2]], colnames(object$mm_pos)))

  sm$par_name <- sub("f\\(", "", sm$par_name)
  sm$par_name <- sub("\\)", "", sm$par_name)
  sm$par_name <- sub("([0-9.]+$)", " \\1", sm$par_name)
  sm$par_name <- sub("\\(", "", sm$par_name)

  # label REs:
  sm$par_name[grep("^z1_k$", sm$par_name)] <-
    paste(model_prefixes[[1]], object$re1, levels(object$re1_bin))
  sm$par_name[grep("^z2_k$", sm$par_name)] <-
    paste(model_prefixes[[2]], object$re1, levels(object$re1_pos))

  sm$par_name[grep("^z1_g$", sm$par_name)] <-
    paste(model_prefixes[[1]], object$re2, levels(object$re2_bin))
  sm$par_name[grep("^z2_g$", sm$par_name)] <-
    paste(model_prefixes[[2]], object$re2, levels(object$re2_pos))

  if (!include_scale_pars) {
    sm <- filter(sm, !grepl("sigma", .data$par_name))
    sm <- filter(sm, !grepl("log_cv", .data$par_name))
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

#' @description * `plot_cpue_index_coefs()` plots coefficients from a CPUE index
#'   standardization model
#'
#' @param regex A regular expression as a character vector of length 2 to
#' transform the coefficient names from something (element 1) to something
#' (element 2).
#'
#' @details Note that for coefficients for predictors treated as factors, the
#'   coefficients represent the difference from the base level factor. For
#'   example, if January was the base month, month 02 represents the estimated
#'   difference between February and January. The default setup of the function
#'   [gfplot::f()] sets the reference level as the most common factor level.
#'
#' @export
#'
#' @rdname cpue-index

plot_cpue_index_coefs <- function(object, regex = c(", base_[a-z]+", "")) {
  sm <- tidy_cpue_index_coefs(object)
  model_prefixes <- c("Bin.", "Pos.")

  sm <- mutate(sm, se_too_big = se > 10, se = ifelse(se > 10, NA, se))
  sm <- mutate(sm, type = grepl(model_prefixes[[1]], par_group))

  sm <- mutate(sm, par_name = gsub(regex[[1]], regex[[2]], par_name))
  sm <- mutate(sm, par_group = gsub(regex[[1]], regex[[2]], par_group))
  sm <- mutate(sm, par_name = gsub("[A-Za-z]+. [a-z]+ ", "", par_name))

  sm <- sm %>%
    filter(!grepl("Intercept", par_name)) %>%
    filter(!grepl("year", par_name))

  cols <- RColorBrewer::brewer.pal(3, "Blues")

  ggplot(sm, aes_string("est", "forcats::fct_rev(par_name)",
    yend = "forcats::fct_rev(par_name)", colour = "type"
  )) +
    ggplot2::geom_segment(aes_string(
      x = "est - 1.96 * se",
      xend = "est + 1.96 * se"
    ), lwd = 0.5) +
    ggplot2::geom_segment(aes_string(
      x = "est - 0.67 * se",
      xend = "est + 0.67 * se"
    ), lwd = 1.25) +
    geom_point(aes_string(shape = "se_too_big"), bg = "white") +
    scale_shape_manual(values = c("TRUE" = 4, "FALSE" = 21)) +
    scale_colour_manual(values = c("TRUE" = "grey30", "FALSE" = cols[[3]])) +
    facet_wrap(~par_group, scales = "free") +
    theme_pbs() + guides(shape = FALSE, colour = FALSE) +
    labs(y = "", x = "Coefficient value (logit or log space)")
}

#' @description * `plot_cpue_index_jk()` "jackknifes" out terms one by one to
#'   test the sensitivity of the standardization model to any one term.
#'
#' @param terms A character vector of terms to jackknife out.
#' @param return_data Logical: should the data be returned?
#'
#' @export
#'
#' @rdname cpue-index

plot_cpue_index_jk <- function(object,
                               terms = c(
                                 "f(month)", "f(vessel)", "f(locality)",
                                 "f(depth)", "f(latitude)"
                               ),
                               return_data = FALSE) {
  pos_dat <- object$data[object$data$pos_catch == 1, , drop = FALSE]

  mm1 <- model.matrix(pos_catch ~ year_factor, data = object$data)
  mm2 <- model.matrix(log(spp_catch / hours_fished) ~ year_factor,
    data = pos_dat
  )
  mm1 <- make_pred_mm(mm1, years = object$years)
  mm2 <- make_pred_mm(mm2, years = object$years)
  m_bin <- speedglm::speedglm(pos_catch ~ year_factor,
    data = object$data,
    family = binomial(link = "logit")
  )
  m_pos <- lm(log(spp_catch / hours_fished) ~ year_factor, data = pos_dat)
  p1 <- plogis(mm1 %*% coef(m_bin))
  p2 <- exp(mm2 %*% coef(m_pos))
  none <- data.frame(
    year = object$years, term = "none", pred = p1 * p2,
    stringsAsFactors = FALSE
  )

  mm1 <- model.matrix(object$f_bin, data = object$data)
  mm2 <- model.matrix(object$f_pos, data = pos_dat)
  mm1 <- make_pred_mm(mm1, years = object$years)
  mm2 <- make_pred_mm(mm2, years = object$years)
  m_bin <- speedglm::speedglm(object$f_bin,
    data = object$data,
    family = binomial(link = "logit")
  )
  m_pos <- lm(object$f_pos, data = pos_dat)
  p1 <- plogis(mm1 %*% coef(m_bin))
  p2 <- exp(mm2 %*% coef(m_pos))
  full <- data.frame(
    year = object$years, term = "all", pred = p1 * p2,
    stringsAsFactors = FALSE
  )

  if (!is.null(terms)) {
    fitm <- function(drop_term) {
      message(paste("Dropping", drop_term, "..."))
      f1 <- update.formula(
        formula(m_bin),
        as.formula(paste0(". ~ . -", drop_term))
      )
      f2 <- update.formula(
        formula(m_pos),
        as.formula(paste0(". ~ . -", drop_term))
      )

      mm1 <- model.matrix(f1, data = object$data)
      mm2 <- model.matrix(f2, data = pos_dat)
      mm1 <- make_pred_mm(mm1, years = object$years)
      mm2 <- make_pred_mm(mm2, years = object$years)
      m_bin <- speedglm::speedglm(f1,
        data = object$data,
        family = binomial(link = "logit")
      )
      m_pos <- lm(f2, data = pos_dat)
      p1 <- plogis(mm1 %*% coef(m_bin))
      p2 <- exp(mm2 %*% coef(m_pos))
      data.frame(
        year = object$years, term = drop_term, pred = p1 * p2,
        stringsAsFactors = FALSE
      )
    }

    out <- lapply(terms, fitm)
    out <- do.call("rbind", out)
  }

  temp <- bind_rows(none, full)


  if (!is.null(terms)) {
    out <- bind_rows(temp, out)
  } else {
    out <- temp
  }

  out <- group_by(out, term) %>%
    mutate(pred = pred / exp(mean(log(pred)))) %>%
    ungroup()

  out$term <- sub("f\\(", "", out$term)
  out$term <- sub("\\)", "", out$term)
  out$term <- paste("Without", out$term)
  out$term <- sub("Without none", "Unstandardized", out$term)
  out$term <- sub("Without all", "Standardized", out$term)

  all <- filter(out, .data$term == "Standardized") %>% select(-.data$term)
  out_without_all <- filter(out, .data$term != "Standardized")

  all_facets <- do.call("rbind",
    replicate(length(unique(out_without_all$term)), all, simplify = FALSE))
  all_facets$term <- rep(unique(out_without_all$term), each = length(unique(all_facets$year)))

  g <- ggplot(
    out_without_all,
    aes_string("year", "pred", colour = "term")
  ) +
    geom_line(
      data = all_facets, lty = "31", lwd = 1.0,
      colour = "grey70"
    ) +
    geom_line(lwd = 1.0) +
    theme_pbs() + guides(size = FALSE, colour = FALSE) +
    labs(colour = "Excluded", y = "Relative CPUE index", x = "") +
    facet_wrap(~term)

  if (return_data) {
    return(out)
  } else {
    return(g)
  }
}

#' Simulate fake commercial CPUE data for examples and testing
#'
#' @param cv The CV for the positive timeseries.
#' @param n_samples The number of samples or fishing events per vessel per
#' year.
#' @param n_years The number of years.
#' @param n_vessels The number of the vessels.
#'
#' @export
#' @examples
#' sim_cpue()
sim_cpue <- function(cv = 0.3, n_samples = 20, n_years = 15,
                     n_vessels = 8) {
  fake_fleet <- expand.grid(
    year_factor = as.factor(seq(1996, 1996 + (n_years - 1))),
    vessel = rep(as.factor(as.character(seq(100, 100 + (n_vessels - 1)))),
      each = n_samples
    )
  )

  vessel_effects <- data.frame(
    vessel = unique(fake_fleet$vessel),
    vessel_effect = stats::rnorm(length(unique(fake_fleet$vessel)), 0, 1)
  )

  year_effects <- data.frame(
    year_factor = unique(fake_fleet$year),
    year_effect = stats::rnorm(length(unique(fake_fleet$year)), 0, 1)
  )

  fake_fleet <- inner_join(fake_fleet, vessel_effects, by = "vessel")
  fake_fleet <- inner_join(fake_fleet, year_effects, by = "year_factor")

  fake_fleet <- mutate(fake_fleet,
    pos_catch = stats::rbinom(nrow(fake_fleet),
      size = 1,
      prob = stats::plogis(vessel_effect + year_effect)
    ),
    spp_catch = ifelse(pos_catch == 1,
      stats::rgamma(nrow(fake_fleet),
        shape = 1/(cv^2),
        scale = exp(vessel_effect + year_effect) / (1/cv^2)), 0),
    hours_fished = 1
  )

  fake_fleet$year <- as.numeric(as.character(fake_fleet$year_factor))

  dplyr::as.tbl(fake_fleet)
}

#' Force factors to be sequential within the positive or binary data sets for TMB
#'
#' This is necessary because sometimes, for example, a certain depth factor
#' level appears in the binary data set (because this depth was fished at), but
#' not in the positive data set (because a given species was now are caught at
#' that depth). This function also sets the base/reference level of the factor.
#'
#' @param x A vector of factor/character values.
#' @param ref A function to define the base "reference" level or a character
#'   object definining the base level. Defaults to the most common level.
#'
#' @export
#' @examples
#' a <- factor(c("a", "b", "c", "c"))
# get_most_common_level(a)
#' f(a)
#' f(a, "b")

f <- function(x, ref = get_most_common_level) {
  out <- as.factor(as.character(x))
  if (is.character(ref)) {
    stats::relevel(out, ref = ref)
  } else {
    stats::relevel(out, ref = ref(x))
  }
}

#' @export
#' @rdname f
get_most_common_level <- function(x) {
  rev(names(sort(table(x))))[[1]]
}

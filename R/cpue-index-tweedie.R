load_tmb <- function(file = "tweedie_cpue.cpp") {
  tmb_cpp <- system.file("tmb", file, package = "gfplot")
  TMB::compile(tmb_cpp)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp)))
}

#' Commercial CPUE index standardization (Tweedie version)
#'
#' @param dat Data TODO
#' @param formula Formula TODO
#'
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' d <- sim_cpue()
#' d$cpue <- d$spp_catch / d$hours_fished
#' d$vessel <- f(d$vessel)
#'
#' m1 <- fit_cpue_index_tweedie(d, cpue ~ year_factor + vessel)
#' m2 <- fit_cpue_index_tweedie(d, cpue ~ year_factor + (1 | vessel))
#'
#' }

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
    logit_p = 0,
    log_phi = 0)

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

predict_cpue_index_tweedie <- function(object, center = FALSE) {
  report_sum <- TMB::summary.sdreport(object$sdreport)
  ii <- grep(
    "log_prediction",
    row.names(report_sum)
  )
  row.names(report_sum) <- NULL
  df <- as.data.frame(report_sum[ii, , drop = FALSE])
  df$year <- object$years
  df$model <- rep("Combined", nrow(df))

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

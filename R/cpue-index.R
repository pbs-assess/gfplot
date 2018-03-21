load_tmb_cpue <- function() {
  tmb_cpp <- system.file("tmb", "deltalognormal.cpp", package = "gfplot")
  TMB::compile(tmb_cpp)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp)))
}

#' Commercial CPUE index standardization
#'
#' @description * `fit_cpue_index()` fits a delta-lognormal commercial CPUE
#'   standardization model
#'
#' @param dat A data frame from [tidy_cpue_index()], or a similarly
#'   formatted data frame.
#' @param formula_binomial Formula for the binomial model.
#' @param formula_lognormal Formula for the lognormal model.
#'
#' @examples
#' # A simulated example:
#' set.seed(1)
#' d <- sim_cpue()
#' d
#'
#' m <- fit_cpue_index(d,
#'   formula_binomial = pos_catch ~ year_factor + f(vessel),
#'   formula_lognormal = log(spp_catch / hours_fished) ~ year_factor + f(vessel)
#' )
#'
#' plot_cpue_index_coefs(m)
#'
#' predict_cpue_index(m) %>%
#'   plot_cpue_index()
#'
#' plot_cpue_index_jk(m, terms = "f(vessel)")
#'
#' \dontrun{
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
#' @export
#' @rdname cpue-index

fit_cpue_index <- function(dat,
                           formula_binomial = pos_catch ~ year_factor + f(month)
                             + f(vessel) + f(locality) + f(depth) + f(latitude),
                           formula_lognormal = log(spp_catch / hours_fished) ~
                           year_factor + f(month) + f(vessel) +
                             f(locality) + f(depth) + f(latitude)) {
  pos_dat <- dat[dat$pos_catch == 1, , drop = FALSE]

  mm1 <- model.matrix(formula_binomial, data = dat)
  mm2 <- model.matrix(formula_lognormal, data = pos_dat)
  mm_pred2 <- make_pred_mm(mm2, years = unique(dat$year_factor))
  mm_pred1 <- make_pred_mm(mm1, years = unique(pos_dat$year_factor))

  # get some close starting values:
  m_bin <- speedglm::speedglm(formula_binomial,
    data = dat,
    family = binomial(link = "logit")
  )
  m_pos <- lm(formula_lognormal, data = pos_dat)

  dlls <- getLoadedDLLs()
  if (!any(vapply(dlls, function(x)
    x[["name"]] == "deltalognormal", FUN.VALUE = TRUE))) {
    load_tmb_cpue()
  }

  message("Fitting CPUE model ...")
  obj <- TMB::MakeADFun(
    data = list(
      X1_ij = mm1, y1_i = dat$pos_catch,
      X2_ij = mm2,
      y2_i = log(pos_dat$spp_catch / pos_dat$hours_fished), # TODO take from formula
      X1_pred_ij = mm_pred1, X2_pred_ij = mm_pred2
    ),
    parameters = list(
      # b1_j = coef(m_bin) + rnorm(length(coef(m_bin)), 0, 0.001),
      # b2_j = coef(m_pos) + rnorm(length(coef(m_pos)), 0, 0.001),
      b1_j = rnorm(ncol(mm_pred1), 0, 0.2),
      b2_j = rnorm(ncol(mm_pred2), 0, 0.2),
      log_sigma = log(summary(m_pos)$sigma) + rnorm(1, 0, 0.001)
    ),
    # log_sigma = rnorm(1, 0, 0.2)),
    DLL = "deltalognormal"
  )

  opt <- stats::nlminb(
    start = obj$par,
    objective = obj$fn,
    gradient = obj$gr, control = list(
      iter.max = 1000L,
      eval.max = 1000L
    )
  )

  message("Getting sdreport ...")
  r <- TMB::sdreport(obj)

  list(
    model = opt, sdreport = r, max_gradient = max(obj$gr(opt$par)),
    years = sort(unique(dat$year)), mm_bin = mm1, mm_pos = mm2,
    f_bin = formula_binomial, f_pos = formula_lognormal,
    data = dat
  )
}

#' @description * `predict_cpue_index()` predicts from a delta-lognormal
#'   commercial CPUE standardization model
#'
#' @param object Model output from [fit_cpue_index()].
#' @param center Should the index be centered by subtracting the mean in link space?
#'
#' @export
#'
#' @rdname cpue-index

predict_cpue_index <- function(object, center = FALSE) {
  report_sum <- summary(object$sdreport)
  ii <- grep(
    "log_prediction|linear_prediction1_i|linear_prediction2_i",
    row.names(report_sum)
  )
  row.names(report_sum) <- NULL
  df <- as.data.frame(report_sum[ii, , drop = FALSE])
  df$year <- rep(object$years, 3L)
  df$model <- rep(c("Combined", "Binomial", "Lognormal"), each = nrow(df) / 3L)
  df$model <- factor(df$model, levels = c("Combined", "Binomial", "Lognormal"))

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

#' @description * `plot_cpue_index()` plots a delta-lognormal commercial CPUE
#'   standardization model
#'
#' @param predicted_dat Input data frame, for example from [predict_cpue_index()].
#' @param all_models TODO
#'
#' @export
#' @return A ggplot object
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
    g <- g + facet_wrap(~ model, scales = "free_y")
  }
  g
}

#' @description * `tidy_cpue_index_coefs()` extracts coefficients from a CPUE
#'   index standardization model
#'
#' @export
#' @rdname cpue-index

tidy_cpue_index_coefs <- function(object) {
  model_prefixes <- c("Bin.", "Pos.")

  sm <- summary(object$sdreport)
  pars <- row.names(sm)
  row.names(sm) <- seq_len(nrow(sm))
  sm <- as.data.frame(sm)
  sm$pars <- pars

  sm <- sm %>%
    rename(se = .data$`Std. Error`) %>%
    rename(est = .data$Estimate) %>%
    filter(!grepl("prediction", pars))
  sm$par_name <- c(
    paste(model_prefixes[[1]], colnames(object$mm_bin)),
    paste(model_prefixes[[2]], colnames(object$mm_pos)),
    "log_sigma"
  )

  sm$par_name <- sub("f\\(", "", sm$par_name)
  sm$par_name <- sub("\\)", "", sm$par_name)
  sm$par_name <- sub("([0-9.]+$)", " \\1", sm$par_name)
  sm$par_name <- sub("\\(", "", sm$par_name)
  sm$par_group <- sub("[0-9. ]+$", "", sm$par_name)

  sm
}

#' @description * `plot_cpue_index_coefs()` plots coefficients from a CPUE index
#'   standardization model
#'
#' @details Note that for coefficients for predictors treated as factors (i.e.
#'   likely all of the predictors), the coefficients represent the difference
#'   from the base level factor, which would be the first factor level
#'   alpha-numerically. For example, month 02 represents the estimated
#'   difference between February and January.
#'
#' @return A ggplot object
#' @export
#'
#' @rdname cpue-index

plot_cpue_index_coefs <- function(object) {
  sm <- tidy_cpue_index_coefs(object)
  model_prefixes <- c("Bin.", "Pos.")
  sm$par_group <- forcats::fct_relevel(sm$par_group, "log_sigma", after = Inf)
  sm <- mutate(sm, se_too_big = se > 10, se = ifelse(se > 10, NA, se))
  sm <- mutate(sm, type = grepl(model_prefixes[[1]], par_group))

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
    facet_wrap(~ par_group, scales = "free") +
    theme_pbs() + guides(shape = FALSE, colour = FALSE) +
    labs(y = "", x = "Coefficient value")
}

#' @description * `plot_cpue_index_jk()` TODO
#'
#' @param terms TODO
#'
#' @return TODO
#' @export
#'
#' @rdname cpue-index

plot_cpue_index_jk <- function(object,
                               terms = c(
                                 "f(month)", "f(vessel)", "f(locality)",
                                 "f(depth)", "f(latitude)"
                               )) {
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
  out <- bind_rows(none, full) %>%
    bind_rows(out)
  out <- group_by(out, term) %>%
    mutate(pred = pred / exp(mean(log(pred)))) %>%
    ungroup()

  out$term <- sub("f\\(", "", out$term)
  out$term <- sub("\\)", "", out$term)
  out$term <- paste("Without", out$term)

  all <- filter(out, .data$term == "Without all") %>% select(-.data$term)
  out$term <- sub("Without none", "Unstandardized", out$term)

  ggplot(
    filter(out, term != "Without all"),
    aes_string("year", "pred", colour = "term")
  ) +
    geom_line(
      data = all, lty = "31", lwd = 1.0,
      aes_string("year", "pred"), colour = "grey70", inherit.aes = FALSE
    ) +
    geom_line(lwd = 1.0) +
    theme_pbs() + guides(size = FALSE, colour = FALSE) +
    labs(colour = "Excluded", y = "Relative CPUE index", x = "") +
    facet_wrap(~ term)
}

#' Simulate fake commercial CPUE data for examples and testing
#'
#' @param sigma TODO
#' @param n_samples TODO
#' @param n_years TODO
#' @param n_vessels TODO
#'
#' @export
#' @examples
#' sim_cpue()
sim_cpue <- function(sigma = 0.4, n_samples = 20, n_years = 15,
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
      rlnorm(nrow(fake_fleet),
        meanlog = vessel_effect + year_effect,
        sdlog = 0.3
      ), 0
    ),
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
#' that depth).
#'
#' @param x A parameter name
#'
#' @export
f <- function(x) as.factor(as.character(x))

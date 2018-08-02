# Simulate a CPUE timeseries
#
# This function simulates a CPUE timeseries and fits the model to it to check
# for accuracy and coverage of confidence intervals.
#
# @param make_plots If `TRUE` then a series of diagnostic plots will be made.
# @param cv The cv parameter for the Gamma distribution for the
#   positive timeseries.
# @param n_samples The number of samples or fishing events per vessel per
# year.
# @param n_years The number of years.
# @param n_vessels The number of the vessels.

sim_cpue_index <- function(make_plots = TRUE, cv = 0.3, n_samples = 10,
  n_years = 20, n_vessels = 10) {
  fake_fleet <- sim_cpue(
    cv = cv, n_samples = n_samples,
    n_years = n_years, n_vessels = n_vessels
  )

  year_effects <- select(fake_fleet, year_effect) %>% unique()
  vessel_effects <- select(fake_fleet, vessel_effect) %>% unique()

  m <- fit_cpue_index(fake_fleet,
    formula_binomial = pos_catch ~
      year_factor + f(vessel, function(x) as.character(x)[[1]]),
    formula_gamma = spp_catch / hours_fished ~
      year_factor + f(vessel, function(x) as.character(x)[[1]])
  )

  if (make_plots) {
    g1 <- predict_cpue_index(m) %>%
      plot_cpue_index()

    # g2 <- plot_cpue_index_jk(m, terms = "f(vessel)")
  }
  sm <- tidy_cpue_index_coefs(m)

  intercept <- year_effects$year_effect[1] + vessel_effects$vessel_effect[1]
  sm$true_b <- c(
    intercept,
    year_effects$year_effect[-1] - year_effects$year_effect[1],
    vessel_effects$vessel_effect[-1] - vessel_effects$vessel_effect[1],
    intercept,
    year_effects$year_effect[-1] - year_effects$year_effect[1],
    vessel_effects$vessel_effect[-1] - vessel_effects$vessel_effect[1],
    cv
  )

  if (make_plots) {

    dd <- sm %>%
      filter(!grepl("Intercept", par_group)) %>%
      mutate(par_group = gsub(", functionx as\\.character\\(x\\)\\[\\[1\\]\\]\\)", "", par_group))

    g3 <- ggplot(dd, aes_string("true_b", "est")) + geom_point() +
      ggplot2::geom_linerange(aes_string(ymin = "est - 2 * se", ymax = "est + 2 * se")) +
      facet_wrap(~par_group) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      coord_equal() + theme_pbs() +
      xlab("True value") + ylab("Estimated value")

    g4 <- plot_cpue_index_coefs(m)
  }

  # coverage!

  pos_dat <- fake_fleet[fake_fleet$pos_catch == 1, , drop = FALSE]
  bin_dat <- fake_fleet

  formula_binomial <- pos_catch ~ year_factor + f(vessel)
  formula_gamma <- spp_catch / hours_fished ~ year_factor + f(vessel)

  mm1 <- model.matrix(formula_binomial, data = bin_dat)
  mm2 <- model.matrix(formula_gamma, data = pos_dat)

  mm_pred1 <- make_pred_mm(mm1, years = unique(fake_fleet$year_factor))
  mm_pred2 <- make_pred_mm(mm2, years = unique(fake_fleet$year_factor))

  b_true_bin <- filter(sm, grepl("Bin", par_name)) %>% pull(true_b)
  b_true_pos <- filter(sm, grepl("Pos", par_name)) %>% pull(true_b)

  true_bin <- mm_pred1 %*% b_true_bin %>% as.numeric()
  true_pos <- mm_pred2 %*% b_true_pos %>% as.numeric()

  true_index_log <- log(plogis(true_bin) * exp(true_pos))

  sm2 <- summary(m$sdreport)
  pars <- row.names(sm2)
  row.names(sm2) <- seq_len(nrow(sm2))
  sm2 <- as.data.frame(sm2)
  sm2$pars <- pars
  est <- sm2 %>% filter(grepl("log_pred", pars))
  est$true <- true_index_log
  est$year <- seq_len(nrow(est))

  est$lwr <- est$Estimate + stats::qnorm(0.025) * est$`Std. Error`
  est$upr <- est$Estimate + stats::qnorm(0.975) * est$`Std. Error`

  if (make_plots) {
    g5 <- ggplot(est, aes_string("year", "exp(Estimate)", ymin = "exp(lwr)",
      ymax = "exp(upr)")) +
      geom_line(col = "grey60") +
      ggplot2::geom_pointrange(pch = 21) +
      geom_point(aes_string(y = "exp(true)"), colour = "red", pch = 4, size = 3) +
      theme_pbs() +
      xlab("Year") + ylab("CPUE") +
      coord_cartesian(expand = FALSE)
  }
  est <- mutate(est, covered = true < upr & true > lwr)

  out <- list(covered = est$covered, model_summary = sm)
  if (make_plots)
    list(ts = g1, cross_val = g3, coef = g4, ts_check = g5)
  else
    invisible(out)
}

context("Commercial CPUE index standardization models work.")

# test_that("index standardization estimates parameters correctly", {

library(dplyr)
library(ggplot2)

sim_cpue_index <- function(make_plots = TRUE, sigma = 0.35, n_samples = 10,
  n_years = 20, n_vessels = 10) {
  fake_fleet <- sim_cpue(
    sigma = sigma, n_samples = n_samples,
    n_years = n_years, n_vessels = n_vessels
  )

  year_effects <- select(fake_fleet, year_effect) %>% unique()
  vessel_effects <- select(fake_fleet, vessel_effect) %>% unique()

  m <- fit_cpue_index(fake_fleet,
    formula_binomial = pos_catch ~
      year_factor + f(vessel, function(x) as.character(x)[[1]]),
    formula_lognormal = log(spp_catch / hours_fished) ~
      year_factor + f(vessel, function(x) as.character(x)[[1]])
  )

  if (make_plots) {
    g <- predict_cpue_index(m) %>%
      plot_cpue_index()
    print(g)

    g <- plot_cpue_index_jk(m, terms = "f(vessel)")
    print(g)
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
    log(sigma)
  )

  if (make_plots) {
    g <- sm %>%
      filter(!grepl("Intercept", par_group)) %>%
      ggplot(aes(true_b, est)) + geom_point() +
      geom_linerange(aes(ymin = est - 2 * se, ymax = est + 2 * se)) +
      facet_wrap(~ par_group) +
      geom_abline(intercept = 0, slope = 1) +
      coord_equal()
    print(g)

    print(plot_cpue_index_coefs(m))
  }

  # coverage!

  pos_dat <- fake_fleet[fake_fleet$pos_catch == 1, , drop = FALSE]
  bin_dat <- fake_fleet

  formula_binomial <- pos_catch ~ year_factor + f(vessel)
  formula_lognormal <- log(spp_catch / hours_fished) ~ year_factor + f(vessel)

  mm1 <- model.matrix(formula_binomial, data = bin_dat)
  mm2 <- model.matrix(formula_lognormal, data = pos_dat)

  mm_pred1 <- gfplot:::make_pred_mm(mm1, years = unique(fake_fleet$year_factor))
  mm_pred2 <- gfplot:::make_pred_mm(mm2, years = unique(fake_fleet$year_factor))

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

  est$lwr <- est$Estimate + qnorm(0.025) * est$`Std. Error`
  est$upr <- est$Estimate + qnorm(0.975) * est$`Std. Error`

  if (make_plots) {
    g <- ggplot(est, aes(year, Estimate, ymin = lwr, ymax = lwr)) +
      geom_pointrange() +
      geom_point(aes(y = true), colour = "red")
    print(g)
  }
  est <- mutate(est, covered = true < upr & true > lwr)

  invisible(list(covered = est$covered, model_summary = sm))
}

test_that("cpue index standardization has correct coverage", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  set.seed(1)
  out <- lapply(seq_len(25), function(x)
    sim_cpue_index(
      make_plots = FALSE, sigma = 0.3, n_samples = 20,
      n_years = 10, n_vessels = 3
    )$covered)
  out <- unlist(out)
  expect_equal(0.95, mean(out), tolerance = 0.01)
})

test_that("cpue index standardization finds same values", {
  skip_on_cran()

  set.seed(42)
  model_summary <- sim_cpue_index(
    make_plots = TRUE, # to test these at the same time
    n_samples = 15,
    n_vessels = 10
  )$model_summary

  expect_identical(class(model_summary), "data.frame")
})

context("Commercial CPUE index standardization models work.")

# test_that("index standardization estimates parameters correctly", {

library(dplyr)
library(ggplot2)

test_that("cpue index standardization has correct coverage", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  set.seed(1)
  out <- lapply(seq_len(50), function(x)
    sim_cpue_index(
      make_plots = FALSE, cv = 0.3, n_samples = 20,
      n_years = 10, n_vessels = 3
    )$covered)
  out <- unlist(out)
  expect_equal(0.95, mean(out), tolerance = 0.01)
})

test_that("cpue index standardization finds same values", {
  skip_on_cran()

  set.seed(42)

  ignore <- sim_cpue_index(
    make_plots = TRUE, # to test these at the same time
    n_samples = 5,
    n_vessels = 5
  )

  model_summary <- sim_cpue_index(
    make_plots = FALSE,
    n_samples = 5,
    n_vessels = 5
  )$model_summary

  expect_identical(class(model_summary), "data.frame")
})

test_that("cpue index standardization matches stats::glm etc.", {
  set.seed(1)
  df <- data.frame(
    spp_catch = rlnorm(200),
    hours_fished = rlnorm(200),
    year_factor = as.factor(rep(2001:2010, 20)),
    pos_catch = sample(c(0, 1), size = 200, replace = TRUE),
    group1 = sample(letters[1:5], size = 200, replace = TRUE),
    group2 = sample(letters[1:5], size = 200, replace = TRUE)
  )

  mglm  <- stats::glm((spp_catch / hours_fished) ~ year_factor,
    data = filter(df, pos_catch == 1), family = Gamma(link = "log"))
  pglm <- predict(mglm,
    newdata = data.frame(year_factor = as.factor(2001:2010)),
    type = "response")

  mglm2  <- stats::glm(pos_catch ~ year_factor,
    data = df, family = binomial(link = "logit"))
  pglm2 <- predict(mglm2,
    newdata = data.frame(year_factor = as.factor(2001:2010)),
    type = "response")

  mgfplot <- fit_cpue_index(df,
    formula_binomial = pos_catch ~ year_factor,
    formula_gamma = (spp_catch / hours_fished) ~ year_factor)
  pgfplot <- predict_cpue_index(mgfplot) %>%
    filter(model == "Gamma") %>%
    pull(est)
  pgfplot2 <- predict_cpue_index(mgfplot) %>%
    filter(model == "Binomial") %>%
    pull(est)
  expect_equal(cor(pglm, pgfplot), 1.0, tolerance = 0.0000001)
  expect_equal(cor(pglm2, pgfplot2), 1.0, tolerance = 0.0000001)

  # 1 RE:
  mglmm <- lme4::glmer((spp_catch / hours_fished) ~ year_factor + (1 | group1),
    data = filter(df, pos_catch == 1), family = Gamma(link = "log"))
  pglmm <- exp(predict(mglmm,
    newdata = data.frame(year_factor = as.factor(2001:2010)),
    re.form = NA))
  mglmm2  <- lme4::glmer(pos_catch ~ year_factor + (1 | group1),
    data = df, family = binomial(link = "logit"))
  pglmm2 <- plogis(predict(mglmm2,
    newdata = data.frame(year_factor = as.factor(2001:2010)),
    re.form = NA))

  mgfplot <- fit_cpue_index(df,
    formula_binomial = pos_catch ~ year_factor + (1 | group1),
    formula_gamma = (spp_catch / hours_fished) ~ year_factor + (1 | group1))
  pgfplot <- predict_cpue_index(mgfplot) %>%
    filter(model == "Gamma") %>%
    pull(est)
  pgfplot2 <- predict_cpue_index(mgfplot) %>%
    filter(model == "Binomial") %>%
    pull(est)
  expect_equal(cor(pglmm, pgfplot), 1.0, tolerance = 0.0000001)
  expect_equal(cor(pglmm2, pgfplot2), 1.0, tolerance = 0.0000001)

  # 2 RE:
  mglmm <- glmmTMB::glmmTMB((spp_catch / hours_fished) ~ year_factor +
      (1 | group1) + (1 | group2),
    data = filter(df, pos_catch == 1), family = Gamma(link = "log"))
  pglmm <- exp(predict(mglmm,
    newdata = data.frame(year_factor = as.factor(2001:2010), group1 = NA, group2 = NA)))
  mglmm2  <- lme4::glmer(pos_catch ~ year_factor + (1 | group1) + (0 | group2),
    data = df, family = binomial(link = "logit"))
  pglmm2 <- plogis(predict(mglmm2,
    newdata = data.frame(year_factor = as.factor(2001:2010)),
    re.form = NA))

  mgfplot <- fit_cpue_index(df,
    formula_binomial = pos_catch ~ year_factor + (1 | group1) + (1 | group2),
    formula_gamma = (spp_catch / hours_fished) ~ year_factor + (1 | group1) + (1 | group2))
  pgfplot <- predict_cpue_index(mgfplot) %>%
    filter(model == "Gamma") %>%
    pull(est)
  pgfplot2 <- predict_cpue_index(mgfplot) %>%
    filter(model == "Binomial") %>%
    pull(est)
  expect_equal(cor(pglmm, pgfplot), 1.0, tolerance = 0.0000001)
  expect_equal(cor(pglmm2, pgfplot2), 1.0, tolerance = 0.0000001)
  plot(pglmm);points(pgfplot, col = "red")
  plot(pglmm2);points(pgfplot2, col = "red")
})

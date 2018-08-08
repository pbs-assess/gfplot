context("Commercial CPUE index standardization models work.")

# test_that("index standardization estimates parameters correctly", {

library(dplyr)
library(ggplot2)

# test_that("cpue index standardization has correct coverage", {
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#
#   set.seed(1)
#   out <- lapply(seq_len(50), function(x)
#     sim_cpue_index(
#       make_plots = FALSE, cv = 0.3, n_samples = 20,
#       n_years = 10, n_vessels = 3
#     )$covered)
#   out <- unlist(out)
#   expect_equal(0.95, mean(out), tolerance = 0.01)
# })
#
# test_that("cpue index standardization finds same values", {
#   skip_on_cran()
#
#   set.seed(42)
#
#   ignore <- sim_cpue_index(
#     make_plots = TRUE, # to test these at the same time
#     n_samples = 5,
#     n_vessels = 5
#   )
#
#   model_summary <- sim_cpue_index(
#     make_plots = FALSE,
#     n_samples = 5,
#     n_vessels = 5
#   )$model_summary
#
#   expect_identical(class(model_summary), "data.frame")
# })
#
# test_that("cpue index standardization matches stats::glm etc.", {
#   set.seed(1)
#   df <- data.frame(
#     cpue = rlnorm(200),
#     year_factor = as.factor(rep(2001:2010, 20)),
#     pos_catch = sample(c(0, 1), size = 200, replace = TRUE),
#     group1 = sample(letters[1:5], size = 200, replace = TRUE),
#     group2 = sample(letters[1:5], size = 200, replace = TRUE)
#   )
#
#   mglm  <- stats::glm(cpue ~ year_factor,
#     data = filter(df, pos_catch == 1), family = Gamma(link = "log"))
#   pglm <- predict(mglm,
#     newdata = data.frame(year_factor = as.factor(2001:2010)),
#     type = "response")
#
#   mglm2  <- stats::glm(pos_catch ~ year_factor,
#     data = df, family = binomial(link = "logit"))
#   pglm2 <- predict(mglm2,
#     newdata = data.frame(year_factor = as.factor(2001:2010)),
#     type = "response")
#
#   mgfplot <- fit_cpue_index(df,
#     formula_binomial = pos_catch ~ year_factor,
#     formula_gamma = cpue ~ year_factor)
#   pgfplot <- predict_cpue_index(mgfplot) %>%
#     filter(model == "Gamma") %>%
#     pull(est)
#   pgfplot2 <- predict_cpue_index(mgfplot) %>%
#     filter(model == "Binomial") %>%
#     pull(est)
#   expect_equal(cor(pglm, pgfplot), 1.0, tolerance = 0.0000001)
#   expect_equal(cor(pglm2, pgfplot2), 1.0, tolerance = 0.0000001)
#
#   # 1 RE:
#   mglmm <- lme4::glmer(cpue ~ year_factor + (1 | group1),
#     data = filter(df, pos_catch == 1), family = Gamma(link = "log"))
#   pglmm <- exp(predict(mglmm,
#     newdata = data.frame(year_factor = as.factor(2001:2010)),
#     re.form = NA))
#   mglmm2  <- lme4::glmer(pos_catch ~ year_factor + (1 | group1),
#     data = df, family = binomial(link = "logit"))
#   pglmm2 <- plogis(predict(mglmm2,
#     newdata = data.frame(year_factor = as.factor(2001:2010)),
#     re.form = NA))
#
#   mgfplot <- fit_cpue_index(df,
#     formula_binomial = pos_catch ~ year_factor + (1 | group1),
#     formula_gamma = cpue ~ year_factor + (1 | group1))
#   pgfplot <- predict_cpue_index(mgfplot) %>%
#     filter(model == "Gamma") %>%
#     pull(est)
#   pgfplot2 <- predict_cpue_index(mgfplot) %>%
#     filter(model == "Binomial") %>%
#     pull(est)
#   expect_equal(cor(pglmm, pgfplot), 1.0, tolerance = 0.0000001)
#   expect_equal(cor(pglmm2, pgfplot2), 1.0, tolerance = 0.0000001)
#
#   # 2 RE:
#   mglmm <- glmmTMB::glmmTMB(cpue ~ year_factor +
#       (1 | group1) + (1 | group2),
#     data = filter(df, pos_catch == 1), family = Gamma(link = "log"))
#   pglmm <- exp(predict(mglmm,
#     newdata = data.frame(year_factor = as.factor(2001:2010), group1 = NA, group2 = NA)))
#   mglmm2  <- lme4::glmer(pos_catch ~ year_factor + (1 | group1) + (0 | group2),
#     data = df, family = binomial(link = "logit"))
#   pglmm2 <- plogis(predict(mglmm2,
#     newdata = data.frame(year_factor = as.factor(2001:2010)),
#     re.form = NA))
#
#   mgfplot <- fit_cpue_index(df,
#     formula_binomial = pos_catch ~ year_factor + (1 | group1) + (1 | group2),
#     formula_gamma = cpue ~ year_factor + (1 | group1) + (1 | group2))
#   pgfplot <- predict_cpue_index(mgfplot) %>%
#     filter(model == "Gamma") %>%
#     pull(est)
#   pgfplot2 <- predict_cpue_index(mgfplot) %>%
#     filter(model == "Binomial") %>%
#     pull(est)
#   expect_equal(cor(pglmm, pgfplot), 1.0, tolerance = 0.0000001)
#   expect_equal(cor(pglmm2, pgfplot2), 1.0, tolerance = 0.0000001)
#   plot(pglmm);points(pgfplot, col = "red")
#   plot(pglmm2);points(pgfplot2, col = "red")
# })
#
# test_that("cpue index standardization matches simulated data", {
#   set.seed(42)
#   df <- data.frame(
#     y = 1,
#     year_factor = as.factor(rep(1:10, each = 500))
#   )
#   betas <- c(1, seq(0, 1, length.out = 9))
#   betas_exp <- exp(c(betas[1], betas[2:10] + betas[1]))
#   mm <- model.matrix(y ~ as.factor(year_factor), data = df)
#   cv <- 0.1
#   shape <- 1 / (cv^2)
#   df$cpue <- rgamma(nrow(df), shape = shape, scale = exp(mm %*% betas) / shape)
#   plot(as.numeric(df$year_factor), df$cpue)
#   prob1 <- 0.6
#   df$pos_catch <- rbinom(nrow(df), size = 1, prob = prob1)
#   plot(as.numeric(df$year_factor), jitter(df$pos_catch, amount = 0.1))
#   df$cpue <- ifelse(df$pos_catch == 1, df$cpue, 0)
#   plot(jitter(as.numeric(df$year_factor), amount = 0.1), df$cpue)
#
#   m <- fit_cpue_index(df,
#     formula_binomial = pos_catch ~ year_factor,
#     formula_gamma = cpue ~ year_factor)
#   pred_gamma <- predict_cpue_index(m) %>%
#     filter(model == "Gamma") %>%
#     pull(est)
#   pred_binomial <- predict_cpue_index(m) %>%
#     filter(model == "Binomial") %>%
#     pull(est)
#   pred_combined <- predict_cpue_index(m) %>%
#     filter(model == "Combined") %>%
#     pull(est)
#   expect_equal(pred_gamma, betas_exp, tolerance = 0.05)
#   expect_equal(pred_binomial, rep(prob1, 10), tolerance = 0.05)
#   expect_equal(pred_combined, betas_exp * prob1, tolerance = 0.05)
#
#   plot(pred_gamma)
#   points(betas_exp, col = "red")
#
#   plot(pred_binomial, ylim = c(0, 1))
#   points(rep(prob1, 10), col = "red")
#
#   plot(pred_combined)
#   points(betas_exp*prob1, col = "red")
#
#   plot(pred_combined)
#   points(betas_exp*pred_binomial, col = "red")
#
#   ## TMB tweedie:
#   # m1 <- glmmTMB::glmmTMB(cpue ~ year_factor, family = glmmTMB::tweedie(link = "log"), data = df)
#   # names(summary(m1))
#   # plot(betas_exp*prob1)
#   # tmb_coefs <- summary(m1)$coefficients$cond[,"Estimate"]
#   # tmb_coefs <- exp(c(tmb_coefs[1], tmb_coefs[2:10] + tmb_coefs[1]))
#   # points(tmb_coefs, col = "blue")
# })

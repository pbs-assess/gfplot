context("Commercial CPUE index standardization models work.")

# test_that("index standardization estimates parameters correctly", {

library(dplyr)
library(ggplot2)

test_that("cpue index standardization works", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  sim_cpue_index <- function(make_plots = TRUE, sigma = 0.35, n_samples = 10) {
    fake_fleet <- expand.grid(
      year_factor = as.factor(seq(1996, 2018)),
      vessel = rep(as.factor(as.character(seq(100, 110))), each = n_samples)
    )

    vessel_effects <- data.frame(
      vessel = unique(fake_fleet$vessel),
      vessel_effect = rnorm(length(unique(fake_fleet$vessel)), 0, 1)
    )

    year_effects <- data.frame(
      year_factor = unique(fake_fleet$year),
      year_effect = rnorm(length(unique(fake_fleet$year)), 0, 1)
    )

    fake_fleet <- inner_join(fake_fleet, vessel_effects, by = "vessel")
    fake_fleet <- inner_join(fake_fleet, year_effects, by = "year_factor")



    fake_fleet <- mutate(fake_fleet,
      pos_catch = rbinom(nrow(fake_fleet),
        size = 1,
        prob = plogis(vessel_effect + year_effect)
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

    m <- fit_cpue_index(fake_fleet,
      formula_binomial = pos_catch ~ year_factor + f(vessel),
      formula_lognormal = log(spp_catch / hours_fished) ~ year_factor + f(vessel)
    )

    if (make_plots) {
      g <- predict_cpue_index(m) %>%
        plot_cpue_index()
      print(g)

      g <- plot_cpue_index_jk(m)
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

    est$lwr <- est$Estimate + qnorm(0.025) * est$`Std. Error`
    est$upr <- est$Estimate + qnorm(0.975) * est$`Std. Error`

    if (make_plots) {
      g <- ggplot(est, aes(year, Estimate, ymin = lwr, ymax = lwr)) +
        geom_pointrange() +
        geom_point(aes(y = true), colour = "red")
      print(g)
    }
    est <- mutate(est, covered = true < upr & true > lwr)

    invisible(est$covered)
  }

  # set.seed(42)
  # out <- lapply(seq_len(50), function(x)
  #   sim_cpue_index(make_plots = FALSE, sigma = 0.35, n_samples = 5))
  # out <- unlist(out)
  # mean(out)

  set.seed(1)
  sim_cpue_index(make_plots = TRUE, n_samples = 15)
})

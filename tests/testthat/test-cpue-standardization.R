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

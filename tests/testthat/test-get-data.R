context("Get data functions")
test_that("get_* data functions work at PBS", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  if (!is_dfo_windows()) skip("Not a DFO computer")
  d <- get_age_precision("lingcod")
  expect_equal(d$species_code[[1L]], "467")

  d <- get_catch("lingcod")
  expect_gte(nrow(d), 1L)

  d <- get_surv_tows("lingcod", 1)
  expect_gte(d$catch_weight[[1]], 1L)

  # d <- get_surv_samples("lingcod", 16)
  # expect_gte(d$survey_id[[1]], 1L)


})

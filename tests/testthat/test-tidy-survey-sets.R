context("Get data functions")
test_that("get_* data functions work at PBS", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  if (!is_dfo_windows()) skip("Not a DFO computer")
  d <- get_survey_sets("lingcod", 3)
  d <- tidy_survey_sets("lingcod", 3, 2012:2017)
  expect_type(d$year, "integer")

  d <- load_bath()
  expect_type(d$X, "numerical")

})

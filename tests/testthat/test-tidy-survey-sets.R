# context("Get data functions")
# test_that("get_* data functions work at PBS", {
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#
#   tryCatch(get_ssids(), error = function(e) skip("No database access"))
#
#   d <- get_survey_sets("lingcod", 3)
#   expect_type(d$year, "integer")
#
#   dd <- tidy_survey_sets(d, 3, 2012:2017)
#   expect_type(dd$year, "integer")
#
#   d <- load_bath()
#   expect_type(d$X, "double")
# })

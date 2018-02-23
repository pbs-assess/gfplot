context("Get data functions")
test_that("get_* data functions work at PBS", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  if (!is_dfo_windows()) skip("Not a DFO computer")
  # d <- get_age_precision("lingcod")
  # expect_equal(d$species_code[[1L]], "467")
  #
  # d <- get_catch("lingcod")
  # expect_gte(nrow(d), 1L)
  #
  # d <- get_survey_tows("lingcod", 1)
  # expect_gte(d$catch_weight[[1]], 0)
  #
  # d <- get_survey_samples("lingcod", 16)
  # expect_gte(d$survey_id[[1]], 1L)

  # d <- get_comm_samples("lingcod")
  # expect_gte(d$year[[1]], 1900L)

  # d <- get_cpue_spatial("lingcod")
  # expect_false(is.null(d$lat[[1]]))
  # expect_false(is.null(d$lon[[1]]))

  # d <- get_cpue_spatial_ll("lingcod")
  # expect_type(d$fishing_event_id, "integer")

  # d <- get_cpue_index("bottom trawl", min_year = 2015)
  # expect_true(!is.null(d$fe_start_date))

  # Why doesn't this work?
  # d <- get_cpue_index("bottom trawl", min_year = 2015)
  # expect_type(d$best_date, "datetime")

  d <- get_surv_index("lingcod", ssid = 1)
  expect_type(d$num_pos_sets, "integer")

})

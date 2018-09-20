library(dplyr)

context("Get data functions")
test_that("get_* data functions work at PBS", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  if (!is_dfo() || !is_windows()) skip("Not a DFO computer")
  d <- get_age_precision("lingcod")
  expect_equal(d$species_code[[1L]], "467")

  d <- get_catch("lingcod")
  expect_gte(nrow(d), 1L)

  d <- get_survey_sets("lingcod", 1)
  expect_gte(d$catch_weight[[1]], 0)

  d <- get_survey_samples("lingcod", 16)
  expect_type(d$survey_id, "integer")

  d <- get_commercial_samples("lingcod")
  expect_gte(d$year[[1]], 1900L)

  d <- get_cpue_spatial("lingcod")
  expect_false(is.null(d$lat[[1]]))
  expect_false(is.null(d$lon[[1]]))

  d <- get_cpue_spatial_ll("lingcod")
  expect_type(d$fishing_event_id, "integer")

  d <- get_cpue_index("bottom trawl", min_cpue_year = 2015)
  expect_true(!is.null(d$fe_start_date))

  # Why doesn't this work? Error says that best_date is of
  # type "double", but it is infact "datetime" in function output
  # d <- get_cpue_index("bottom trawl", min_year = 2015)
  # expect_type(d$best_date, "datetime")

  d <- get_survey_index("lingcod", ssid = 1)
  expect_type(d$num_pos_sets, "integer")

  d <- get_ssids()
  expect_type(d$SURVEY_SERIES_DESC, "character")

  d <- get_age_methods()
  expect_true(!is.null(d$ageing_method_desc))

  d <- get_sample_trips()
  expect_true(!is.null(d$SAMPLE_ID[[1]]))

  d <- get_strata_areas()
  expect_true(!is.null(d$SURVEY_ID[[1]]))

  d <- get_survey_ids(16)
  expect_true(!is.null(d$SURVEY_ID[[1]]))

  expect_error(get_survey_sets("lingcod", ssid = 99999))

  d <- get_iphc_sets("lingcod")
  expect_type(d$species, "character")

  d <- get_iphc_sets_info()
  expect_type(d$year, "integer")

  d <- get_survey_sets(c("yelloweye rockfish", "lingcod"),
    join_sample_ids = TRUE
  )
  dups <- d %>%
    filter(!is.na(sample_id)) %>%
    group_by(sample_id) %>%
    summarize(n = length(unique(species_common_name))) %>%
    filter(n > 1)
  expect_equal(nrow(dups), 0L)
})

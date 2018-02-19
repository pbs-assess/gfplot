
test_that("get_* data functions work at PBS", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  if (is_dfo_windows()) {
    get_catch("lingcod")
  } else {
    expect_equal(1L, 1L)
  }
})

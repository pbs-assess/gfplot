context("coord_sf_auto()")

testthat::skip_if_not_installed("sf")

sf_geo <- sf::st_sf(
  id = 1,
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-129.1, 52.9), c(-128.9, 52.9), c(-128.9, 53.1), c(-129.1, 53.1), c(-129.1, 52.9)
    ))),
    crs = 4326
  )
)

test_that("preserves sf_obj's own CRS after buffering a geographic object", {
  result <- coord_sf_auto(sf_geo, buffer = 1000)
  expect_true(result$crs == sf::st_crs(sf_geo))
})

test_that("simplify actually runs when buffer = 0", {
  expect_error(coord_sf_auto(sf_geo, buffer = 0, simplify = "not-a-number"))
})

test_that("simplify tolerance is applied in metres, not degrees", {
  unsimplified <- coord_sf_auto(sf_geo, buffer = 0, simplify = NULL)
  result <- coord_sf_auto(sf_geo, buffer = 0, simplify = 500)

  expect_true(all(is.finite(result$limits$x)))
  expect_true(all(is.finite(result$limits$y)))
  expect_equal(diff(result$limits$x), diff(unsimplified$limits$x), tolerance = 0.1)
  expect_equal(diff(result$limits$y), diff(unsimplified$limits$y), tolerance = 0.1)
})

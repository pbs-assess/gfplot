context("Utility functions work")

test_that("all_species_codes() works", {
  expect_false(all_species_codes("a"))
  expect_false(all_species_codes(c("a", "b")))
  expect_true(all_species_codes(1))
  expect_true(all_species_codes(c(1, 1)))
  expect_true(all_species_codes(c("1", "2")))
  expect_false(all_species_codes(c("a", 1)))
  expect_false(all_species_codes(c("a", "1")))
  expect_false(all_species_codes(c("a", "a1")))
})

test_that("force_three_letter_species_code() works", {
  expect_equal(force_three_letter_species_code(3), "003")
  expect_equal(force_three_letter_species_code(c(1, 2)), c("001", "002"))
  expect_equal(force_three_letter_species_code(03), "003")
  expect_equal(force_three_letter_species_code("4004"), "4004")
  expect_equal(force_three_letter_species_code("003"), "003")
  expect_equal(force_three_letter_species_code("abc"), "abc")
})

test_that("common2codes() works", {
  expect_equal(common2codes(3), "003")
  expect_equal(common2codes(c(1, 2)), c("001", "002"))
  expect_equal(common2codes(c("001", "002")), c("001", "002"))
})

context("Utility functions work as expected")

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

test_that("collapse_filters() works", {
  expect_equal(collapse_filters(c(1, 2)), "'1','2'")
})

test_that("inject_filter() works", {
  x <- inject_filter("a",
    species = "b",
    sql_code = list("y", "\n-- insert here", "z"),
    search_flag = "-- insert here",
    conversion_func = I
  )

  expect_equal(x[[1]], "y")
  expect_equal(x[[2]], "a ('b')")
  expect_equal(x[[3]], "z")
})

test_that("firstup() works", {
  expect_equal(firstup("abc"), "Abc")
})

test_that("round_down_even() works", {
  expect_equal(round_down_even(3), 2)
  expect_equal(round_down_even(4), 4)
})

test_that("mround() works", {
  expect_equal(mround(14, 5), 15)
  expect_equal(mround(14, 10), 10)
  expect_equal(mround(15, 5), 15)
})

test_that("round_nice() works", {
  expect_equal(round_nice(0), "")
  expect_equal(round_nice(1), "1")
  expect_equal(round_nice(42), "42")
  expect_equal(round_nice(100), "100")
  expect_equal(round_nice(101), "100")
  expect_equal(round_nice(1001), "1000")
  expect_equal(round_nice(1502), "1500")
})

test_that("read_sql works", {
  x <- read_sql("get-catch.sql")
  expect_gte(length(x), 1)
  expect_identical(class(x), "character")
})

context("Ageing precision plots work")

g <- plot_age_precision(dplyr::tibble(
  prec_age = 2.0, prim_age = 1.9,
  prec_min_age = 1.5, prec_max_age = 2.5,
  prim_min_age = 1.5, prim_max_age = 2.9))
expect_identical(class(g), c("gg", "ggplot"))


plot_cpue_predictions <- function(dat,
                                  model_version = "Combined",
                                  scale = FALSE,
                                  french = FALSE) {
  if (scale) {
    dat <- dat |>
      group_by(.data$formula_version, .data$model, .data$area) |>
      mutate(geo_mean = exp(mean(log(.data$est)))) |>
      mutate(upr = .data$upr / .data$geo_mean,
             lwr = .data$lwr / .data$geo_mean,
             est = .data$est / .data$geo_mean) |>
      ungroup()
  }

  unstandardized <- dat |>
    filter(.data$formula_version == "Unstandardized") |>
    rename(est_unstandardized = .data$est,
           lwr_unstandardized = .data$lwr, upr_unstandardized = .data$upr) |>
    select(.data$year, .data$area, .data$model,
           .data$est_unstandardized, .data$lwr_unstandardized,
           .data$upr_unstandardized)

  temp <- dat |>
    filter(.data$formula_version != "Unstandardized") |>
    left_join(unstandardized, by = c("area", "year", "model")) |>
    filter(model == model_version) |>
    mutate(formula_version = gsub("\\+ ", " ", .data$formula_version)) |>
    mutate(formula_version =
             gsub("Full standardization",
                  ifelse(french,
                         "Toutes les variables",
                         "All variables"),
                  .data$formula_version)
    ) |>
    mutate(formula_version =
             forcats::fct_relevel(.data$formula_version,
                                  ifelse(french,
                                         "Toutes les variables",
                                         "All variables"),
                                  after = Inf)
    )

  if(!scale){
    y_label <- ifelse(french,
                      "CPUE (kg/heure)",
                      "CPUE (kg/hour)")
  }else{
    y_label <- ifelse(french,
                      "CPUE (kg/h) moyenne géométrique divisée",
                      "CPUE (kg/hour) divided\nby geometric mean")
  }

  temp |>
    ggplot(aes_string("year", "est", ymin = "upr", ymax = "lwr")) +
    geom_line(aes_string(y = "est_unstandardized"),
              colour = "grey30",
              lty = 1) +
    ggplot2::geom_ribbon(aes_string(ymin = "lwr_unstandardized",
                                    ymax = "upr_unstandardized"),
                         fill = "#00000030") +
    ggplot2::geom_ribbon(alpha = 0.4, fill = "red") +
    geom_line(colour = "red") +
    theme_pbs() +
    facet_grid(area ~ formula_version, scales = "free_y") +
    labs(y = y_label, x = "") +
    ylim(0, NA) +
    guides(colour = "none", fill = "none")
}


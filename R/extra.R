plot_cpue_predictions <- function(dat, model_version = "Combined", scale = FALSE) {
  if (scale) {
    dat <- dat %>%
      group_by(formula_version, model, area) %>%
      mutate(geo_mean = exp(mean(log(est)))) %>%
      mutate(upr = upr / geo_mean, lwr = lwr / geo_mean, est = est / geo_mean) %>%
      ungroup()
  }

  unstandardized <- dat %>%
    filter(formula_version == "Unstandardized") %>%
    rename(est_unstandardized = est, lwr_unstandardized = lwr, upr_unstandardized = upr) %>%
    select(year, area, model, est_unstandardized, lwr_unstandardized, upr_unstandardized)

  temp <- dat %>%
    filter(formula_version != "Unstandardized") %>%
    left_join(unstandardized, by = c("area", "year", "model")) %>%
    filter(model == model_version) %>%
    mutate(formula_version = gsub("\\+ ", " ", formula_version)) %>%
    mutate(
      formula_version =
        gsub("Full standardization", "All variables", formula_version)
    ) %>%
    mutate(
      formula_version =
        forcats::fct_relevel(formula_version, "All variables", after = Inf)
    )
  temp %>%
    ggplot(aes_string("year", "est", ymin = "upr", ymax = "lwr")) +
    geom_line(aes_string(y = "est_unstandardized"), colour = "grey30", lty = 1) +
    ggplot2::geom_ribbon(
      aes_string(ymin = "lwr_unstandardized", ymax = "upr_unstandardized"),
      fill = "#00000030"
    ) +
    ggplot2::geom_ribbon(alpha = 0.4, fill = "red") +
    geom_line(colour = "red") +
    theme_pbs() +
    facet_grid(area ~ formula_version, scales = "free_y") +
    labs(
      y = if (!scale) "CPUE (kg/hour)" else "CPUE (kg/hour) divided\nby geometric mean",
      x = ""
    ) +
    ylim(0, NA) +
    guides(colour = FALSE, fill = FALSE)
}

plot_predictor_bubbles <- function(dat, variable, reorder_group = FALSE,
                              group_id = "fishing_event_id", ncol = 2) {
  temp_pos <- dat %>%
    filter(spp_catch > 0) %>%
    group_by(area, year, !!rlang::sym(variable)) %>%
    summarise(n = length(unique(!!rlang::sym(group_id)))) %>%
    group_by(area, !!rlang::sym(variable)) %>%
    mutate(n_tot = sum(n)) %>%
    ungroup()

  temp_all <- dat %>%
    group_by(area, year, !!rlang::sym(variable)) %>%
    summarise(n = length(unique(!!rlang::sym(group_id)))) %>%
    group_by(area, !!rlang::sym(variable)) %>%
    mutate(n_tot = sum(n)) %>%
    ungroup()

  if (reorder_group) {
    # temp[, variable] <- paste(temp$area,
    #   as.character(temp[, variable,drop=TRUE]))
    # temp$y_group <- forcats::fct_reorder2(temp[, variable, drop=TRUE],
    #   temp$area, -temp$n_tot)
    temp_pos$y_group <- temp_pos[, variable, drop = TRUE]
    temp_all$y_group <- temp_all[, variable, drop = TRUE]
  } else {
    temp_pos$y_group <- temp_pos[, variable, drop = TRUE]
    temp_all$y_group <- temp_all[, variable, drop = TRUE]
  }

  temp_pos %>%
    ggplot(aes_string("as.factor(year)", y = "y_group")) +
    geom_point(aes_string(size = "n", fill = "n"), alpha = 0.4, pch = 21) +
    geom_point(data = temp_all, aes_string(size = "n"), alpha = 0.4, pch = 21) +
    facet_wrap(~area, scales = "free", ncol = ncol) +
    scale_x_discrete(breaks = seq(1950, 2020, 10)) +
    xlab("") + ylab(firstup(gsub("_", " ", variable))) +
    labs(size = paste0("Number of\n", group_id)) +
    labs(fill = paste0("Number of\n", group_id)) +
    ggplot2::scale_size_continuous(range = c(0, 7)) +
    ggplot2::scale_fill_viridis_c(trans = "log", breaks = c(1, 10, 100, 500))
}

plot_predictor_boxplots <- function(dat, y, ylab = "", title = "") {
  ggplot(dat, aes_string("as.factor(year)", y = y)) +
    geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.5) +
    ggplot2::scale_y_log10(
      breaks = c(0.1, 1, 10, 100, 1000, 10000),
      labels = scales::comma
    ) +
    ggplot2::scale_x_discrete(breaks = seq(1950, 2030, 5)) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
    facet_wrap(~area) + xlab("") + ylab(ylab) +
    ggplot2::ggtitle(title)
}

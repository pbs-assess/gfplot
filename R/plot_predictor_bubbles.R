#' Make predictor level bubble plots
#'
#' @param dat A data frame with the columns area, year
#' @param variable The column to summarize (character value)
#' @param group_id The events to count over (e.g. `"fishing_event_id"` or `"trip_id"`)
#' @param ncol Number of facetted columns (facets by area)
#' @param french If `TRUE`, all text will appear as French in the plot
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' d <- data.frame(year = rep(1991:2010, each = 20),
#'   spp_catch = rbinom(20 * 20, size = 1, prob = 0.8),
#'   fishing_event_id = rep(1:20, 20),
#'   my_predictor = sample(gl(20, 20), size = 400),
#'   area = rep("a", 400))
#' plot_predictor_bubbles(d, "my_predictor")
plot_predictor_bubbles <- function(dat, variable,
  group_id = "fishing_event_id", ncol = 2, french = FALSE) {
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

  leg_title <- ifelse(french,
                      paste0("Nombre de\n", group_id),
                      paste0("Number of\n", group_id))

  p <- temp_pos %>%
    ggplot(aes_string("as.factor(year)", y = variable)) +
    geom_point(aes_string(size = "n", fill = "n"), alpha = 0.4, pch = 21) +
    geom_point(data = temp_all, aes_string(size = "n"), alpha = 0.4, pch = 21) +
    facet_wrap(~area, scales = "free", ncol = ncol) +
    ggplot2::scale_x_discrete(breaks = seq(1950, 2020, 5)) +
    xlab("") + ylab(firstup(gsub("_", " ", en2fr(variable, translate = french)))) +
    labs(size = leg_title) +
    labs(fill = leg_title) +
    ggplot2::scale_size_continuous(range = c(0, 7), breaks = c(1, 10, 100, 500, 1000)) +
    ggplot2::scale_fill_viridis_c(trans = "log", breaks = c(1, 10, 100, 500, 1000)) +
    guides(fill = guide_legend(reverse=T), size = guide_legend(reverse=T)) +
    theme_pbs()

  if(length(levels(temp_all[[variable]]))> 13 & length(levels(temp_all[[variable]])) < 40){
    p <- p + ggplot2::scale_y_discrete(breaks = levels(temp_all[[variable]])[c(T, rep(F, 1))])
  }

  if(length(levels(temp_all[[variable]]))> 41){
    p <- p + ggplot2::scale_y_discrete(breaks = levels(temp_all[[variable]])[c(T, rep(F, 4))])
  }

  p
}

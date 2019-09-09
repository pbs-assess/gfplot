#' Plot catch data over time
#'
#' Functions for plotting catch data over time as a stacked bar plot.
#'
#' @details
#' * `tidy_catch()` Prepares PBS data for `plot_catch()`. This works across one
#' or multiple species.
#' * `plot_catch()` Plots catch. The input data frame must come from
#' `tidy_catch()` or have the columns (in any order): `year`, `gear`, `value`
#' (containing catches or landings).
#'
#' @param dat Input data frame. For `tidy_catch()` should be from
#' [get_catch()]. For `plot_catch()` should be from `tidy_catch()` or be
#' formatted similarly. See details.
#' @param areas Fisheries and Oceans major statistical areas (eg.
#'  3C, 3D, 5A, 5B, etc.). See gfplot::pbs_areas for a lookup table
#'  with descriptions. Enter as a regular expression. For example, `c("5A", "5B")` or
#'  `c("5[CDE]+", "5[AB]+")`.
#' @param french Logical for French or English.
#' @param ylab Y axis label.
#' @param xlim X axis limits in years.
#' @param units A named character vector with names referring to text that will
#'   be pasted into the y-axis label and a value defining the quantity to divide
#'   the `value` column by for that unit label.
#'   Defaults to `c('1000 t' = 1000000, 't' = 1000, 'kg' = 1)` if
#'   `units = NULL`.
#' @param unreliable An optional numeric vector defining years before which the
#'   data are less reliable. Leave as `NA` to omit.
#' @param blank_plot If `TRUE` then a blank plot with appropriate axes will be
#'   returned. Used in the synopsis report.
#'
#' @examples
#' \dontrun{
#' d <- get_catch("lingcod")
#' tidy_catch(d, areas = c("5[CDE]+", "5[AB]+", "3[CD]+")) %>%
#'   plot_catch()
#'
#' tidy_catch(d, areas = c("5A", "5B")) %>%
#'   plot_catch()
#' }
#' @name plot_catch
NULL

#' @rdname plot_catch
#' @export
tidy_catch <- function(dat, areas = NULL) {
  if (!is.null(areas)) {
    dat$area <- assign_areas(dat$major_stat_area_name, areas)
    dat <- dat[!is.na(dat$area), , drop = FALSE]
  } else {
    dat$area <- "Coastwide"
  }

  dat <- filter(dat, !is.na(species_common_name), !is.na(year)) %>%
    group_by(year, species_common_name, gear, area) %>%
    summarise(
      landed_kg = sum(landed_kg, na.rm = TRUE),
      discarded_kg = sum(discarded_kg, na.rm = TRUE),
      landed_pcs = sum(landed_pcs, na.rm = TRUE),
      discarded_pcs = sum(discarded_pcs, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(species_common_name, year)

# Note that discarded_kg only includes trawl discards from 1996+ and
# trap/hook and line discards from 2006+
  catches <- mutate(dat,
    gear = dplyr::recode(gear,
      UNKNOWN = "Unknown/trawl",
      `BOTTOM TRAWL` = "Bottom trawl",
      `HOOK AND LINE` = "Hook and line",
      `MIDWATER TRAWL` = "Midwater trawl",
      `TRAP` = "Trap",
      `UNKNOWN TRAWL` = "Unknown/trawl"
    )
  ) %>%
    select(year, area, species_common_name, gear, landed_kg, discarded_kg)

  cm <- reshape2::melt(catches,
    id.vars = c("year", "species_common_name", "area", "gear")
  )

  landings <- filter(cm, variable %in% c("landed_kg"))
  discards <- filter(cm, variable %in% c("discarded_kg"))

  landings$gear <- as.character(landings$gear)
  discards$gear <- as.character(discards$gear)
  discards$gear <- "Discarded"

  all_catch <- bind_rows(landings, discards)
  all_catch <- mutate(all_catch,
    gear = forcats::fct_relevel(
      gear,
      "Bottom trawl",
      "Midwater trawl",
      "Hook and line",
      "Trap",
      "Unknown/trawl",
      "Discarded"
    )
  )

  all_catch <- group_by(all_catch, year, species_common_name, area, gear) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()

  all_catch
}

#' @rdname plot_catch
#' @export
plot_catch <-
  function(
             dat,
             french = translate,
             ylab = en2fr("Catch", french), xlim = c(1954, 2017),
             units = NULL,
             unreliable = c(1996, 2006),
             blank_plot = FALSE) {
    if (is.null(units)) {
      units <- c(`1000 t` = 1000000,
        `t` = 1000, `kg` = 1)
    }

    gears <- c(
      "Bottom trawl",
      "Midwater trawl",
      "Hook and line",
      "Trap",
      "Unknown/trawl",
      "Discarded"
    )

    if (french) {
      gears <- rosettafish::en2fr(gears)
      dat$gear <- as.character(dat$gear)
      dat$gear <- rosettafish::en2fr(dat$gear)
    }

    pal <- c(RColorBrewer::brewer.pal(
      n = length(gears) - 2,
      "Paired"
    ), "grey60", "grey30")[c(2, 1, 4, 3, 5, 6)]
    names(pal) <- gears
    dat$gear <- factor(dat$gear, levels = gears)

    scale_val <- units[[1]]
    ylab_gg <- paste0(ylab, " (", names(units)[1], ")")

    for (i in seq_along(units)) {
      if (max(dat$value) < (1000 * units[[i]])) {
        scale_val <- units[[i]]
        ylab_gg <- paste0(ylab, " (", names(units)[i], ")")
      }
    }

    if (!blank_plot) {
      dat <- left_join(
        expand.grid(year = seq(min(xlim), max(xlim)),
          area = levels(as.factor(dat$area)), gear = levels(dat$gear)), dat,
        by = c("year", "area", "gear"))
      dat$value[is.na(dat$value)] <- 0
    }

    yrs <- xlim
    g <- ggplot(data = dat)

    g <- g + geom_vline(xintercept = seq(yrs[1], yrs[2]), col = "grey98") +
      geom_vline(
        xintercept = seq(mround(yrs[1], 5), yrs[2], 5),
        col = "grey95"
      )

    stacked_data <- group_by(dat, area, year) %>%
      summarise(catch = sum(value / scale_val, na.rm = FALSE))

    if (!is.na(unreliable[[1]])) {
      for (i in seq_along(unreliable))
        g <- g + ggplot2::geom_rect(
          data = data.frame(x = NA), # fake
          xmin = xlim[1] - 1, xmax = unreliable[[i]], ymin = 0,
          ymax = max(stacked_data$catch, na.rm = TRUE) * 1.07,
          fill = "#00000010", inherit.aes = FALSE
        )
    }

    if (!blank_plot) {
      g <- g + geom_col(
        data = dat,
        aes_string("year", "value/scale_val", colour = "gear", fill = "gear")
      ) +
        ylim(0, max(stacked_data$catch, na.rm = TRUE) * 1.05)
    }

    if (blank_plot) {
      g <- g + ylim(0, 1)
    }

    g <- g +
      theme_pbs() +
      scale_fill_manual(values = pal, drop = FALSE, breaks = gears,
        labels = gears) +
      scale_colour_manual(values = pal, drop = FALSE, breaks = gears,
        labels = gears) +
      coord_cartesian(xlim = xlim + c(-0.5, 0.5), expand = FALSE) +
      xlab("") + ylab(ylab_gg) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(fill = "", colour = "") +
      labs(title = en2fr("Commercial catch", french)) +
      # ggplot2::theme(legend.justification = c(0, 1), legend.position = c(0, 1)) +
      ggplot2::theme(legend.background = ggplot2::element_rect(fill = "#FFFFFF99"))

    if (french) {
      g <- g + ggplot2::theme(legend.text =
          ggplot2::element_text(size = ggplot2::rel(0.6), colour = "grey20"))
    }

    if (!all(dat$area == "Coastwide") && !all(dat$area == rosettafish::en2fr("Coastwide"))) {
      g <- g + facet_wrap(~ area, ncol = 1)
    }

    g
  }

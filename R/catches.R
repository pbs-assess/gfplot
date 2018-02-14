#' Tidy PBS catch data for \code{\link{plot_catch}}
#'
#' @param dat TODO
#'
#' @return A data frame formatted for \code{\link{plot_catch}}
#' @export
#' @family tidy data functions
#'
#' @examples
#' \dontrun{
#' d <- get_catch("lingcod")
#' tidy_catch(d)
#' }

tidy_catch <- function(dat) {

  catches <- dplyr::mutate(dat,
    gear = dplyr::recode(gear,
      UNKNOWN = "Unknown/trawl",
      `BOTTOM TRAWL` = "Bottom trawl",
      `HOOK AND LINE` = "Hook and line",
      `MIDWATER TRAWL` = "Midwater trawl",
      `TRAP` = "Trap",
      `UNKNOWN TRAWL` = "Unknown/trawl")) %>%
    select(year, species_common_name, gear, landed_kg, discarded_kg)

  cm <- reshape2::melt(catches,
    id.vars = c("year", "species_common_name", "gear"))

  landings <- filter(cm, variable %in% c("landed_kg"))
  discards <- filter(cm, variable %in% c("discarded_kg"))

  landings$gear <- as.character(landings$gear)
  discards$gear <- as.character(discards$gear)
  discards$gear <- "Discarded"

  all_catch <- bind_rows(landings, discards)
  all_catch <- mutate(all_catch,
    gear = forcats::fct_relevel(gear,
      "Bottom trawl",
      "Midwater trawl",
      "Hook and line",
      "Trap",
      "Unknown/trawl",
      "Discarded"))

  all_catch <- group_by(all_catch, year, species_common_name, gear) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()

  all_catch
}

#' Plot catches
#'
#' @param dat A properly formatted data frame. For example, from
#'   \code{\link{tidy_catch}}. The input data frame must have the columns
#'   (in any order): \code{year}, \code{gear}, \code{value} (containing catches
#'   or landings).
#' @param ylab Text to label y axis
#' @param units A named character vector with names referring to text that will
#'   be pasted into the y-axis label and a value defining the quantity to divide
#'   the \code{value} column by for that unit label.
#' @param unreliable An optional numeric vector defining years before which the
#'   data are less reliable. Leave as \code{NA} to omit.
#' @param unreliable_alpha The alpha level for the optional unreliable
#'   rectangles.
#'
#' @export
#' @family plotting functions
#'
#' @examples
#' \dontrun{
#' d <- get_catch("lingcod")
#' d <- tidy_catch(d)
#' plot_catch(d)
#' }
plot_catch <- function(dat,
  ylab = "Landings", units = c("1000 tons" = 1000000, "tons" = 1000, "kg" = 1),
  unreliable = c(1996, 2006), unreliable_alpha = 0.2) {

  pal <-  c(RColorBrewer::brewer.pal(n = length(unique(dat$gear)) - 2,
    "Paired"), "grey60", "grey30")[c(2, 1, 4, 3, 5, 6)]
  names(pal) <- levels(dat$gear)

  scale_val <- units[[1]]
  ylab_gg <- paste0(ylab, " (", names(units)[1], ")")

  for (i in seq_along(units)) {
    if (max(dat$value) < (1000 * units[[i]])) {
      scale_val <- units[[i]]
      ylab_gg <-  paste0(ylab, " (", names(units)[i], ")")
    }
  }

  g <- ggplot()

  if (!is.na(unreliable[[1]])) {
    for (i in seq_along(unreliable))
      g <- g + ggplot2::annotate("rect",
        xmin = min(dat$year) - 1, xmax = unreliable[[i]], ymin = 0,
        ymax = max(dat$value / scale_val) * 1.5, alpha = unreliable_alpha,
        fill = "black")
  }

  g <- g + geom_col(data = dat,
    aes_string("year", "value/scale_val", colour = "gear", fill = "gear")) +
    theme_pbs() +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    ylim(0, NA) +
    coord_cartesian(xlim = range(dat$year) + c(-0.5, 0.5), expand = FALSE) +
    xlab("") + ylab(ylab_gg) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::labs(fill = "", colour = "") +
    labs(title = "Commercial catch")

  g
}

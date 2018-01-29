firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

round_nice <- function(x) {
  out <- plyr::round_any(x, 100)
  out[out == 0] <- x[out == 0]
  out
}

#' Prepare PBS samples data for \code{\link{plot_samples}}
#'
#' @export

prep_pbs_samples <- function() {

}


#' Plot sample availability
#'
#' @param dat A properly formatted data frame. For example, from
#'   \code{\link{prep_pbs_samples}}. The input data frame must have the columns
#'   (in any order): \code{year}, \code{type} (with the types of samples to
#'   plot, e.g. maturity, weight), and \code{n} (with the numbers of samples).
#'   The axis labels will be derived from \code{type} after capitalizing the
#'   first letter.
#'
#' @examples
#' d <- expand.grid(year = 1996:2016,
#'   type = c("maturity", "weight", "length", "age"), stringsAsFactors = FALSE)
#' d$n <- rpois(nrow(d), 100)
#' plot_samples(d)
#'
#' @export

plot_samples <- function(dat) {

  d$n_plot <- log(d$n + 1)
  d$n_text <- round_nice(d$n)
  d$type <- paste("#", firstup(d$type))

  ggplot(d, aes_string("year", "type")) +
    ggplot2::geom_tile(aes_string(fill = "n_plot"), colour = "grey90", width = 1.5) +
    theme_pbs() +
    coord_cartesian(expand = FALSE, xlim = range(d$year) + c(-0.75, 0.25)) +
    ggplot2::scale_fill_continuous(limits = c(0, max(d$n_plot)), low = "white", high = "grey10") +
    ggplot2::scale_x_continuous(breaks = seq(min(d$year), max(d$year), 2)) +
    theme(axis.text.x = element_text(hjust = .7),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()) +
    ggplot2::guides(fill = FALSE) + xlab("") + ylab("") +
    geom_text(aes_string(x = "year - 0.25", label = "n_text"), colour = "white")
}

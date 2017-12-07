#' Add a text label to a plot
#'
#' @param xfrac The fraction over from the left side.
#' @param yfrac The fraction down from the top.
#' @param label The text to label with.
#' @param pos Position to pass to text()
#' @param col Colour of text
#' @param ... Anything extra to pass to text(), e.g. cex, col.

add_label <- function(xfrac, yfrac, label, pos = 4, col = "grey30", ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, col = col, ...)
}

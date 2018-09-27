#' Quantile residuals for a Tweedie GLMM fit with glmmTMB
#'
#' @param obj A model object from [glmmTMB::glmmTMB()].
#' @param response The name of the response column in the data. E.g. `"cpue"`.
#'
#' @export
#' @importFrom stats pgamma qnorm
#'
#' @examples
#' m <- glmmTMB::glmmTMB(cpue ~ 1,
#'   data = data.frame(cpue =
#'       tweedie::rtweedie(1000, mu = 2, phi = 5, power = 1.5)),
#'   family = glmmTMB::tweedie(link = "log"))
#' qr <- qres_tweedie(m, "cpue")
#' plot_qres_qq(qr)
#' plot_qres_histogram(qr)
#' @rdname qres_tweedie
qres_tweedie <- function (obj, response) {
  mu <- stats::fitted(obj)
  y <- obj$frame[[response]]
  w <- 1 # weights
  p <- stats::plogis(obj$fit$par[["thetaf"]]) + 1
  dispersion <- exp(obj$fit$par[["betad"]])
  u <- tweedie::ptweedie(q = y, power = p, mu = mu,
    phi = dispersion/w)
  if (p > 1 && p < 2)
    u[y == 0] <- runif(sum(y == 0), min = 0, max = u[y == 0])
  u <- stats::qnorm(u)
  u <- u[is.finite(u)]
  u
}

#' @rdname qres_tweedie
#' @export
qres_binomial <- function (obj, response) {
  p <- stats::fitted(obj)
  y <- obj$frame[[response]]
  n <- rep(1, length(y))
  a <- stats::pbinom(y - 1, n, p)
  b <- stats::pbinom(y, n, p)
  u <- stats::runif(n = length(y), min = a, max = b)
  qnorm(u)
}

#' @rdname qres_tweedie
#' @export
qres_gamma <- function (obj, response) {
  mu <- stats::fitted(obj)
  y <- obj$frame[[response]]
  df <- stats::df.residual(obj)
  w <- 1
  # dispersion <- sum(w * ((y - mu)/mu)^2)/df
  dispersion <- 1/exp(obj$fit$par[["betad"]])
  logp <- stats::pgamma((w * y)/mu/dispersion, w/dispersion, log.p = TRUE)
  stats::qnorm(logp, log.p = TRUE)
}

#' @rdname qres_tweedie
#' @export
qres_gaussian <- function (obj, response) {
  mu <- stats::fitted(obj)
  y <- obj$frame[[response]]
  dispersion <- exp(obj$fit$par[["betad"]])
  logp <- stats::pnorm(y, mu, dispersion, log.p = TRUE)
  stats::qnorm(logp, log.p = TRUE)
}

#' @param qr A vector of quantile residuals from [qres_tweedie()].
#' @param nmax The maximum number of residuals to plot. Very large values will
#'   take a while to plot.
#' @rdname qres_tweedie
#' @export
plot_qres_qq <- function(qr, nmax = 1e4) {
  if (nmax < length(qr)) {
    qr <- qr[sample.int(nmax)]
  }
  ggplot(data.frame(u = qr), aes_string(sample = "u")) +
    ggplot2::stat_qq() + ggplot2::stat_qq_line() +
    gfplot::theme_pbs() +
    ggplot2::labs(x = "Theoretical", y = "Sample")
}

#' @param bins The number of histogram bins.
#' @rdname qres_tweedie
#' @export
plot_qres_histogram <- function(qr, bins = 50, nmax = 1e4) {
  if (nmax < length(qr)) {
    qr <- qr[sample.int(nmax)]
  }
  xx <- seq(min(qr), max(qr), length.out = 300)
  dn <- stats::dnorm(xx, sd = stats::sd(qr))
  dn <- data.frame(x = xx, y = dn)
  ggplot(data.frame(u = qr), aes_string("u")) +
    ggplot2::geom_histogram(aes_string(y = "..density.."),
      bins = bins, fill = NA, col = "grey50") +
    gfplot::theme_pbs() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(x = "Randomized quantile residual", y = "Density") +
    ggplot2::geom_line(data = dn, aes_string("x", "y"), lty = 2)
}

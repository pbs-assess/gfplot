% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ageing-precision.R
\name{plot_age_precision}
\alias{plot_age_precision}
\title{Plot ageing precision data}
\usage{
plot_age_precision(dat, n = 250, jitter = 0.25, seed = 42)
}
\arguments{
\item{dat}{A data frame from \code{\link[=tidy_age_precision]{tidy_age_precision()}}.}

\item{n}{Number of fish to randomly sample to plot.}

\item{jitter}{Amount to randomly jitter ages for visualization. Same jitter
values are used for the precision and primary ages for the same fish.}

\item{seed}{If a numeric value, set the random seed so that the same rows
are sampled each time and the same jitter values are generated. If
\code{NULL}, different fish will be sampled each time the function is run.}
}
\description{
Plot ageing precision data
}
\examples{
\dontrun{
gfdata::get_ageing_precision("shortraker rockfish") \%>\%
  tidy_ageing_precision() \%>\%
  plot_ageing_precision()

gfdata::get_ageing_precision("pacific ocean perch") \%>\%
  tidy_ageing_precision() \%>\%
  plot_ageing_precision(n = 200)
}
}

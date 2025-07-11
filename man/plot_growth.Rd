% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth.R
\name{plot_growth}
\alias{plot_growth}
\alias{plot_vb}
\alias{plot_length_weight}
\title{Plot von Bertalanffy or length-weight fits}
\usage{
plot_growth(
  object_female,
  object_male,
  object_all,
  type = c("vb", "length-weight"),
  downsample = 2000L,
  pt_alpha = 0.2,
  xlab = "Age (years)",
  ylab = "Length (cm)",
  seed = 42,
  lab_x = 0.45,
  lab_y = 0.3,
  lab_x_gap = 0.3,
  lab_y_gap = 0.06,
  col = c(Female = "black", Male = "grey40"),
  french = FALSE,
  jitter = FALSE
)

plot_vb(..., type = "vb")

plot_length_weight(
  ...,
  type = "length-weight",
  xlab = "Length (cm)",
  ylab = "Weight (kg)",
  lab_x = 0.1,
  lab_y = 0.9,
  lab_x_gap = 0.35,
  french = FALSE
)
}
\arguments{
\item{object_female}{Output from \code{\link[=fit_length_weight]{fit_length_weight()}} or \code{\link[=fit_vb]{fit_vb()}}.}

\item{object_male}{Output from \code{\link[=fit_length_weight]{fit_length_weight()}} or \code{\link[=fit_vb]{fit_vb()}}.}

\item{object_all}{Output from \code{\link[=fit_length_weight]{fit_length_weight()}} or \code{\link[=fit_vb]{fit_vb()}}.}

\item{type}{von Bertalanffy or length-weight fits?}

\item{downsample}{Downsample the individual fish to plot down to this number.}

\item{pt_alpha}{Transparency for the points.}

\item{xlab}{Label for the x axis.}

\item{ylab}{Label for the y axis.}

\item{seed}{A random seed value that only comes into play for downsampling.}

\item{lab_x}{Fraction from left to place text labels.}

\item{lab_y}{Fraction from bottom to place text labels.}

\item{lab_x_gap}{Horizontal gap between text labels.}

\item{lab_y_gap}{The vertical gap between text labels.}

\item{col}{A named character vector declaring the colors for female and male}

\item{french}{Logical.}

\item{jitter}{Logical, whether to jitter data points.}

\item{...}{Arguments to pass to \code{\link[=plot_growth]{plot_growth()}}.}

\item{pts_col}{Logical for whether data points should be coloured to match lines.}
}
\description{
Plot von Bertalanffy or length-weight fits
}
\details{
You can include \code{object_female} and/or \code{object_male} or \code{object_all}
depending on whether the model was fit to female, male, or both sexes
combined.
}
\examples{
\dontrun{
# d <- gfdata::get_survey_samples("pacific ocean perch")
d <- pop_samples
model_f <- fit_length_weight(d, sex = "female")
model_m <- fit_length_weight(d, sex = "male")
plot_length_weight(object_female = model_f, object_male = model_m)

model_f <- fit_vb(d, sex = "female")
model_m <- fit_vb(d, sex = "male")
plot_vb(object_female = model_f, object_male = model_m)
}
}

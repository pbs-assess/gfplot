% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maturity.R
\name{fit_mat_ogive}
\alias{fit_mat_ogive}
\alias{plot_mat_ogive}
\alias{plot_mat_annual_ogives}
\title{Fit and plot maturity ogives}
\usage{
fit_mat_ogive(
  dat,
  type = c("age", "length"),
  sample_id_re = FALSE,
  year_re = FALSE,
  months = seq(1, 12),
  custom_maturity_at = NULL,
  ageing_method_codes = NULL,
  usability_codes = c(0, 1, 2, 6),
  link = c("logit", "probit", "cloglog", "cauchit", "log")
)

plot_mat_ogive(
  object,
  col = c(M = "grey50", F = "black"),
  xlab = if (object$type[[1]] == "age") "Age (years)" else "Length (cm)",
  title = if (object$type[[1]] == "age") "Age at maturity" else "Length at maturity",
  rug = TRUE,
  rug_n = 1500,
  x_max = 1.75,
  prediction_type = c("all", "male", "female", "none"),
  text_label_size = 3,
  show_quant_text = TRUE,
  french = FALSE
)

plot_mat_annual_ogives(
  object,
  xlab = if (object$type[[1]] == "age") "Age (years)" else "Length (cm)",
  title = if (object$type[[1]] == "age") "Age at maturity" else "Length at maturity",
  rug = TRUE,
  rug_n = 1500,
  x_max = 1.75,
  prediction_type = c("all", "male", "female", "none"),
  french = FALSE
)
}
\arguments{
\item{dat}{Data from \code{\link[gfdata:get_data]{gfdata::get_survey_samples()}}.}

\item{type}{Should this be an age or length fit?}

\item{sample_id_re}{If \code{TRUE} then the model will include random intercepts
for sample ID.}

\item{year_re}{If \code{TRUE} the model will include random intercepts
for year.}

\item{months}{A numeric vector indicating which months to include when
fitting the maturity ogive. Defaults to including all months, but not NAs.
Changing to NULL will include all data including NAs.}

\item{custom_maturity_at}{A numeric vector of two threshold codes to define
maturity at with the first being for males and the second for females.
Defaults to \code{NULL}, which brings in default values from maturity assignment
dataframe included with this package.
\code{NA} in either position will also retain the default.}

\item{ageing_method_codes}{A numeric vector of ageing method codes to filter
on. Defaults to \code{NULL}, which brings in all valid ageing codes.
See \code{\link[gfdata:lookup]{gfdata::get_age_methods()}}.}

\item{usability_codes}{An optional vector of usability codes.
All usability codes not in this vector will be omitted.
Set to \code{NULL} to include all samples.}

\item{link}{The link function for the binomial GLM (by default, \code{"logit"}).}

\item{object}{Output from \code{\link[=fit_mat_ogive]{fit_mat_ogive()}}.}

\item{col}{A named character vector declaring the colors for F and M.}

\item{xlab}{X axis label.}

\item{title}{Title for the plot.}

\item{rug}{Logical indicating whether rug lines should be added.}

\item{rug_n}{The number of rug lines to sample from the total number of fish.}

\item{x_max}{Used in determining the right axis limit.}

\item{prediction_type}{The prediction lines to show. Useful if you only want
to show model fits when you have sufficient data.}

\item{text_label_size}{Font size for the labels showing the age-at- values
for either age or length on the plot panel}

\item{show_quant_text}{Logical. If \code{TRUE}, show the quantile values for each
sex on the panel}

\item{french}{Translate to French?}
}
\description{
Fit and plot maturity ogives
}
\examples{
# d <- gfdata::get_survey_samples("pacific ocean perch", ssid = 1)
d <- pop_samples

m <- fit_mat_ogive(d, type = "age", sample_id_re = FALSE)
plot_mat_ogive(m)

m <- fit_mat_ogive(d, type = "length", sample_id_re = FALSE)
plot_mat_ogive(m)
\dontrun{
## with random intercepts for sample ID:
m <- fit_mat_ogive(d, type = "length", sample_id_re = TRUE)
plot_mat_ogive(m)
}
}

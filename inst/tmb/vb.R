library(dplyr)
library(TMB)

## Compile and load the model
compile("vb.cpp")
dyn.load(dynlib("vb"))

## Data and parameters
dat <- pop_samples
usability_codes = c(0, 1, 2, 6)
dat <- filter(dat, usability_code %in% usability_codes)
dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
dat <- filter(dat, !is.na(sex), !is.na(length), !is.na(age))
dat <- switch("male",
  "female" = filter(dat, sex == 2L),
  "male" = filter(dat, sex == 1L),
  "all" = dat
)
data <- list(len=dat$length, age=dat$age)
parameters <- list(k = 0.2, linf = 40, log_sigma=log(0.1), t0=-1)

## Make a function object
obj <- MakeADFun(data, parameters, DLL="vb")

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
sdr

library(gfplot)
model_f <- fit_vb(pop_samples, sex = "male")
model_f$pars
model_f <- fit_vb(pop_samples, sex = "male", uniform_priors = TRUE)
model_f$pars

library(tmbstan)
m <- tmbstan::tmbstan(obj = obj, iter = 500, chains = 4, cores = 4,
  control = list(adapt_delta = 0.99))
m

y <- sapply(1:100, function(x) {
  model_f <- fit_vb(pop_samples, sex = "male", uniform_priors = TRUE)
  # model_f$pars$t0
  model_f$model$value
})
table(round(y, 1))

y <- sapply(1:100, function(x) {
  model_f <- fit_vb(pop_samples, sex = "male")
  # model_f$pars$t0
  model_f$model$value
})
table(round(y, 2))

y <- sapply(1:100, function(x) {
  opt <- nlminb(obj$par, obj$fn, obj$gr)
  opt$objective
})
table(round(y, 2))

y <- fit_vb(pop_samples, sex = "male", method = "tmb")
y$pars
y <- fit_vb(pop_samples, sex = "female", method = "tmb")
y$pars

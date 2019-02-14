library(dplyr)
library(TMB)

## Compile and load the model
compile("inst/tmb/vb.cpp")
dyn.load(dynlib("inst/tmb/vb"))

## Data and parameters
dat <- pop_samples
dat <- readRDS("../gfsynopsis/report/data-cache/shortspine-thornyhead.rds")
dat <- dat$survey_samples
usability_codes = c(0, 1, 2, 6)
dat <- filter(dat, usability_code %in% usability_codes)
dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE]
dat <- filter(dat, !is.na(sex), !is.na(length), !is.na(age))
dat <- switch("female",
  "female" = filter(dat, sex == 2L),
  "male" = filter(dat, sex == 1L),
  "all" = dat
)
data <- list(len=dat$length, age=dat$age)
parameters <- list(k = rlnorm(1, meanlog = log(0.1)), linf = rlnorm(1, meanlog = log(50)), log_sigma=rlnorm(1), t0=-5)

plot(dat$age, dat$length)

# dat <- filter(dat, age < 90)
## Make a function object
obj <- MakeADFun(data, parameters, DLL="vb")

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt$par
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

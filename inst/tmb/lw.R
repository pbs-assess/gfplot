library(dplyr)
library(TMB)

## Compile and load the model
compile("lw.cpp")
dyn.load(dynlib("lw"))

a <- -1
b <- 2
x <- rnorm(5000)
sigma <- 0.2
y <- metRology::rt.scaled(length(x), a + b * x, df = 3, sd = sigma)
plot(x, y)

data <- list(len = x, weight = y, df = 3)
parameters <- list(log_a = 0, b = 0, log_sigma = 0)

## Make a function object
obj <- MakeADFun(data, parameters, DLL="lw")

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)

summary(lm(y ~ x))

opt$par
exp(opt$par[3])

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
plot(dat$length, dat$weight)

data <- list(len=log(dat$length), weight=log(dat$weight), df = 3)
parameters <- list(log_a = 0, b = 0, log_sigma = 0)

## Make a function object
obj <- MakeADFun(data, parameters, DLL="lw", silent = F)

## Call function minimizer
opt <- nlminb(obj$par, obj$fn, obj$gr)

## Get parameter uncertainties and convergence diagnostics
sdr <- sdreport(obj)
sdr

opt$par
exp(opt$par[3])

summary(MASS::rlm(log(weight) ~ log(length), data = dat))

m <- gfplot::fit_length_weight(pop_samples, sex = "female", method = "tmb")
m$pars

m <- gfplot::fit_length_weight(pop_samples, sex = "female", method = "rlm")
m$pars

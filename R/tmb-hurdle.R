set.seed(1)
x <- runif(100, -2, 2)
lp <- -0.9 + x * 0.5
y_bin <- rbinom(length(x), size = 1, prob = plogis(lp))
y <- y_bin * exp(rnorm(length(x), lp, 0.2))
non_zero <- ifelse(y > 0, 1, 0)
d <- data.frame(x, y, non_zero)
m1 <- glm(non_zero ~ x, data = d, family = binomial(link = logit))
m2 <- glm(y ~ x, data = subset(d, non_zero == 1), family = Gamma(link = log))
library(ggplot2)
p <- ggplot(d, aes(x, y, colour = as.factor(non_zero))) + geom_point()
print(p)
pos_dat <- subset(d, non_zero == 1)

bin_coef <- plogis(predict(m1, newdata = data.frame(x = 0)))
gamma_coef <- exp(predict(m2, newdata = data.frame(x = 0)))
bin_coef * gamma_coef

library(TMB)
compile("deltalognormal.cpp")
dyn.load(dynlib("deltalognormal"))

mm1 <- as.matrix(model.matrix(non_zero ~ x, data = d))
mm2 <- as.matrix(model.matrix(y ~ x, data = pos_dat))

mm_pred <- matrix(nrow = 1, ncol = 2, data = c(1, 0))

obj <- MakeADFun(
  data = list(
    X1_ij = mm1, y1_i = d$non_zero,
    X2_ij = mm2, y2_i = log(pos_dat$y),
    X1_pred_ij = mm_pred, X2_pred_ij = mm_pred),
  parameters = list(b1_j = rep(0, ncol(mm1)), b2_j = rep(0, ncol(mm2)),
    log_sigma = log(0.2)),
  DLL = "deltalognormal")
obj$fn(obj$par)
obj$gr(obj$par)
opt <- nlminb(
  start = obj$par,
  objective = obj$fn,
  gradient = obj$gr)
opt

plogis(opt$par[[1]])
opt$par[[2]]
exp(opt$par[[3]])

sdreport(obj)
obj$report()
r <- sdreport(obj)
summary(r)
exp(summary(r)["log_prediction","Estimate"])
exp(summary(r)["log_prediction","Estimate"] + 2 * summary(r)["log_prediction","Std. Error"])
exp(summary(r)["log_prediction","Estimate"] - 2 * summary(r)["log_prediction","Std. Error"])

hurdle_fn <- function(data, i) {
  dat_boot <- data[i, ]
  m1 <- glm(non_zero ~ x, data = dat_boot,
    family = binomial(link = logit))
  m2 <- glm(y ~ x, data = subset(dat_boot, non_zero == 1),
    family = Gamma(link = log))
  bin_coef <- plogis(predict(m1, newdata = data.frame(x = 0)))
  gamma_coef <- exp(predict(m2, newdata = data.frame(x = 0)))
  bin_coef * gamma_coef
}

library(boot)
b <- boot(d, hurdle_fn, R = 1000)
b.ci <- boot.ci(b, type = "bca")
b
print(b.ci)

# ------- real

f1 <- pos_catch ~ year_factor + vessel_name
f2 <- log(spp_catch) ~ year_factor + vessel_name
mm1 <- model.matrix(f1, data = d_retained)
mm2 <- model.matrix(f2, data = subset(d_retained, pos_catch == 1))

get_most_freq_factor <- function(x) {
  names(rev(sort(table(x))))[1]
}
newdata <- expand.grid(
  year_factor = sort(unique(d_retained$year_factor)),
  month_factor = get_most_freq_factor(d_retained$month_factor),
  depth_band = get_most_freq_factor(d_retained$depth_band),
  latitude_band = get_most_freq_factor(d_retained$latitude_band),
  dfo_locality = get_most_freq_factor(d_retained$locality_code),
  vessel_name = get_most_freq_factor(d_retained$vessel_name),
  hours_fished = mean(d_retained$hours_fished),
  pos_catch = NA,
  spp_catch = NA,
  best_depth = mean(d_retained$best_depth))
mm_pred <- mm2[1:length(unique(d_retained$year)), ]
for (i in 1:ncol(mm_pred)) {
  for (j in 1:nrow(mm_pred)) {
    mm_pred[j, i] <- 0
  }}
mm_pred[,1] <- 1
for (i in 1:ncol(mm_pred)) {
  for (j in 1:nrow(mm_pred)) {
    if (i == j)
    mm_pred[j, i] <- 1
  }}

obj <- MakeADFun(
  data = list(
    X1_ij = mm1, y1_i = d_retained$pos_catch,
    X2_ij = mm2, y2_i = log(subset(d_retained, pos_catch == 1)$spp_catch),
    X1_pred_ij = mm_pred, X2_pred_ij = mm_pred),
  parameters = list(b1_j = rep(0, ncol(mm1)), b2_j = rep(0, ncol(mm2)),
    log_sigma = log(0.2)),
  DLL = "deltalognormal")
obj$fn(obj$par)
obj$gr(obj$par)
system.time({
opt <- nlminb(
  start = obj$par,
  objective = obj$fn,
  gradient = obj$gr, control = list(iter.max = 500,
    eval.max = 500, trace = 100))
opt
})

# system.time({
#   opt2 <- optim(
#     par = obj$par,
#     fn = obj$fn,
#     gr = obj$gr, method = "BFGS", control = list(maxit = 500))
# })
# opt <- opt2
# plogis(opt$par[[1]])
# opt$par[[2]]
# exp(opt$par[[3]])

# sdreport(obj)
obj$report()
r <- sdreport(obj)
# summary(r)
sm <- summary(r)
ii <- grep("log_prediction", row.names(sm))
plot(exp(sm[ii, ][,1]), type = "l")
lines(exp(sm[ii, ][,1] + 2 * sm[ii, ][,2]), type = "l")
lines(exp(sm[ii, ][,1] - 2 * sm[ii, ][,2]), type = "l")

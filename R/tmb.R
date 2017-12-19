library(TMB)
compile("logistic2.cpp")
dyn.load(dynlib("logistic2"))
# x <- runif(10000, -1, 1)
# y_i <- rbinom(10000, 1, 0.4)
# obj <- MakeADFun(
#   data = list(X_ij = cbind(rep(1, length(y_i)), x), y_i = y_i),
#   parameters = list(b_j = c(0, 0)),
#   DLL = "logistic2")
# # obj$fn(obj$par)
# # obj$gr(obj$par)
# # opt <- do.call("optim", obj)
# opt <- nlminb(
#     start = obj$par,
#     objective = obj$fn,
#     gradient = obj$gr)
# opt

f <- pos_catch ~ as.factor(year) + as.factor(month) + dfo_locality +
  depth_band
mm <- model.matrix(f, data = dbin)

# d2$logit_pos_catch <- binomial()$linkfun(d2$pos_catch)
# m_check <- glm(f, data = dbin,
# family = binomial(link = "logit"))

obj <- MakeADFun(
  data = list(X_ij = mm, y_i = d2$pos_catch),
  parameters = list(b_j =
      c(binomial()$linkfun(mean(d2$pos_catch)), # intercept
        rep(0, ncol(mm)-1))),
  # parameters = list(b_j = coef(m_check)),
  DLL = "logistic2")
# obj$fn(obj$par)
# obj$gr(obj$par)
opt <- nlminb(
  start = obj$par,
  objective = obj$fn,
  gradient = obj$gr)

opt2 <- optim(
  par = obj$par,
  fn = obj$fn,
  gr = obj$gr)

opt

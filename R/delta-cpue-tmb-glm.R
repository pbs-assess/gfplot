mm1 <- model.matrix(f1, data = d_retained)
mm2 <- model.matrix(f2, data = subset(d_retained, pos_catch == 1))

make_pred_mm <- function(x) {
  mm_pred <- x[1:length(unique(d_retained$year)), ]
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
  mm_pred
}
mm_pred2 <- make_pred_mm(mm2)
mm_pred1 <- make_pred_mm(mm1)

# Get some close starting values so this document compiles quickly:
m_bin <- speedglm::speedglm(f1, data = d_retained, family = binomial(link = "logit"))
m_pos <- lm(f2, data = subset(d_retained, pos_catch == 1))

message("Fitting model...")
obj <- MakeADFun(
  data = list(
    X1_ij = mm1, y1_i = d_retained$pos_catch,
    X2_ij = mm2,
    y2_i = log(
      subset(d_retained, pos_catch == 1)$spp_catch/
        subset(d_retained, pos_catch == 1)$hours_fished),
    X1_pred_ij = mm_pred1, X2_pred_ij = mm_pred2),
  # parameters = list(b1_j = rep(0, ncol(mm1)), b2_j = rep(0, ncol(mm2)),
  parameters = list(
    b1_j = coef(m_bin) + rnorm(length(coef(m_bin)), 0, 0.00001),
    b2_j = coef(m_pos) + rnorm(length(coef(m_pos)), 0, 0.00001),
    log_sigma = log(summary(m_pos)$sigma)),
  DLL = "deltalognormal")
# obj$fn(obj$par)
# obj$gr(obj$par)

st - Sys.time()

system.time({
  opt <- nlminb(
    start = obj$par,
    objective = obj$fn,
    gradient = obj$gr, control = list(iter.max = 1000,
      eval.max = 1000))
  opt
})
max(obj$gr(opt$par))

message("Getting sdreport...")
# obj$report()
r <- sdreport(obj)

ft <- Sys.time()

that_took <- abs(round(as.numeric(st - ft, units = "secs"), 0))

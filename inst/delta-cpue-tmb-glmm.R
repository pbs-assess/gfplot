mm1 <- model.matrix(f1, data = d_retained)
mm2 <- model.matrix(f2, data = subset(d_retained, pos_catch == 1))

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

fct_to_tmb_num <- function(x) {
  as.numeric(as.factor(as.character(x))) - 1
}
bin_vess <- fct_to_tmb_num(d_retained$vessel_name)
pos_vess <- fct_to_tmb_num(subset(d_retained, pos_catch == 1)$vessel_name)
bin_loc <- fct_to_tmb_num(d_retained$dfo_locality)
pos_loc <- fct_to_tmb_num(subset(d_retained, pos_catch == 1)$dfo_locality)

# find good fixed-effect starting values from GLMs/LMs:
m_bin <- speedglm::speedglm(f1, data = d_retained,
  family = binomial(link = "logit"))
m_pos <- lm(f2, data = subset(d_retained, pos_catch == 1))

obj <- MakeADFun(
  data = list(
    X1_ij = mm1, y1_i = d_retained$pos_catch,
    X2_ij = mm2, y2_i = log(
      subset(d_retained, pos_catch == 1)$spp_catch/
        subset(d_retained, pos_catch == 1)$hours_fished),

    factor1k_i = bin_vess,
    factor2k_i = pos_vess,
    factor1g_i = bin_loc,
    factor2g_i = pos_loc,

    X1_pred_ij = mm_pred,
    X2_pred_ij = mm_pred,

    nk1 = length(unique(bin_vess)),
    nk2 = length(unique(pos_vess)),
    ng1 = length(unique(bin_loc)),
    ng2 = length(unique(pos_loc))
  ),
  parameters = list(
    b1_j = coef(m_bin) + rnorm(length(coef(m_bin)), 0, 0.01),
    b2_j = coef(m_pos) + rnorm(length(coef(m_bin)), 0, 0.01),

    z1_k = rep(0, length(unique(bin_vess))),
    z2_k = rep(0, length(unique(pos_vess))),
    z1_g = rep(0, length(unique(bin_loc))),
    z2_g = rep(0, length(unique(pos_loc))),

    log_sigma = log(1.5),
    log_sigma_zk1 = log(0.25),
    log_sigma_zk2 = log(0.25),
    log_sigma_zg1 = log(1),
    log_sigma_zg2 = log(1)
  ),
  random = c("z1_k", "z2_k", "z1_g", "z2_g"),
  DLL = "deltalognormal_2re")

obj$fn(obj$par)
obj$gr(obj$par)
system.time({
  opt <- nlminb(
    start = obj$par,
    objective = obj$fn,
    gradient = obj$gr, control = list(iter.max = 1000,
      eval.max = 1000))
  opt
})

obj$report()
r_re <- sdreport(obj)

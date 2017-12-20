# library(TMB)
# compile("deltalognormal_re.cpp")
# dyn.load(dynlib("deltalognormal_re"))
#
# f1 <- pos_catch ~ year_factor
# f2 <- log(spp_catch) ~ year_factor
# mm1 <- model.matrix(f1, data = d_retained)
# mm2 <- model.matrix(f2, data = subset(d_retained, pos_catch == 1))
#
# get_most_freq_factor <- function(x) {
#   names(rev(sort(table(x))))[1]
# }
# newdata <- expand.grid(
#   year_factor = sort(unique(d_retained$year_factor)),
#   month_factor = get_most_freq_factor(d_retained$month_factor),
#   depth_band = get_most_freq_factor(d_retained$depth_band),
#   latitude_band = get_most_freq_factor(d_retained$latitude_band),
#   dfo_locality = get_most_freq_factor(d_retained$locality_code),
#   vessel_name = get_most_freq_factor(d_retained$vessel_name),
#   hours_fished = mean(d_retained$hours_fished),
#   pos_catch = NA,
#   spp_catch = NA,
#   best_depth = mean(d_retained$best_depth))
# mm_pred <- mm2[1:length(unique(d_retained$year)), ]
# for (i in 1:ncol(mm_pred)) {
#   for (j in 1:nrow(mm_pred)) {
#     mm_pred[j, i] <- 0
#   }}
# mm_pred[,1] <- 1
# for (i in 1:ncol(mm_pred)) {
#   for (j in 1:nrow(mm_pred)) {
#     if (i == j)
#       mm_pred[j, i] <- 1
#   }}
#
# m_bin <- speedglm::speedglm(f1, data = d_retained, family = binomial(link = "logit"))
# m_pos <- lm(f2, data = subset(d_retained, pos_catch == 1))
#
# bin_vess <- as.numeric(as.factor(as.character(d_retained$vessel_name)))
# pos_vess <- as.numeric(as.factor(as.character(subset(d_retained, pos_catch == 1)$vessel_name)))

obj <- MakeADFun(
  data = list(
    X1_ij = mm1, y1_i = d_retained$pos_catch,
    X2_ij = mm2, y2_i = log(subset(d_retained, pos_catch == 1)$spp_catch),

    factor1_k = bin_vess,
    factor2_k = pos_vess,

    X1_pred_ij = mm_pred,
    X2_pred_ij = mm_pred,

    nk1 = length(unique(bin_vess)),
    nk2 = length(unique(pos_vess))),
  # parameters = list(b1_j = rep(0, ncol(mm1)), b2_j = rep(0, ncol(mm2)),
  parameters = list(
    b1_j = coef(m_bin) + rnorm(length(coef(m_bin)), 0, 0.05),
    b2_j = coef(m_pos)  + rnorm(length(coef(m_bin)), 0, 0.05),

    z1_k = rep(0, length(unique(bin_vess))),
    z2_k = rep(0, length(unique(pos_vess))),

    log_sigma = log(1),
    log_sigma_z1 = log(1),
    log_sigma_z2 = log(1)),
  # random = c("z1_k", "z2_k"),
  DLL = "deltalognormal_re")

# not using right now!
fit_inla <- function(dat, plot = TRUE, max.edge = c(1, 3), convex = 1.5) {
  library(INLA)
  
  pos_dat <- filter(dat, present == 1)
  coords <- cbind(dat$X10, dat$Y10)
  coords_pos <- cbind(pos_dat$X10, pos_dat$Y10)
  
  bnd <- inla.nonconvex.hull(coords, convex = convex)
  mesh6 = inla.mesh.2d(
    boundary = bnd,
    max.edge = max.edge,
    cutoff = 0.01,
    offset = 1.5)
  
  if (plot) {
    plot(mesh6)
    points(dat$X, dat$Y, col = "red")
  }
  
  A.est6 <- inla.spde.make.A(mesh=mesh6, loc=coords)
  A.est6.pos <- inla.spde.make.A(mesh=mesh6, loc=coords_pos)
  spde <- inla.spde2.matern(mesh=mesh6, alpha=1.5)
  formula <- y ~ -1 + intercept + depth_scaled + depth_scaled2 + f(spatial.field, model=spde)
  s.index <- inla.spde.make.index(name="spatial.field", n.spde=spde$n.spde)
  
  stack.est.pos <- inla.stack(data=list(y=log(pos_dat$density)), A=list(A.est6.pos, 1, 1), 
    effects = list(c(s.index, list(intercept=1)), 
      list(depth_scaled = pos_dat$depth_scaled),
      list(depth_scaled2 = pos_dat$depth_scaled2)), tag="est_pos")
  output6.stack.pos <- inla(formula, data=inla.stack.data(stack.est.pos, spde=spde), 
    family="gaussian", control.predictor=list(A=inla.stack.A(stack.est.pos), compute=TRUE), 
    verbose=FALSE, control.compute=list(config = TRUE))
  
  stack.est.bin <- inla.stack(data=list(y=dat$present), A=list(A.est6, 1, 1), 
    effects = list(c(s.index, list(intercept=1)), 
      list(depth_scaled = dat$depth_scaled),
      list(depth_scaled2 = dat$depth_scaled2)), tag="est_bin")
  output6.stack.bin <- inla(formula, data=inla.stack.data(stack.est.bin, spde=spde), 
    family="binomial", control.predictor=list(A=inla.stack.A(stack.est.bin), compute=TRUE), 
    verbose=FALSE, control.compute=list(config = TRUE))
  
  list(pos = output6.stack.pos, bin = output6.stack.bin, mesh = mesh6)
}

# not using!!
predict_inla <- function(model_bin, model_pos, pred_grid, mesh, n = 1000L) {
  
  inla.mcmc.pos <- inla.posterior.sample(n = n, model_pos)
  inla.mcmc.bin <- inla.posterior.sample(n = n, model_bin)
  na <- rownames(inla.mcmc.pos[[1]]$latent)
  sf <- grep("spatial.field", na)
  b0_ <- grep("intercept", na)[1]
  b1_ <- grep("depth_scaled", na)[1]
  b2_ <- grep("depth_scaled", na)[2]
  
  b0 <- plyr::laply(1:n, function(i) inla.mcmc.pos[[i]]$latent[b0_])
  b1 <- plyr::laply(1:n, function(i) inla.mcmc.pos[[i]]$latent[b1_])
  b2 <- plyr::laply(1:n, function(i) inla.mcmc.pos[[i]]$latent[b2_])
  b0_bin <- plyr::laply(1:n, function(i) inla.mcmc.bin[[i]]$latent[b0_])
  b1_bin <- plyr::laply(1:n, function(i) inla.mcmc.bin[[i]]$latent[b1_])
  b2_bin <- plyr::laply(1:n, function(i) inla.mcmc.bin[[i]]$latent[b2_])
  
  projMatrix <- inla.spde.make.A(mesh, loc=as.matrix(pred_grid[,c("X10", "Y10")]))
  
  pp <- plyr::laply(1:n, function(i) {
    as.numeric(projMatrix%*%inla.mcmc.pos[[i]]$latent[sf]) +
      b0[i] +
      b1[i] * pred_grid$depth_scaled + 
      b2[i] * pred_grid$depth_scaled2
  })
  pb <- plyr::laply(1:n, function(i) {
    as.numeric(projMatrix%*%inla.mcmc.bin[[i]]$latent[sf]) +
      b0_bin[i] +
      b1_bin[i] * pred_grid$depth_scaled + 
      b2_bin[i] * pred_grid$depth_scaled2
  })
  pc <- plogis(pb) * exp(pp)
  
  # spatial field only:
  spp <- plyr::laply(1:n, function(i) {
    as.numeric(projMatrix%*%inla.mcmc.pos[[i]]$latent[sf])
  })
  spb <- plyr::laply(1:n, function(i) {
    as.numeric(projMatrix%*%inla.mcmc.bin[[i]]$latent[sf])
  })
  
  pred_grid$pred_delta <- apply(pc, 2, median)
  pred_grid$pred_binary <- apply(plogis(pb), 2, median)
  pred_grid$pred_positive <- apply(exp(pp), 2, median)
  pred_grid$spatial_field <- apply(pc, 2, median)
  pred_grid$spatial_field_binary <- apply(spb, 2, median)
  pred_grid$spatial_field_positive <- apply(spp, 2, median)
  
  list(pred = pred_grid, prediction_posterior = pc, 
    params = list(b0 = b0, b1 = b1, b2 = b2, b0_bin = b0_bin, b1_bin = b1_bin, b2_bin = b2_bin))
} 
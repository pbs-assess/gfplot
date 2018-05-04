#' Fit a spatial model with INLA
#'
#' @param dat TODO
#' @param response TODO
#' @param n_knots TODO
#' @param family TODO
#' @param max_edge TODO
#' @param kmeans TODO
#' @param plot TODO
#' @param fit_model TODO
#' @param extend TODO
#' @param offset TODO
#' @param cutoff TODO
#' @param include_depth TODO
#'
#' @importFrom INLA inla.models inla.reorderings

fit_inla <- function(dat, response = "present", n_knots = 50,
                     family = "binomial", max_edge = c(3, 10),
                     kmeans = TRUE,
                     plot = FALSE, fit_model = TRUE,
                     extend = list(n = 8, offset = -0.1),
                     offset = c(10, 50), cutoff = 1,
                     include_depth = TRUE,
                     verbose = FALSE,
                     debug = FALSE,
                     trials = 10) {
  d <- dat
  coords <- as.matrix(unique(d[, c("X", "Y")]))

  # use kmeans() to calculate centers:
  if (kmeans) {
    km <- stats::kmeans(x = coords, centers = n_knots)
    mesh <- INLA::inla.mesh.create(km$centers,
      extend = extend
    )
  } else {
    bnd <- INLA::inla.nonconvex.hull(coords)
    mesh <- INLA::inla.mesh.2d(
      offset = offset,
      boundary = bnd,
      max.edge = max_edge,
      cutoff = cutoff
    )
  }

  if (plot) {
    plot(mesh)
    points(coords)
  }

  spde <- INLA::inla.spde2.matern(mesh, alpha = 3 / 2)
  dat <- data.frame(
    y = d[, response, drop = TRUE],
    xcoo = d$X,
    ycoo = d$Y
  )

  dat$intercept <- 1

  if (include_depth) {
    dat$depth <- d$depth_scaled
    dat$depth2 <- d$depth_scaled2
  }

  # construct index for ar1 model:
  iset <- INLA::inla.spde.make.index(
    "i2D",
    n.spde = mesh$n
  )

  # make the covariates:
  X.1 <- dat[, -c(1:3), drop = FALSE]
  covar_names <- colnames(X.1)
  XX.list <- as.list(X.1)
  effect.list <- list()
  effect.list[[1]] <- c(iset)
  for (Z in seq_len(ncol(X.1)))
    effect.list[[Z + 1]] <- XX.list[[Z]]
  names(effect.list) <- c("1", covar_names)

  A <- INLA::inla.spde.make.A(
    mesh = mesh,
    loc = cbind(dat$xcoo, dat$ycoo)
  )
  A.list <- list()
  A.list[[1]] <- A
  for (Z in seq_len(ncol(X.1)))
    A.list[[Z + 1]] <- 1

  # make projection points stack:
  sdat <- INLA::inla.stack(
    tag = "stdata",
    data = list(y = dat$y),
    A = A.list,
    effects = effect.list
  )

  formula <- as.formula(paste(
    "y ~ -1 +",
    paste(covar_names, collapse = "+"),
    "+ f(i2D, model=spde)"
  ))

  if (fit_model) {
    try_fitting <- TRUE
    trial_i <- 1
    while (try_fitting && trial_i <= trials) {
      model <- try(INLA::inla(formula,
        family = family,
        data = INLA::inla.stack.data(sdat),
        control.predictor = list(
          compute = TRUE,
          A = INLA::inla.stack.A(sdat)
        ),
        control.fixed = list(
          mean = 0, prec = 1 / (4^2),
          mean.intercept = 0, prec.intercept = 1 / (25^2)
        ),
        control.compute = list(config = TRUE),
        verbose = verbose,
        debug = debug,
        keep = FALSE
      ))
      trial_i <- trial_i + 1
      if (class(model) != "try-error") {
        try_fitting <- FALSE
      } else {
        if (trial_i > 5) browser()
        message("INLA error. Trying again.")
      }
    }
    if (class(model) == "try-error") {
      model <- NA
    }
  } else {
    model <- NA
  }
  list(
    model = model, mesh = mesh, spde = spde, data = dat,
    formula = formula, iset = iset
  )
}


#' predict_inla()
#'
#' @param obj TODO
#' @param pred_grid TODO
#' @param samples TODO
#' @param include_depth TODO

predict_inla <- function(obj, pred_grid, samples = 100L,
                         include_depth = TRUE) {
  mesh <- obj$mesh
  model <- obj$model
  iset <- obj$iset

  inla.mcmc <- INLA::inla.posterior.sample(n = samples, model)

  # get indices of various effects:
  latent_names <- rownames(inla.mcmc[[1]]$latent)
  re_indx <- grep("^i2D", latent_names)
  if (include_depth) {
    depth_fe_indx1 <- grep("^depth$", latent_names)
    depth_fe_indx2 <- grep("^depth2$", latent_names)
  }
  intercept_indx <- grep("^intercept$", latent_names)

  # read in locations and knots, form projection matrix
  grid_locs <- pred_grid[, c("X", "Y")]

  # projections will be stored as an array:
  projected_latent_grid <-
    array(0, dim = c(nrow(grid_locs), samples))
  proj_matrix <- INLA::inla.spde.make.A(mesh, loc = as.matrix(grid_locs))


  # do MCMC projections:
  indx <- which(iset$i2D.group == 1)
  for (i in seq_len(samples)) {

    if (include_depth) {
      depth_effects <-
        pred_grid[, "depth_scaled", drop = TRUE] * inla.mcmc[[i]]$latent[depth_fe_indx1] +
        pred_grid[, "depth_scaled2", drop = TRUE] * inla.mcmc[[i]]$latent[depth_fe_indx2]
    } else {
      depth_effects <- 0
    }

    projected_latent_grid[, i] <-
      as.numeric(proj_matrix %*% inla.mcmc[[i]]$latent[re_indx][indx]) +
      inla.mcmc[[i]]$latent[intercept_indx] +
      depth_effects
  }

  projected_latent_grid
}

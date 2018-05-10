#' Fit a spatial model with INLA
#'
#' This function fits a special model using Gaussian random fields with INLA.
#' The function is not intended to be used directly by the user, but
#' documentation is included so that you can see the argument values that you
#' might pass from [fit_survey_sets()]. It is called internally by
#' [fit_survey_sets()].
#'
#' @param dat A data frame from `scale_survey_predictors()`.
#' @param response A character value representing the column name for the
#'   response variable.
#' @param n_knots Number of knots in the mesh. Passed to [stats::kmeans()].
#' @param family Family to pass on to INLA. Like we should be "binomial" or
#'   "gamma".
#' @param max_edge Value to pass on to [INLA::inla.mesh.2d()].
#' @param kmeans Logical for whether to use [stats::kmeans()] to calculate the
#'   knot locations before generating the mesh with [INLA::inla.mesh.create()]
#'   (if `TRUE`) or use [INLA::inla.mesh.2d()] if `FALSE`.
#' @param plot Logical for whether the INLA mesh should be plotted.
#' @param fit_model Logic for whether the model should be fitted. Useful for
#'   debugging. Will return `NA` in the model component if `FALSE`.
#' @param extend Value to pass on to [INLA::inla.mesh.create()].
#' @param offset Value to pass on to [INLA::inla.mesh.2d()].
#' @param cutoff Value to pass on to [INLA::inla.mesh.2d()].
#' @param include_depth Logical for whether to include depth as a predictor.
#' @param verbose Logical for verbose output from INLA.
#' @param debug Logical for debugging.
#' @param trials Number of times to retry the model if it fails to fit.
#'
#' @importFrom INLA inla.models inla.reorderings

fit_inla <- function(dat, response = "present", n_knots = 50,
                     family = "binomial", max_edge = c(20, 100),
                     kmeans = FALSE,
                     plot = FALSE, fit_model = TRUE,
                     extend = list(n = 8, offset = -0.1),
                     offset = c(5, 25), cutoff = 10,
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

  message("INLA max_edge = ", "c(", max_edge[[1]], ", ", max_edge[[2]], ")")
  if (plot) {
    graphics::plot(mesh)
    graphics::points(coords)
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
        # if (trial_i > 5) browser()
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


# predict_inla()
#
# @param obj A model fit object from INLA.
# @param pred_grid The grid to project the predictions onto.
# @param samples The number of posterior samples to draw.
# @param include_depth Logical for whether to include depth.

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

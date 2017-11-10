library(INLA)
library(tidyverse)
data(SPDEtoy)
coords <- as.matrix(SPDEtoy[,1:2])
mesh2 <- inla.mesh.2d(loc=coords, max.edge=c(0.1, 0.2), offset = c(0.1, 0.4), cutoff = 0.02)
plot(mesh2)

# A feature in R-INLA named inla.nonconvex.hull makes it possible to include a nonconvex hull as boundary in the mesh construction. 4 We include below an example whose resulting mesh is reported in Figure 6.15.
bnd <- inla.nonconvex.hull(as.matrix(coords),convex=0.07)

mesh6 <- inla.mesh.2d(loc=coords, boundary=bnd, max.edge=c(0.04, 0.2), cutoff=0.05, offset = c(0.1, 0.4))
plot(mesh6)

# The R-INLA function inla.spde.make.A creates the sparse weight matrix A by identifying the data locations in the mesh and organizing the corresponding values of the basis functions. 
  
A.est6 <- inla.spde.make.A(mesh=mesh6, loc=coords)

# We show now how to estimate the model parameters using the mesh6 triangulation and the projector matrix A.est6 created previously and choosing the default prior specifications for the SPDE parameters; see Section 6.9 for more details about how to change priors. First of all, we need to create a Matérn SPDE object through
spde <- inla.spde2.matern(mesh=mesh6, alpha=1.5)

# Then we define the linear predictor through the formula
formula <- y ~ -1 + intercept + f(spatial.field, model=spde)
# removing the default intercept (with the code spefication -1) and including an explicit one named intercept. The spatial random effect is included with the f() term where spatial.field is a proper index variable and spde is the model created previously with inla.spde2.matern. As usual the model is fitted by means of the inla function:

output6 <- inla(
  formula, data = list(y = SPDEtoy$y, intercept = rep(1, spde$n.spde), 
    spatial.field = 1:spde$n.spde),
  control.predictor = list(A = A.est6, compute = TRUE)
)

# Note that the data list also includes the spatial.field variable for the random effect, defined as the sequence of integers from 1 to the number of mesh vertices. Moreover, the projector matrix is provided through the control.predictor option by setting compute=TRUE for obtaining the posterior marginals for the linear predictor.

# The posterior summaries of the intercept and of the precision for the Gaussian observations can be retrieved with the following commands:
round(output6$summary.fixed,3)

# If the interest is on the variance ) e (and not on the precision), it is possible to compute the mean of the transformed posterior marginal through

inla.emarginal(function(x) 1/x, output6$marginals.hyper[[1]])

# The posterior summaries of the spatial parameters can be obtained from the inla output by means of the inla.spde2.result function which extracts all the relevant bits of information from the list output6. Moreover, the function transforms the results from internal parameter scales and provides the posterior distributions for the nominal variance ) 2 and the nominal range r, in addition to the internal results regarding ! 1 = log(#) and ! 2 = log(+). In the code
  
output6.field <- inla.spde2.result(inla=output6, 
  name="spatial.field", spde=spde, do.transf=TRUE)
 
# name denotes the name of the SPDE effect used in the formula (in this case spatial.field) and spde is the result of the call to inla.spde2.matern. The option do.transf=TRUE is used to compute marginals of the parameters on the transformed scale. See names(output6.field) for exploring the elements of the resulting list. The posterior mean of +, ) 2 and the range r can be obtained by typing


# We also introduce a new function named inla.spde.make.index which takes care of creating all the required indexes for the SPDE model and which is particularly useful when replicates or groups are involved in the model specification:
s.index <- inla.spde.make.index(name="spatial.field", n.spde=spde$n.spde)

# Here name represents the name of the effect which will then be used in the formula. The output returned by inla.spde.make.index is composed of a list with the following components:

stack.est <- inla.stack(data=list(y=SPDEtoy$y), A=list(A.est6), effects=list(c(s.index, list(intercept=1))), tag="est") #Estimation

output6.stack <- inla(formula, data=inla.stack.data(stack.est, spde=spde), family="gaussian", control.predictor=list(A=inla.stack.A(stack.est), compute=TRUE))

grid.x <- 50
grid.y <- 50
pred.grid <- expand.grid(x = seq(0, 1, length.out = grid.x), y = seq(0, 1, length.out = grid.y))

# In R-INLA, the simplest way for performing spatial prediction is by joining linear predictors (one for parameter estimation and one for prediction) through the inla.stack function, as described in the previous section. To do this, first of all a new projector matrix for the 2500 grid locations has to be created:
  
A.pred6 <- inla.spde.make.A(mesh=mesh6, loc=as.matrix(pred.grid))

# and then the inla.stack object is built using a proper tag (pred.latent):
  
stack.pred.latent <- inla.stack(data=list(xi=NA), A=list(A.pred6), effects=list(s.index), tag="pred.latent")

# Note that, since we are interested in prediction, we need to specify xi=NA in the data argument; moreover, as the intercept is not included in the effects list, it means we are computing the prediction just for the spatial latent field #(s). If instead the interest is in the prediction of the response variable, we will use the following call which includes the intercept and sets y=NA:

stack.pred.response <- inla.stack(data=list(y=NA), A=list(A.pred6), effects=list(c(s.index, list(intercept=1))), tag="pred.response")

# We join all together the three linear predictors (parameter estimation, prediction of the latent field and prediction of the response) into a single joint stack object:
  
join.stack <- inla.stack(stack.est, stack.pred.latent, stack.pred.response)

# and call the inla function

join.output <- inla(formula, data=inla.stack.data(join.stack), control.predictor=list(A=inla.stack.A(join.stack), compute=TRUE))

# To access the posterior marginal distributions of predictions at the target grid locations, we use the inla.stack.index function to extract the corresponding data indexes from the full stack object using the corresponding tags:
  
index.pred.latent <- inla.stack.index(join.stack, tag="pred.latent")$data

index.pred.response <- inla.stack.index(join.stack, tag="pred.response")$data

post.mean.pred.response <- join.output$summary.linear.predictor[index.pred.response,"mean"]
post.mean.pred.response.sd <- join.output$summary.linear.predictor[index.pred.response,"sd"]

pred.grid$z <- post.mean.pred.response
pred.grid2 <- pred.grid %>% rename(s1 = x, s2 = y) %>% mutate(time = 1)

# glmmfields:
library(glmmfields)
m <- glmmfields(y ~ 1, data = SPDEtoy, lon = "s1", lat = "s2",
  chains = 2, iter = 500, nknots = 25, cores = 2, 
  covariance = "matern", matern_kappa = 1.5)
m
pred.grid2$z <- predict(m, newdata = pred.grid2, estimate_method = "mean")$estimate

# compare:

g1 <- ggplot(pred.grid, aes(x, y)) + geom_tile(aes(fill = z)) + 
  geom_point(data = SPDEtoy, aes(x = s1, y = s2, size = y), pch = 21) +
  viridis::scale_fill_viridis(limits = c(3, 14)) +
  ggtitle("R-INLA")

g2 <- ggplot(pred.grid2, aes(s1, s2)) + geom_tile(aes(fill = z)) + 
  geom_point(data = SPDEtoy, aes(x = s1, y = s2, size = y), pch = 21) +
  viridis::scale_fill_viridis(limits = c(3, 14)) + 
  ggtitle("glmmfields")

gridExtra::grid.arrange(g1, g2)


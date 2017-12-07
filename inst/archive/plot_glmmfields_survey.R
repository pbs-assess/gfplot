source("R/survey-functions.R")

dd1 <- get_surv_data("pacific ocean perch",
  c("Queen Charlotte Sound Synoptic Survey"),
  years = c(2015:2017))
dd1 <- unique(dd1) # FIXME!!
unique(dd1$year)
nrow(dd1)
table(dd1$present)
b <- join_noaa_bathy(dd1)
dd2 <- b$data
dd3 <- scale_predictors(dd2)
dd3$X10 <- dd3$X10 * 10
dd3$Y10 <- dd3$Y10 * 10
m <- fit_glmmfields(dd3, chains = 4L, iter = 800L, n_knots = 15L, adapt_delta = 0.99)
m
dd3$X10 <- dd3$X10 / 10
dd3$Y10 <- dd3$Y10 / 10
pg <- make_prediction_grid(dd3, b$bath, n = 50L, region = "QCS")
pg$X10 <- pg$X10 * 10
pg$Y10 <- pg$Y10 * 10
pos <- predict(m$pos, newdata = data.frame(pg, time = 1),
  type = "response", return_mcmc = TRUE, iter = 250)
bin <- predict(m$bin, newdata = data.frame(pg, time = 1),
  type = "response", return_mcmc = TRUE, iter = 250)
pg$X10 <- pg$X10 / 10
pg$Y10 <- pg$Y10 / 10

com <- bin * pos
pg$combined <- apply(com, 1, median)
pg$combined_lwr <- apply(com, 1, quantile, probs = 0.1)
pg$combined_upr <- apply(com, 1, quantile, probs = 0.9)
pg$bin <- apply(bin, 1, median)
pg$pos <- apply(pos, 1, median)

main_scale <- viridis::scale_fill_viridis(option = "C", 
  limits = sqrt(range(c(pg$combined, pg$combined_lwr, pg$combined_upr))))
binary_scale <- scale_fill_gradient2(low = scales::muted("blue"), 
  mid = "grey90", high = scales::muted("red"), 
  midpoint = 0.5, limits = c(0, 1))
spatial_scale <- scale_fill_gradient2(low = scales::muted("blue"), 
  mid = "grey90", high = scales::muted("red"))

## don't extrapolate beyond observed depth?
# pg <- filter(pg, akima_depth >= min(dd3$akima_depth), 
  # akima_depth <= max(dd3$akima_depth))

pdf("pop-example-glmmfields.pdf", width = 8, height = 8)
plot_bc_map(pg, dd3, "sqrt(combined)", viridis::scale_fill_viridis(option = "D"))
g1 <- plot_bc_map(pg, dd3, "sqrt(combined)", main_scale, show_legend = FALSE)
g2 <- plot_bc_map(pg, dd3, "sqrt(combined_lwr)", main_scale, show_legend = FALSE)
g3 <- plot_bc_map(pg, dd3, "sqrt(combined_upr)", main_scale, show_legend = FALSE)
gridExtra::grid.arrange(g2, g1, g3, nrow = 1)
plot_bc_map(pg, dd3, "bin", binary_scale)
plot_bc_map(pg, dd3, "sqrt(pos)", main_scale)

x <- seq(min(dd3$depth_scaled), max(dd3$depth_scaled), length.out = 200L)
x_real <- exp((x * dd3$depth_sd[1]) + dd3$depth_mean[1])
ep <- rstan::extract(m$pos$model)
eb <- rstan::extract(m$bin$model)

post <- exp(sapply(x, function(i) ep$B[,1] + ep$B[,2] * i + ep$B[,3] * i^2))
l <- apply(post, 2, quantile, probs = 0.025)
u <- apply(post, 2, quantile, probs = 0.975)
plot(x_real, apply(post, 2, quantile, probs = 0.5), type = "l", 
  xlab = "Depth (m)", ylab = "Density",
  ylim = range(c(l, u)))
polygon(c(x_real, rev(x_real)), c(l, rev(u)),
  col = "#00000030", border = NA)

post <- plogis(sapply(x, function(i) eb$B[,1] + eb$B[,2] * i + eb$B[,3] * i^2))
l <- apply(post, 2, quantile, probs = 0.025)
u <- apply(post, 2, quantile, probs = 0.975)
plot(x_real, apply(post, 2, quantile, probs = 0.5), type = "l", 
  xlab = "Depth (m)", ylab = "Prob.",
  yaxs = "i", ylim = c(-0.1, 1.1))
polygon(c(x_real, rev(x_real)), c(l, rev(u)),
  col = "#00000030", border = NA)
points(dd3$akima_depth, jitter(dd3$present, amount = 0.05))

dev.off()


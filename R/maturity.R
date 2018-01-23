library(dplyr)
dbio <- readRDS("data-cache/all-survey-bio.rds")
dbio <- dbio[!duplicated(dbio$specimen_id), ]
dbio <- dbio %>%
  select(species_common_name, species_science_name,
    year, age, length, weight,
    maturity_code, sex, survey_series_desc,
    maturity_convention_desc, maturity_convention_maxvalue)

mat_df <- tibble::tribble(
  ~maturity_convention_desc, ~mature_at,
  "ROCKFISH (1977+)",        3,
  "DOGFISH",                 77,
  "FLATFISH (1978+)",        3
)

dbio <- left_join(dbio, mat_df)
dbio <- mutate(dbio, mature = maturity_code >= mature_at)

xx <- filter(dbio, species_common_name == "canary rockfish",
# xx <- filter(dbio, species_common_name == "pacific ocean perch",
  !is.na(length),
  !is.na(mature))
plot(xx$length, xx$mature)

# summary(m)
library(rstanarm)
options(mc.cores = parallel::detectCores())

# m <- glm(mature ~ length, data = xx, family = binomial)
m <- rstanarm::stan_glm(mature ~ length,
  data = xx, family = binomial, iter = 600, chains = 1,
  prior_intercept = normal(0, 25),
  prior = normal(0, 5, autoscale = TRUE))
prior_summary(m)

l <- seq(min(xx$length), max(xx$length), length.out = 200)
nd <- data.frame(length = l)
pp <- posterior_linpred(m, newdata = nd)
nd$est <- plogis(apply(pp, 2, median))
nd$lwr <- plogis(apply(pp, 2, quantile, probs = 0.05))
nd$upr <- plogis(apply(pp, 2, quantile, probs = 0.95))

logit_50 <- function(a, b) {
  -(log(1) + a) / b
}

e <- as.data.frame(m)
lg <- logit_50(e$`(Intercept)`, e$length)
lg <- quantile(lg, probs = c(0.025, 0.5, 0.975))

l50 <- data.frame(
  lwr = lg[[1]],
  est = lg[[2]],
  upr = lg[[3]])

bins <- seq(min(xx$length), max(xx$length), length.out = 25)
xx$bin <- bins[findInterval(xx$length, bins)]
bin_diff <- diff(bins)[1]
p <- group_by(xx, bin) %>%
  summarise(p = mean(mature), mean_length = mean(length),
    n = n())

# plot:
par(mfrow = c(1, 1), mar = c(3.5, 3, 0, 0),
  oma = c(0, 0, .5, .5), cex = 0.7,
  tcl = -0.2, mgp = c(2, 0.4, 0))

plot(nd$length, nd$est, ylim = c(0, 1),
  yaxs = "i", xlab = "Length", ylab = "Proportion mature",
  las = 1, type = "n", axes = FALSE)
axis(1, las = 1, col = "grey60", col.axis = "grey40", col.ticks = "grey70")
axis(2, las = 1, col = "grey60", col.axis = "grey40", col.ticks = "grey70")
box(col = "grey60")

# polygon(c(nd$length, rev(nd$length)), c(nd$lwr, rev(nd$upr)),
  # col = "#00000020", border = NA)
points(xx$length,
  jitter(as.numeric(xx$mature), amount = 0.01),
  col = "#00000005", pch = 19, cex = 0.7)
summary(m)

# points(p$mean_length, p$p)
radius <- function(area) {
  sqrt(area / 3.141592)
}
par(xpd = NA)
symbols(p$mean_length, y = p$p,
  circles = radius(p$n / max(p$n)) + 0.25,
  inches = FALSE, add = TRUE, bg = "grey40", fg = "grey40")
par(xpd = FALSE)

abline(v = l50$est)
rect(xleft = l50$lwr, xright = l50$upr, ybottom = 0, ytop = 1,
  border = NA, col = "#00000040")
lines(nd$length, nd$est, col = "red", lwd = 2)


source("R/add-label.R")
add_label(0.1, 0.1, label =
    paste0("l50 = ", sprintf("%.1f", round(l50$est, 1))))

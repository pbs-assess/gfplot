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
plot(nd$length, nd$est, ylim = c(0, 1), type = "l",
  yaxs = "i", xlab = "Length", ylab = "Proportion mature",
  las = 1)
polygon(c(nd$length, rev(nd$length)), c(nd$lwr, rev(nd$upr)),
  col = "#00000020", border = NA)
# points(xx$length, jitter(as.numeric(xx$mature), amount = 0.05), col = "#00000020")
summary(m)

a <- coef(m)[[1]]
b <- coef(m)[[2]]
pi <- rstanarm::posterior_interval(m, prob = 0.95)

logit_50 <- function(a, b) {
  -(log(1) + a) / b
}
l50 <- data.frame(
  est = logit_50(a, b),
  lwr = logit_50(pi[1,1], pi[2,1]),
  upr = logit_50(pi[1,2], pi[2,2]))

abline(v = l50$est)
rect(xleft = l50$lwr, xright = l50$upr, ybottom = 0, ytop = 1,
  border = NA, col = "#00000040")

bins <- seq(min(xx$length), max(xx$length), length.out = 30)
xx$bin <- bins[findInterval(xx$length, bins)]
bin_diff <- diff(bins)[1]
p <- group_by(xx, bin) %>%
  summarise(p = mean(mature), mean_length = mean(length))
points(p$mean_length, p$p)

l50

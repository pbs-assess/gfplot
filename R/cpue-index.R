library(tidyverse)

catch <- readRDS("~/Dropbox/dfo/selected-data/gf_merged_catch_1996_onwards.rds")
areas <- readRDS("~/Dropbox/dfo/selected-data/major_stat_area.rds")
areas <- areas[grep("5[CDE]+", areas$MAJOR_STAT_AREA_DESCRIPTION), ]
species <- readRDS("~/Dropbox/dfo/selected-data/species.rds")
names(catch) <- tolower(names(catch))
names(areas) <- tolower(names(areas))
names(species) <- tolower(names(species))
catch <- inner_join(catch, species, by = "species_code")

lat_bands <- seq(48, 60, 0.1)
depth_bands <- seq(50, 550, 25)

d <- inner_join(catch, areas, by = "major_stat_area_code") %>%
  mutate(year = lubridate::year(best_date)) %>%
  filter(year >= 1996) %>%
  filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
  filter(gear == "BOTTOM TRAWL") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  filter(latitude > 48) %>%
  mutate(month = lubridate::month(best_date)) %>%
  mutate(hours_fished =
      as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))) %>%
  filter(hours_fished > 0) %>%
  mutate(catch = landed_kg + discarded_kg)

d_fe <- group_by(d, fishing_event_id, month, locality_code,
  vessel_name, year, hours_fished, trip_id) %>%
  mutate(
    spp_in_fe = "WALLEYE POLLOCK" %in% species_common_name,
    spp_in_row = species_common_name == "WALLEYE POLLOCK") %>%
  summarize(pos_catch = ifelse(spp_in_fe[[1]], 1, 0),
    spp_catch = sum(ifelse(spp_in_row, catch, 0), na.rm = TRUE),
    best_depth = mean(best_depth, na.rm = TRUE),
    latitude = mean(latitude, na.rm = TRUE))
# rm(catch, d, d2)

# get those with __ as in pollock res doc. 2017
# fish 4+ years and 4+ trips <- all years? in one year? any year?
# must catch something for a tow/FE to count

caught_something <- d_fe %>%
  filter(year <= 2015, spp_catch > 0) %>%
  group_by(year, vessel_name, trip_id) %>%
  summarise(sum_catch = sum(spp_catch, na.rm = TRUE)) %>%
  filter(sum_catch > 0) %>%
  ungroup() %>% group_by(year, vessel_name) %>%
  mutate(n_trips = length(unique(trip_id))) %>%
  ungroup() %>% group_by(vessel_name) %>%
  mutate(n_years = length(unique(year))) %>%
  filter(n_years >= 4) %>%
  ungroup() %>% group_by(vessel_name) %>%
  mutate(max_trips_per_year = max(n_trips)) %>%
  ungroup() %>%
  filter(max_trips_per_year >= 4)

d_retained <- filter(d_fe, vessel_name %in% unique(caught_something$vessel_name))
d_retained <- mutate(d_retained, pos_catch = ifelse(spp_catch > 0, 1, 0))

# compare to pg 100:
length(unique(caught_something$vessel_name))
sum(d_retained$spp_catch)/1000
nrow(d_retained)
nrow(filter(d_retained, spp_catch > 0))

d_retained <- ungroup(d_retained) %>%
  filter(year <= 2015) %>%
  mutate(
    vessel_name = as.factor(vessel_name),
    latitude_band = as.factor(lat_bands[findInterval(latitude, lat_bands)]),
    year = year,
    dfo_locality = as.factor(locality_code)) %>%
  filter(best_depth >= min(depth_bands), best_depth <= max(depth_bands)) %>%
  mutate(depth_band = as.factor(depth_bands[findInterval(best_depth, depth_bands)]))

p0 <- mean(filter(d_retained, year == 1996)$pos_catch)
p0

group_by(d_fe, year) %>%
  summarise(cpue_mean = sum(spp_catch)/sum(hours_fished), na.rm = TRUE) %>%
  ungroup() %>%
  ggplot(aes(year, cpue_mean)) + geom_line() + ggtitle("Arithmetic CPUE")

d_retained$year_factor <- as.factor(d_retained$year)
d_retained$month_factor <- as.factor(d_retained$month)
f <- pos_catch ~ year_factor + month_factor +
  dfo_locality + depth_band + latitude_band


m_pos <- lm(log(spp_catch) ~ year_factor + dfo_locality + depth_band +
    month_factor + vessel_name + poly(hours_fished, 3) + latitude_band,
  data = filter(d_retained, pos_catch == 1))



system.time({
  m_bin <- glm(f, data = d_retained, family = binomial(link = "logit"))
})
system.time({
  m_bin <- speedglm::speedglm(f, data = d_retained, family = binomial(link = "logit"))
})

#
# # library(mgcv)
# m_tw <- gam(f, data = d_retained, family = Tweedie(1.25,power(.1)))
# get_most_freq_factor <- function(x) {
#   names(rev(sort(table(x))))[1]
# }
# p_tw <- predict(m_tw, newdata = expand.grid(
#   year_factor = sort(unique(d_retained$year_factor)),
#   month_factor = get_most_freq_factor(d_retained$month_factor),
#   depth_band = get_most_freq_factor(d_retained$depth_band),
#   latitude_band = get_most_freq_factor(d_retained$latitude_band),
#   dfo_locality = get_most_freq_factor(d_retained$locality_code)
# ))
# plot(1996:2015, p_tw, type = "l")


mm <- model.matrix(f, data = d_retained)

# d2$logit_pos_catch <- binomial()$linkfun(d2$pos_catch)
# m_check <- glm(f, data = dbin,
# family = binomial(link = "logit"))

library(TMB)
compile("logistic2.cpp")
dyn.load(dynlib("logistic2"))

m_bin_start <- glm(f, data = d_retained, family = binomial(link = "logit"))

obj <- MakeADFun(
  data = list(X_ij = mm, y_i = d_retained$pos_catch),
  # parameters = list(b_j = c(rep(0, ncol(mm)))),
  parameters = list(b_j = coef(m_bin)),
  DLL = "logistic2")
# obj$fn(obj$par)
# obj$gr(obj$par)

# system.time({
# opt <- nlminb(
#   start = obj$par,
#   objective = obj$fn,
#   gradient = obj$gr, control = list(iter.max = 500, eval.max = 500))
# })

system.time({
opt2 <- optim(
  par = obj$par,
  fn = obj$fn,
  gr = obj$gr, method = "BFGS", control = list(maxit = 500))
})

# mstan2 <- rstanarm::stan_glm(pos_catch ~ year_factor + month_factor +
#     dfo_locality + depth_band + latitude_band,
#   data = d_retained,
#   family = binomial(link = "logit"),
#   algorithm = "optimizing")
# library(rstanarm)
#
# mstan3 <- rstanarm::stan_glm(pos_catch ~ year_factor + month_factor +
#     dfo_locality + depth_band + latitude_band,
#   data = d_retained,
#   init = 0,
#   family = binomial(link = "logit"), iter = 100, chains = 1)

library(rstan)
qq <- d_retained[, ]
mm <- model.matrix(f, data = qq)
sm <- stan_model("logistic.stan")
system.time({
m_stan <- optimizing(sm,
  data = list(X = mm, y = qq$pos_catch, N = nrow(qq), J = ncol(mm)))
})

initf1 <- function() {
  list(beta = as.numeric(as.matrix(m_stan$par)[,1]))
}

m_stan2 <- stan("logistic.stan",
  data = list(X = mm, y = qq$pos_catch, N = nrow(qq), J = ncol(mm)),
  iter = 100, chains = 1, thin = 1, init = initf1)

mm2 <- model.matrix(pos_catch ~ year_factor, data = qq)
system.time({
  opt <- optimizing(sm,
    data = list(X = mm2, y = qq$pos_catch, N = nrow(qq), J = ncol(mm2)))
})
initf1 <- function() {
  list(beta = as.numeric(as.matrix(opt$par)[,1]))
}
m_stan22 <- stan("logistic.stan",
  data = list(X = mm2, y = qq$pos_catch, N = nrow(qq), J = ncol(mm2)),
  iter = 100, chains = 1, thin = 1, init = initf1)

# m_stan3 <- vb(sm,
#   data = list(X = mm, y = qq$pos_catch, N = nrow(qq), J = ncol(mm)),
#   init = initf1)
# extract(m_stan3)


# b <- coef(m_pos)
# b_y <- b[seq(1, max(grep("year", names(b))))]
# for (i in 2:length(b_y)) {
#   b_y[i] <- b_y[i] + b_y[1]
# }
#
# b2 <- coef(m_bin)
# b_y2 <- b2[seq(1, max(grep("year", names(b2))))]
# for (i in 2:length(b_y2)) {
#   b_y2[i] <- b_y2[i] + b_y2[1]
# }
#
# cols <- RColorBrewer::brewer.pal(3, "Dark2")
# cols <- c("red", "blue")
# par(mfrow = c(1, 1))
# plot(seq(1996, 2015), scale((b_y)), type = "o", col = cols[1], ylim = c(-2, 3.5))
# lines(seq(1996, 2015), scale((b_y2)), type = "o", col = cols[2])
# # P.S.!!!
# cyy <- function(p0, byy, lyy) {lyy / (1 - p0 * (1 - 1/(byy)))}
# lines(seq(1996, 2015),
#   scale(cyy(p0 = p0, byy = (b_y2), lyy = (b_y))),
#   type = "o", col = "black", lwd = 2)

get_most_freq_factor <- function(x) {
  names(rev(sort(table(x))))[1]
}

newdata <- expand.grid(
  year_factor = sort(unique(d_retained$year_factor)),
  month_factor = get_most_freq_factor(d_retained$month_factor),
  depth_band = get_most_freq_factor(d_retained$depth_band),
  latitude_band = get_most_freq_factor(d_retained$latitude_band),
  dfo_locality = get_most_freq_factor(d_retained$locality_code),
  vessel_name = get_most_freq_factor(d_retained$vessel_name),
  hours_fished = mean(d_retained$hours_fished))

p_bin <- predict(m_bin, newdata = newdata)
p_pos <- predict(m_pos, newdata = newdata)

pp_bin <- posterior_linpred(mstan, newdata = newdata)

plot_dat <- data.frame(
  positive_index = scale(exp(b_y)),

  )
# prob * mean:
plot(seq(1996, 2015), scale(exp(p_pos)), type = "o", col = cols[1], ylim = c(-2, 3.5))
lines(seq(1996, 2015), scale(plogis(p_bin)), type = "o", col = cols[2])
lines(seq(1996, 2015),
  scale(
    plogis(p_bin) * exp(p_pos)
  ),
  type = "o", col = "black", lwd = 2)


# boot:

hurdle_fn <- function(data, i) {
  dat_boot <- d_retained[i, ]
  m1 <- glm(non_zero ~ 1, data = dat_boot,
    family = binomial(link = logit))
  m2 <- glm(y ~ 1, data = subset(dat_boot, non_zero == 1),
    family = Gamma(link = log))
  bin_coef <- plogis(coef(m1)[[1]])
  gamma_coef <- exp(coef(m2)[[1]])
  exp(log(bin_coef) + log(gamma_coef))
}

library(boot)
b <- boot(d, hurdle_fn, R = 500)
b.ci <- boot.ci(b, type = "bca")
print(b.ci)
















# -----------
library(mgcv)
m_pos2 <- gam(log(catch/hours_fished) ~ as.factor(year) + dfo_locality + s(best_depth) +
    s(month, k = 12, bs = "cc") + vessel_name +
    s(latitude), data = filter(d2, catch > 0))

par(mfrow = c(1, 3))
plot(m_pos2)

p <- predict(m_pos2, newdata = data.frame(
  year = sort(unique(as.factor(d$year))),
  month = mean(d$month),
  latitude = mean(d$latitude),
  best_depth = mean(d$best_depth),
  dfo_locality = names(rev(sort(table(d$dfo_locality))))[[1]],
  vessel_name = names(rev(sort(table(d$vessel_name))))[[1]]
)) %>% exp()

b2 <- coef(m_pos2)
b_y2 <- b[seq(1, max(grep("year", names(b2))))]
for (i in 2:length(b_y2)) {
  b_y2[i] <- b_y2[i] + b_y2[1]
}

cols <- RColorBrewer::brewer.pal(3, "Dark2")
par(mfrow = c(1, 1))
plot(seq(1996, 2017), scale(exp(b_y)), type = "o", col = cols[1], ylim = c(-2, 3.5))
lines(seq(1996, 2017), scale(p), type = "o", col = cols[2])
lines(seq(1996, 2017), scale(exp(b_y2)), type = "o", col = cols[3])

# too slow!
# library(gamm4)
# m_pos3 <- gamm4(log(catch/hours_fished) ~ as.factor(year) + s(best_depth) +
#     s(month, k = 12, bs = "cc") +
#     s(latitude), data = filter(d, catch > 0),
#   random = ~ (1|dfo_locality) + (1|vessel_name))
# summary(m_pos3)

library(glmmTMB)
m_pos3 <- glmmTMB(log(catch/hours_fished) ~ as.factor(year) + depth_band +
    as.factor(month) +
    latitude_band + (1|dfo_locality) + (1|vessel_name),
  data = filter(d, catch > 0))
summary(m_pos3)

b3 <- fixef(m_pos3)$cond
b_y3 <- b[seq(1, max(grep("year", names(b3))))]
for (i in 3:length(b_y3)) {
  b_y3[i] <- b_y3[i] + b_y3[1]
}
lines(seq(1996, 2017), scale(exp(b_y3)), type = "o", col = "red")
# weird looking? and long?

# P.S.!!!
cyy <- function(p0, byy, lyy) {lyy / (1 - p0 * (1 - 1/exp(byy)))}
x <- seq(-3, 3, length.out = 100)
plot(x, scale(cyy(p0 = 0.5, byy = x, lyy = 10)), type = "l", lty = 2)
lines(x, scale(10 * plogis(0.5 + x)))
abline(h = 10, col = "grey80")

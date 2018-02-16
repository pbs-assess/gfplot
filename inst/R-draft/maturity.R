library(dplyr)

dbio <- readRDS("data-cache/all-survey-bio.rds")
dbio <- dbio[!duplicated(dbio$specimen_id), ]
dbio <- dbio %>%
  select(species_common_name, species_science_name,
    year, age, length, weight,
    maturity_code, sex, survey_series_desc,
    maturity_convention_desc, maturity_convention_maxvalue,
    specimen_id, sample_id, trip_start_date)

dbio <- mutate(dbio, month = lubridate::month(trip_start_date))

library(readr)
mat_df <- readr::read_csv("data/maturity_assignment.csv",
  col_types = readr::cols(
  maturity_convention_code = col_integer(),
  maturity_convention_description = col_character(),
  specimen_sex_code = col_integer(),
  maturity_convention_maxvalue = col_integer(),
  mature_at = col_integer())) %>%
  rename(sex = specimen_sex_code,
    maturity_convention_desc = maturity_convention_description) %>%
  select(-maturity_convention_maxvalue)

dbio <- left_join(dbio, mat_df, by = c("sex", "maturity_convention_desc"))
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
m <- stan_glm(mature ~ length,
  data = xx, family = binomial, iter = 600, chains = 1,
  prior_intercept = normal(0, 25),
  prior = normal(0, 5, autoscale = TRUE))
prior_summary(m)
#
# m_re <- stan_glmer(mature ~ length + (1 | sample_id),
#   data = xx, family = binomial, iter = 600, chains = 1,
#   prior_intercept = normal(0, 25),
#   prior = normal(0, 5, autoscale = TRUE))


tows <- readRDS("data-cache/all-survey-spatial-tows.rds")
tows <- readRDS("data-cache/")
xx <- left_join(xx, select(tows, ))


m <- glm(mature ~ length,
  data = xx, family = binomial)

library(glmmTMB)
m_re <- glmmTMB(mature ~ 0 + length + as.factor(month) + (1 | sample_id),
  data = xx, family = binomial)

confint(m)
confint(m_re)
b <- fixef(m_re)[[1]]

l <- seq(min(xx$length), max(xx$length), length.out = 100)
nd <- expand.grid(length = l, sample_id = unique(xx$sample_id), stringsAsFactors = FALSE)
nd$glm <- plogis(predict(m, newdata = nd))
nd$glmm <- predict(m_re, newdata = nd, se.fit = FALSE)
nd$glmm_fe <- plogis(b[[1]] + b[[2]] * nd$l)
nd_fe <- filter(nd, sample_id == xx$sample_id[[1]]) %>%
  select(-glmm)

library(ggplot2)
reshape2::melt(nd_fe, id.vars = c("length", "sample_id")) %>%
  ggplot(aes(length, value,
    colour = variable, alpha = variable, size = variable)) +
  geom_line(data = nd, aes(length, glmm, group = sample_id),
     inherit.aes = FALSE, alpha = 0.03) +
  geom_line() +
  theme_light() +
  scale_alpha_manual(values = c("glm" = 1, "glmm_fe" = 1, "glmm" = 0.1)) +
  scale_size_manual(values = c("glm" = 1.5, "glmm_fe" = 1.5, "glmm" = 0.3)) +
  scale_colour_manual(values = c("glm" = "blue", "glmm_fe" = "black", "glmm" = "black")) +
  ggtitle("Sample ID random intercepts") +
  labs(subtitle = "Canary rockfish") + ylab("Probability mature") + xlab("Length")
ggsave("figs/canary-mature-re-int.png", width = 6, height = 4)

########

m_re <- glmmTMB(mature ~ length + (1 + length | sample_id),
  data = xx, family = binomial)

confint(m)
confint(m_re)
b <- fixef(m_re)[[1]]

nd$glm <- plogis(predict(m, newdata = nd))
nd$glmm <- predict(m_re, newdata = nd, se.fit = FALSE)
nd$glmm_fe <- plogis(b[[1]] + b[[2]] * nd$l)
nd_fe <- filter(nd, sample_id == xx$sample_id[[1]]) %>%
  select(-glmm)

library(ggplot2)
reshape2::melt(nd_fe, id.vars = c("length", "sample_id")) %>%
  ggplot(aes(length, value,
    colour = variable, alpha = variable, size = variable)) +
  geom_line(data = nd, aes(length, glmm, group = sample_id),
    inherit.aes = FALSE, alpha = 0.03) +
  geom_line() +
  theme_light() +
  scale_alpha_manual(values = c("glm" = 1, "glmm_fe" = 1, "glmm" = 0.1)) +
  scale_size_manual(values = c("glm" = 1.5, "glmm_fe" = 1.5, "glmm" = 0.3)) +
  scale_colour_manual(values = c("glm" = "blue", "glmm_fe" = "black", "glmm" = "black")) +
  ggtitle("Sample ID random intercepts and slopes") +
  labs(subtitle = "Canary rockfish") + ylab("Probability mature") + xlab("Length")
ggsave("figs/canary-mature-re-slope.png", width = 6, height = 4)









l <- seq(min(xx$length), max(xx$length), length.out = 200)
nd <- data.frame(length = l)
pp <- posterior_linpred(m, newdata = nd)
nd$est <- plogis(apply(pp, 2, median))
nd$lwr <- plogis(apply(pp, 2, quantile, probs = 0.05))
nd$upr <- plogis(apply(pp, 2, quantile, probs = 0.95))

logit_perc <- function(a, b, perc = 0.5) {
  -(log((1/perc) - 1) + a) / b
}

e <- as.data.frame(m)
lg <- logit_perc(e$`(Intercept)`, e$length, 0.5)
lg <- quantile(lg, probs = c(0.025, 0.5, 0.975))
l95 <- logit_perc(e$`(Intercept)`, e$length, 0.95) %>% median()
l05 <- logit_perc(e$`(Intercept)`, e$length, 0.05) %>% median()

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


dir.create("maturity", showWarnings = FALSE)

pdf("maturity/mat-canary.pdf", width = 2.7, height = 2.2)
par(mfrow = c(1, 1), mar = c(3.5, 3, 0, 0),
  oma = c(0, 0, .5, .5), cex = 0.7,
  tcl = -0.2, mgp = c(2, 0.4, 0))

plot(nd$length, nd$est, ylim = c(0, 1),
  yaxs = "i", xlab = "", ylab = "",
  las = 1, type = "n", axes = FALSE)
axis(1, las = 1, col = "grey60", col.axis = "grey40", col.ticks = "grey70")
axis(2, las = 1, col = "grey60", col.axis = "grey40", col.ticks = "grey70")
box(col = "grey60")

# polygon(c(nd$length, rev(nd$length)), c(nd$lwr, rev(nd$upr)),
# col = "#00000020", border = NA)
# points(xx$length,
# jitter(as.numeric(xx$mature), amount = 0.01),
# col = "#00000002", pch = 19, cex = 0.7)
summary(m)

# points(p$mean_length, p$p)
radius <- function(area) {
  sqrt(area / 3.141592)
}

abline(v = l50$est, lty = 2, col = "grey40", lwd = 1.5)
abline(v = l95, lty = 2, col = "grey40", lwd = 1.5)
abline(v = l05, lty = 2, col = "grey40", lwd = 1.5)

# abline(h = 0.05, lty = 2, col = "grey30", lwd = 1.5)
# abline(h = 0.5, lty = 2, col = "grey30", lwd = 1.5)
# abline(h = 0.95, lty = 2, col = "grey30", lwd = 1.5)

# rect(xleft = l50$lwr, xright = l50$upr, ybottom = 0, ytop = 1,
# border = NA, col = "#00000040")
lines(nd$length, nd$est, col = "red", lwd = 2.2)
par(xpd = NA)
symbols(p$mean_length, y = p$p,
  circles = radius(p$n / max(p$n)) + 0.45,
  inches = FALSE, add = TRUE, bg = "grey40", fg = "grey40")
par(xpd = FALSE)

source("R/add-label.R")
ytop <- 0.09
gap <- 0.075
xleft <- 0.01

add_label(xleft, ytop, label =
    paste0("L05 = ", sprintf("%.1f", round(l05, 1)), " cm"))
add_label(xleft, ytop + gap, label =
    paste0("L50 = ", sprintf("%.1f", round(l50$est, 1)), " cm"))
add_label(xleft, ytop + gap * 2, label =
    paste0("L95 = ", sprintf("%.1f", round(l95, 1)), " cm"))

line <- 1.75
mtext("Length (cm)", side = 1, col = "grey30", line = line, cex = 0.8)
mtext("Proportion mature", side = 2, col = "grey30", line = line, cex = 0.8)
dev.off()

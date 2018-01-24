library(dplyr)
library(rio)

dbio <- readRDS("data-cache/all-survey-bio.rds")
dbio <- dbio[!duplicated(dbio$specimen_id), ]
dbio <- dbio %>%
  select(species_common_name, species_science_name,
         year, age, length, weight,
         maturity_code, sex, survey_series_desc,
         maturity_convention_desc, maturity_convention_maxvalue)

mat_df <- import("data/maturity_assignment.csv") %>%
  rename(sex = specimen_sex_code, maturity_convention_desc = maturity_convention_description)

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
par(xpd = NA)
symbols(p$mean_length, y = p$p,
  circles = radius(p$n / max(p$n)) + 0.25,
  inches = FALSE, add = TRUE, bg = "grey40", fg = "grey40")
par(xpd = FALSE)

abline(v = l50$est, lty = 2, col = "grey40", lwd = 1.5)
abline(v = l95, lty = 2, col = "grey40", lwd = 1.5)
abline(v = l05, lty = 2, col = "grey40", lwd = 1.5)

# abline(h = 0.05, lty = 2, col = "grey30", lwd = 1.5)
# abline(h = 0.5, lty = 2, col = "grey30", lwd = 1.5)
# abline(h = 0.95, lty = 2, col = "grey30", lwd = 1.5)

# rect(xleft = l50$lwr, xright = l50$upr, ybottom = 0, ytop = 1,
  # border = NA, col = "#00000040")
lines(nd$length, nd$est, col = "red", lwd = 2.2)

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

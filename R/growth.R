library(dplyr)

source("add-label.R")

dbio <- readRDS("data-cache/all-survey-bio.rds")
dbio <- dbio[!duplicated(dbio$specimen_id), ]

dup <- group_by(dbio, species_common_name) %>%
  summarise(n_spp = length(unique(species_science_name))) %>%
  arrange(-n_spp) %>%
  filter(n_spp > 1)
stopifnot(nrow(dup) == 0)

dbio <- dbio %>%
  select(species_common_name, species_science_name,
    year, age, length, weight,
    maturity_code, sex, survey_series_desc,
    maturity_convention_desc, maturity_convention_maxvalue)

# TODO move to outside:
# bad data:
dbio <- dbio[-which(dbio$length > 600 & dbio$species_common_name == "north pacific spiny dogfish"), ]
dbio <- dbio[-which(dbio$length > 600 & dbio$species_common_name == "big skate"), ]
dbio <- dbio[-which(dbio$length > 600 & dbio$species_common_name == "longnose skate"), ]
dbio <- dbio[-which(dbio$length > 60 & dbio$species_common_name == "pacific tomcod"), ]
dbio <- dbio[-which(dbio$length > 50 & dbio$species_common_name == "quillback rockfish"), ]
dbio <- dbio[-which(dbio$length < 10 & dbio$weight/1000 > 1.0 &
    dbio$species_common_name == "pacific flatnose"), ]

library(rstan)
rstan_options(auto_write = TRUE)
smo <- stan_model("inst/stan/vb.stan")

source("R/make-spp-list.R")
spp <- get_spp_names()$species_common_name

dir.create("vb", showWarnings = FALSE)

female_col <- RColorBrewer::brewer.pal(5, "Reds")[[4]]

for (i in seq_along(spp)) {
  # VB:
  d <- filter(dbio, species_common_name == spp[i], !is.na(length),
    !is.na(age))
  d_male <- filter(d, sex == 1)
  d_female <- filter(d, sex == 2)

  fit_vb_male <- FALSE
  fit_vb_female <- FALSE
  if (nrow(d_male) > 100) fit_vb_male <- TRUE
  if (nrow(d_female) > 100) fit_vb_female <- TRUE

  # downsample for now for speed:
  set.seed(1)
  if (nrow(d_male) > 1000)
    d_male <- d_male[sample(seq_len(nrow(d_male)), 1000), ]
  if (nrow(d_female) > 1000)
    d_female <- d_female[sample(seq_len(nrow(d_female)), 1000), ]

  if (fit_vb_male) {
    # if (nrow(d_male) > 20000) {
    #   m_vb_male <- rstan::optimizing(
    #     smo, data = list(N = nrow(d_male), age = d_male$age, length = d_male$length),
    #     init = list(k = 0.1, linf = 50, sigma = 0.1, t0 = -1),
    #     hessian = TRUE, iter = 5000L)
    #   p_male <- m_vb_male$par
    # } else {
    m_vb_male <- rstan::stan("R/vb.stan",
      data = list(N = nrow(d_male), age = d_male$age, length = d_male$length,
        linf_upper_sd = quantile(d_male$length, 0.99)[[1]] * 2),
      iter = 800, chains = 1, cores = 3, thin = 2)
    p_male <- lapply(extract(m_vb_male), median)
    # }
    ages <- seq(quantile(d$age, probs = 0.001),
      quantile(d$age, probs = 0.999), length.out = 200)
    lengths_male <- p_male[["linf"]] *
      (1 - exp(-p_male[["k"]] * (ages - p_male[["t0"]])))
  }
  if (fit_vb_female) {
    # if (nrow(d_female) > 20000) {
    #   m_vb_female <- rstan::optimizing(
    #     smo, data = list(N = nrow(d_female), age = d_female$age, length = d_female$length),
    #     init = list(k = 0.1, linf = 50, sigma = 0.1, t0 = -1),
    #     hessian = TRUE, iter = 5000L)
    #   p_female <- m_vb_female$par
    # } else {
    m_vb_female <- rstan::stan("R/vb.stan",
      data = list(N = nrow(d_female), age = d_female$age, length = d_female$length,
        linf_upper_sd = quantile(d_female$length, 0.99)[[1]] * 2),
      iter = 800, chains = 1, cores = 3, thin = 2)
    p_female <-lapply(extract(m_vb_female), median)
    # }
    ages <- seq(quantile(d$age, probs = 0.000),
      quantile(d$age, probs = 1), length.out = 200)
    lengths_female <- p_female[["linf"]] *
      (1 - exp(-p_female[["k"]] * (ages - p_female[["t0"]])))
  }

  # Weight:
  d_weight <-  filter(dbio,
    species_common_name == spp[i],
    !is.na(length), !is.na(weight))
  d_weight_male <- filter(d_weight, sex == 1)
  d_weight_female <- filter(d_weight, sex == 2)

  fit_weight_male <- FALSE
  fit_weight_female <- FALSE
  if (nrow(d_weight_male) > 50) fit_weight_male <- TRUE
  if (nrow(d_weight_female) > 50) fit_weight_female <- TRUE

  if (fit_weight_male | fit_weight_female)
    nd <- data.frame(length = seq(quantile(d_weight$length, probs = 0.00),
      quantile(d_weight$length, probs = 1), length.out = 200))
  if (fit_weight_female) {
    m_weight_female <- MASS::rlm(log(weight) ~ log(length), data = d_weight_female)
    nd$weight_female <- predict(m_weight_female, newdata = nd)
    # syx <- summary(m_weight_female)$sigma # bias correction
    # cf <- exp((syx^2)/2) # bias correction
    # cf <- 1
    # nd$weight_female <- nd$weight_female * cf # bias correction
    p_weight_female <- coef(m_weight_female)
  }
  if (fit_weight_male) {
    m_weight_male <- MASS::rlm(log(weight) ~ log(length), data = d_weight_male)
    nd$weight_male <- predict(m_weight_male, newdata = nd)
    # syx <- summary(m_weight_male)$sigma # bias correction
    # cf <- exp((syx^2)/2) # bias correction
    # cv <- 1
    # nd$weight_male <- nd$weight_male * cf # bias correction
    p_weight_male <- coef(m_weight_male)
  }

  pdf(paste0("vb/", gsub("/", "-", gsub(" ", "-", spp[[i]])), ".pdf"),
    width = 2.7, height = 4)

  par(mfrow = c(2, 1), mar = c(3.5, 0, 0, 0), oma = c(0, 3, .5, .5), cex = 0.7,
    tcl = -0.2, mgp = c(2, 0.4, 0))

  set.seed(42)
  d_plot <- d
  if (nrow(d_plot) > 2000 & nrow(d) > 0)
    d_plot <- d[sample(seq_len(nrow(d)), 2000), ]

  if (fit_vb_female | fit_vb_male) {
    with(d_plot, plot(age, length, pch = 20, col = "#88888830", type = "p",
      axes = FALSE, ann = FALSE, xaxs = "i", xlim = range(d_plot$age) + c(-0.25, 0.25)))
    axis(1, las = 1, col = "grey60", col.axis = "grey40", col.ticks = "grey70")
    axis(2, las = 1, col = "grey60", col.axis = "grey40", col.ticks = "grey70")
  } else {
    plot(1, 1, ann = FALSE, axes = FALSE, type = "n")
  }

  line <- 1.75

  if (fit_vb_male) {
    lines(ages, lengths_male, col = "grey10", lwd = 3)
  }
  if (fit_vb_female) {
    lines(ages, lengths_female, col = female_col, lwd = 3)
  }

  box(col = "grey60")
  mtext("Age (years)", side = 1, col = "grey30", line = line, cex = 0.8)
  mtext("Length (cm)", side = 2, col = "grey30", line = line, cex = 0.8)

  ytop <- 0.71
  gap <- 0.075
  xleft <- 0.40

  if (fit_vb_female) {
    add_label(label = "Females", xfrac = xleft, yfrac = ytop, col = female_col)
    add_label(label = paste0("k = ", sprintf("%.2f", round(p_female[["k"]], 2))),
      xfrac = xleft, yfrac = ytop + gap)
    add_label(label = paste0("linf = ", sprintf("%.1f", round(p_female[["linf"]], 1))),
      xfrac = xleft, yfrac = ytop + gap * 2)
    add_label(label = paste0("t0 = ", sprintf("%.2f", round(p_female[["t0"]], 2))),
      xfrac = xleft, yfrac = ytop + gap * 3)
  }

  if (fit_vb_male) {
    xleft <- xleft + 0.3
    add_label(label = "Males", xfrac = xleft, yfrac = ytop)
    add_label(label = paste0("k = ", sprintf("%.2f", round(p_male[["k"]], 2))),
      xfrac = xleft, yfrac = ytop + gap)
    add_label(label = paste0("linf = ", sprintf("%.1f", round(p_male[["linf"]], 1))),
      xfrac = xleft, yfrac = ytop + gap * 2)
    add_label(label = paste0("t0 = ", sprintf("%.2f", round(p_male[["t0"]], 2))),
      xfrac = xleft, yfrac = ytop + gap * 3)
  }

  if (fit_weight_female | fit_weight_male) {
    d_weight_plot <- d_weight
    if (nrow(d_weight) > 2000) d_weight_plot <- d_weight[sample(seq_len(nrow(d_weight)), 2000), ]

    with(d_weight_plot, plot(length, weight/1000, pch = 20, col = "#88888830", type = "p",
      axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i"))
    axis(1, las = 1, col = "grey60", col.axis = "grey40", col.ticks = "grey70")
    axis(2, las = 1, col = "grey60", col.axis = "grey40", col.ticks = "grey70")
  } else {
    plot(1, 1, ann = FALSE, axes = FALSE, type = "n")
  }

  if (fit_weight_male) {
    lines(nd$length, exp(nd$weight_male)/1000, col = "grey10", lwd = 3)
  }
  if (fit_weight_female) {
    lines(nd$length, exp(nd$weight_female)/1000, col = female_col, lwd = 3)
  }

  box(col = "grey60")
  mtext("Length (cm)", side = 1, col = "grey30", line = line, cex = 0.8)
  mtext("Weight (kg)", side = 2, col = "grey30", line = line, cex = 0.8)

  ytop <- 0.1
  xleft <- 0.04
  if (fit_weight_female) {
    add_label(label = "Females", xfrac = xleft, yfrac = ytop, col = female_col)
    add_label(label = paste0("a = ", sprintf("%.3f", round(exp(coef(m_weight_female)[[1]]), 3))),
      xfrac = xleft, yfrac = ytop + gap)
    add_label(label = paste0("b = ", sprintf("%.2f", round(coef(m_weight_female)[[2]], 2))),
      xfrac = xleft, yfrac = ytop + gap + gap)
  }

  xleft <- xleft + 0.3
  if (fit_weight_male) {
    add_label(label = "Males", xfrac = xleft, yfrac = ytop)
    add_label(label = paste0("a = ", sprintf("%.3f", round(exp(coef(m_weight_male)[[1]]), 3))),
      xfrac = xleft, yfrac = ytop + gap)
    add_label(label = paste0("b = ", sprintf("%.2f", round(coef(m_weight_male)[[2]], 2))),
      xfrac = xleft, yfrac = ytop + gap + gap)
  }

  dev.off()
}

# -------------
# maturity

mat_df <- tibble::tribble(
  ~maturity_convention_desc, ~mature_at,
  "ROCKFISH (1977+)",        3,
  "DOGFISH",                 77,
  "FLATFISH (1978+)",        3
)

dbio <- left_join(dbio, mat_df)


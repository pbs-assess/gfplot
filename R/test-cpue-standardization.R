## context("Commercial CPUE index standardization models work.")
##
## # d <- get_cpue_index(gear = "bottom trawl")
##
## # d <- readRDS("inst/pcod-cache/pbs-cpue-index.rds")
## #
## # walleye <- tidy_cpue_index(d, "walleye pollock",
## #   area_grep_pattern = "5[CDE]+"
## # )
## #
## library(dplyr)
##
##
## fake_fleet <- expand.grid(
##   year_factor = as.factor(seq(1996, 2010)),
##   vessel = rep(as.factor(as.character(seq(100, 120))), each = 75)
## )
##
## vessel_effects <- data.frame(
##   vessel = unique(fake_fleet$vessel),
##   vessel_effect = rnorm(length(unique(fake_fleet$vessel)), 0, 1)
## )
##
## year_effects <- data.frame(
##   year_factor = unique(fake_fleet$year),
##   year_effect = rnorm(length(unique(fake_fleet$year)), 0, 1)
## )
##
## fake_fleet <- inner_join(fake_fleet, vessel_effects, by = "vessel")
## fake_fleet <- inner_join(fake_fleet, year_effects, by = "year_factor")
##
## set.seed(1)
## sigma <- 0.3
##
## fake_fleet <- mutate(fake_fleet,
##   pos_catch = rbinom(nrow(fake_fleet), size = 1,
##     prob = plogis(vessel_effect + year_effect)),
##   spp_catch = ifelse(pos_catch == 1,
##     rlnorm(nrow(fake_fleet), meanlog = vessel_effect + year_effect,
##       sdlog = 0.3), 0),
##   hours_fished = 1
## )
##
## fake_fleet$year <- as.numeric(as.character(fake_fleet$year_factor))
##
## m <- fit_cpue_index(fake_fleet,
##   formula_binomial = pos_catch ~ year_factor + f(vessel),
##   formula_lognormal = log(spp_catch / hours_fished) ~ year_factor + f(vessel)
## )
##
## # out <- summary(m$sdreport)
## # labs <- row.names(out)
## # row.names(out) <- NULL
## # out <- as.data.frame(out)
## # out$labs <- labs
## #
##
## object <- m
## model_prefixes <- c("Bin.", "Pos.")
## sm <- summary(object$sdreport)
## pars <- row.names(sm)
## row.names(sm) <- seq_len(nrow(sm))
## sm <- as.data.frame(sm)
## sm$pars <- pars
##
## sm <- sm %>%
##   rename(se = .data$`Std. Error`) %>%
##   rename(est = .data$Estimate) %>%
##   filter(!grepl("prediction", pars))
## sm$par_name <- c(
##   paste(model_prefixes[[1]], colnames(object$mm_bin)),
##   paste(model_prefixes[[2]], colnames(object$mm_pos)),
##   "log_sigma"
## )
##
## intercept_bin <- sm[sm$par_name == "Bin. (Intercept)", "est"]
## intercept_pos <- sm[sm$par_name == "Pos. (Intercept)", "est"]
##
## intercept_bin <- year_effects$year_effect[[1]]
## intercept_pos <- year_effects$year_effect[[1]]
##
## b <- filter(sm, grepl("Bin\\. year*", par_name)) %>% pull(est)
## plot(intercept_bin + b, year_effects$year_effect[-1])
## abline(a = 0, b = 1)
##
## b <- filter(sm, grepl("Pos\\. year*", par_name)) %>% pull(est)
## plot(intercept_pos + b, year_effects$year_effect[-1])
## abline(a = 0, b = 1)
##
## intercept_bin_v <- vessel_effects$vessel_effect[[1]]
## intercept_pos_v <- vessel_effects$vessel_effect[[1]]
##
## b <- filter(sm, grepl("Bin\\. f\\(vessel*", par_name)) %>% pull(est)
## plot(intercept_bin_v + b, vessel_effects$vessel_effect[-1])
## abline(a = 0, b = 1)
##
## b <- filter(sm, grepl("Pos\\. f\\(vessel*", par_name)) %>% pull(est)
## plot(intercept_pos_v + b, vessel_effects$vessel_effect[-1])
## abline(a = 0, b = 1)
##
## sm$true_b <- c(NA,
##   year_effects$year_effect[-1],
##   vessel_effects$vessel_effect[-1],
##   NA,
##   year_effects$year_effect[-1],
##   vessel_effects$vessel_effect[-1],
##   sigma
##   )
##
## exp(sm$est[sm$par_name == "log_sigma"])
##
##
## library(ggplot2)
## ggplot(sm, aes(true_b, est)) + geom_point()
##
## # plot_cpue_index_coefs(m)
## #
## # predict_cpue_index(m) %>%
## #   plot_cpue_index()
## #
## # # plot_cpue_index_jk(m)


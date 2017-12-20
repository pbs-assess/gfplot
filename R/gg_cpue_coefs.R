gg_cpue_coefs <- function(sm, mm1, mm2, re = TRUE) {
  library(ggplot2)
  library(dplyr)
  pars <- row.names(sm)
  row.names(sm) <- 1:nrow(sm)
  sm <- as.data.frame(sm)
  sm$par <- pars
  sm <- group_by(sm, par) %>%
    mutate(par_num = 1:n()) %>%
    ungroup()
  # ii <- grep("b1_j", row.names(sm))

  if (re) {
    sm$par_name <- c(paste("bin", colnames(mm1)),
      paste("pos", colnames(mm2)),
      "log_sigma", "log_sigma_zk1", "log_sigma_zk2", "log_sigma_zg1", "log_sigma_zg2",
      paste("z1_k", seq_along(grep("z1_k", pars))),
      paste("z2_k", seq_along(grep("z2_k", pars))),
      paste("z1_g", seq_along(grep("z1_g", pars))),
      paste("z2_g", seq_along(grep("z2_g", pars))),
      paste("prediction", 1996:2015), paste("log-prediction", 1996:2015))
  } else {
    sm$par_name <- c(paste("bin", colnames(mm1)), paste("pos", colnames(mm2)),
      "log_sigma",
      paste("prediction", 1996:2015), paste("log-prediction", 1996:2015))
  }
  sm <- mutate(sm, par_name = gsub("as\\.factor\\(", "", par_name))
  sm <- mutate(sm, par_name = gsub("as\\.character\\(", "", par_name))

  sm$par_group <- substr(sm$par_name, 1, 15)
  if (re) {
    sm$par_group <- gsub(" [0-9]+$", "",  sm$par_group)
    sm$par_group <- gsub("_z[gk]+[0-9]+", "",  sm$par_group)
    sm$par_group <- gsub("z1_k", "bin vessel RE",  sm$par_group)
    sm$par_group <- gsub("z2_k", "pos vessel RE",  sm$par_group)
    sm$par_group <- gsub("z1_g", "bin locality RE",  sm$par_group)
    sm$par_group <- gsub("z2_g", "pos locality RE",  sm$par_group)
  }
  sm$par_name_short <- abbreviate(sm$par_name)
  sm$par_name_short <- gsub("zk", "ves_re",  sm$par_name_short)
  sm$par_name_short <- gsub("zg", "loc_re",  sm$par_name_short)
  sm$par_name_short <- gsub("lg_sgm", "log_sd",  sm$par_name_short)
  sm$par_name_short <- gsub("pp\\(", "poly\\(",  sm$par_name_short)
  sm$par_name_short <- gsub("bp\\(", "poly\\(",  sm$par_name_short)

  g <- filter(sm, !par %in% c("prediction")) %>%
    mutate(par_group = gsub("best_d", "depth", par_group)) %>%
    mutate(par_group = gsub("best_d", "depth", par_group)) %>%
    mutate(par_group = gsub("pos \\(Intercept\\)", "pos year_factor", par_group)) %>%
    mutate(par_group = gsub("bin \\(Intercept\\)", "bin year_factor", par_group)) %>%
    ggplot(aes(`Estimate`, par_name_short)) +
    geom_segment(aes(
      x = Estimate - 1.96 * `Std. Error`,
      xend = Estimate + 1.96 * `Std. Error`,
      y = par_name_short, yend = par_name_short
    )) +
    geom_point() +
    facet_wrap(~par_group, scales = "free") +
    geom_vline(xintercept = 0, lty = 2, col = "grey45")
  print(g)
}

#' Prepare commercial PBS CPUE data
#'
#' @param dat An input data frame from TODO
#' @param species_common The species common name
#' @param year_range The range of years to include
#' @param lat_range The range of latitudes to include
#' @param min_positive_tows The minimum number of positive tows over all years
#' @param min_annual_positive_trips The minimum number of annual positive trips
#' @param min_years_with_min_positive_trips The number of years in which the
#'   \code{min_annual_positive_trips} criteria needs to be met
#' @param area_grep_pattern A regular expression to extract the management areas
#'   of interest
#' @param lat_bands A sequence of latitude bans
#' @param depth_bands A sequence of depth bands
#' @param gear Gear types
#'
#' @family CPUE index functions
#'
#' @export
#'
# @examples
# prep_pbs_cpue_index(catch, species = "walleye pollock")
prep_pbs_cpue_index <- function(dat, species_common,
  year_range = c(1996, 2015),
  lat_range = c(48, Inf),
  min_positive_tows = 100,
  min_annual_positive_trips = 4,
  min_years_with_min_positive_trips = 4,
  area_grep_pattern = "5[CDE]+",
  lat_bands = seq(48, 60, 0.1),
  depth_bands = seq(50, 550, 25),
  gear = "BOTTOM TRAWL") {

  # areas and species are package data
  pbs_areas <- pbs_areas[grep(area_grep_pattern, pbs_areas$major_stat_area_description), ]
  names(catch) <- tolower(names(catch))
  catch <- inner_join(catch, pbs_species, by = "species_code")

  catch <- catch %>%
    inner_join(pbs_areas, by = "major_stat_area_code") %>%
    mutate(year = lubridate::year(best_date)) %>%
    filter(!fishing_event_id %in% c(0, 1)) %>% # bad events; many duplicates
    filter(year >= year_range[[1]] & year <= year_range[[2]]) %>%
    filter(!is.na(fe_start_date), !is.na(fe_end_date)) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    filter(gear %in% toupper(gear)) %>%
    filter(latitude >= lat_range[[1]] & latitude <= lat_range[[2]]) %>%
    mutate(month = lubridate::month(best_date)) %>%
    mutate(hours_fished =
        as.numeric(difftime(fe_end_date, fe_start_date, units = "hours"))) %>%
    filter(hours_fished > 0) %>%
    mutate(catch = landed_kg + discarded_kg) %>%
    group_by(fishing_event_id) %>%
    mutate(n_date = length(unique(best_date))) %>%
    filter(n_date < Inf) %>% select(-n_date) %>% # remove FE_IDs with multiple dates
    ungroup()

  catch <- group_by(catch, fishing_event_id, month, locality_code,
    vessel_name, year, trip_id, hours_fished) %>%
    mutate(
      spp_in_fe = toupper(species_common) %in% species_common_name,
      spp_in_row = species_common_name == toupper(species_common)) %>%
    summarise(
      pos_catch = ifelse(spp_in_fe[[1]], 1, 0),
      # hours_fished = mean(hours_fished, na.rm = TRUE),
      spp_catch = sum(ifelse(spp_in_row, catch, 0), na.rm = TRUE),
      best_depth = mean(best_depth, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE)) %>%
    ungroup()

  # figure out which vessels should be considered part of our fleet
  fleet <- catch %>%
    group_by(vessel_name) %>%
    mutate(total_positive_tows = sum(pos_catch)) %>%
    filter(total_positive_tows >= min_positive_tows) %>%
    filter(spp_catch > 0) %>%
    group_by(year, vessel_name, trip_id) %>%
    summarise(sum_catch = sum(spp_catch, na.rm = TRUE)) %>%
    filter(sum_catch > 0) %>%
    group_by(vessel_name) %>%
    mutate(n_years = length(unique(year))) %>%
    # for speed (filter early known cases):
    filter(n_years >= min_years_with_min_positive_trips) %>%
    group_by(year, vessel_name) %>%
    summarise(n_trips_per_year = length(unique(trip_id))) %>%
    mutate(trips_over_treshold_this_year =
        n_trips_per_year >= min_annual_positive_trips) %>%
    group_by(vessel_name) %>%
    summarise(trips_over_thresh = sum(trips_over_treshold_this_year)) %>%
    filter(trips_over_thresh >= min_years_with_min_positive_trips) %>%
    ungroup()

  # retain the data from our "fleet"
  d_retained <- dplyr::semi_join(catch, fleet, by = "vessel_name") %>%
    filter(best_depth >= min(depth_bands) & best_depth <= max(depth_bands)) %>%
    mutate(
      depth_band = factor_bin_clean(best_depth, depth_bands),
      vessel_name = as.factor(vessel_name),
      latitude_band = factor_bin_clean(latitude, lat_bands),
      dfo_locality = factor_clean(locality_code),
      year_factor = factor_clean(year),
      month_factor = factor_clean(month)) %>%
    mutate(pos_catch = ifelse(spp_catch > 0, 1, 0))

  # anonymize the vessels
  vessel_df <- dplyr::tibble(vessel_name = unique(d_retained$vessel_name),
    scrambled_vessel = factor_clean(seq_along(vessel_name)))
  d_retained <- left_join(d_retained, vessel_df, by = "vessel_name") %>%
    select(-vessel_name) %>%
    dplyr::rename(vessel_name = scrambled_vessel)

  d_retained <- arrange(d_retained, .data$year, .data$vessel_name)

  dplyr::as_tibble(d_retained)
}

factor_bin_clean <- function(x, bins, clean = TRUE) {
  out <- bins[findInterval(x, bins)]
  max_char <- max(nchar(out))
  ndec <- ndecimals(out)
  if (clean & ndec == 0)
    out <- sprintf(paste0("%0", max_char, "d"), out)
  if (clean & ndec > 0)
    out <- sprintf(paste0("%.", ndec, "f"), out)
  as.factor(out)
}

factor_clean <- function(x) {
  max_char <- max(nchar(x))
  as.factor(sprintf(paste0("%0", max_char, "d"), x))
}

ndecimals <- function(x) {
  out <- nchar(strsplit(as.character(x), "\\.")[[1]][2])
  if (is.na(out)) out <- 0
  out
}

# make prediction model matrix
make_pred_mm <- function(x, years) {
  mm_pred <- x[seq_along(years), ]
  for (i in 1:ncol(mm_pred)) {
    for (j in 1:nrow(mm_pred)) {
      mm_pred[j, i] <- 0
    }}
  mm_pred[,1] <- 1
  for (i in 1:ncol(mm_pred)) {
    for (j in 1:nrow(mm_pred)) {
      if (i == j)
        mm_pred[j, i] <- 1
    }}
  mm_pred
}

# Force factors to be sequential within the positive or binary data sets:
f <- function(x) as.factor(as.character(x))

#' Fit a delta-lognormal commercial CPUE standardization model
#'
#' @param dat A data frame from \code{\link{prep_pbs_cpue_index}}, or a similarly
#'   formatted data frame
#' @param formula_binomial Formula for the binomial model
#' @param formula_lognormal Formula for the lognormal model
#'
#' @export
#'
#' @importFrom stats coef model.matrix lm binomial rnorm
#' @family CPUE index functions

fit_cpue_index <- function(dat,
  formula_binomial = pos_catch ~ year_factor + f(month_factor) + f(vessel_name) +
    f(dfo_locality) + f(depth_band) + f(latitude_band),
  formula_lognormal = log(spp_catch/hours_fished) ~ year_factor +
    f(month_factor) + f(vessel_name) +
    f(dfo_locality) + f(depth_band) + f(latitude_band)) {

  tmb_cpp <- system.file("tmb", "deltalognormal.cpp", package = "PBSsynopsis")
  TMB::compile(tmb_cpp)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp)))

  pos_dat <- dat[dat$pos_catch == 1, , drop = FALSE]

  mm1 <- model.matrix(formula_binomial, data = dat)
  mm2 <- model.matrix(formula_lognormal, data = pos_dat)

  mm_pred2 <- make_pred_mm(mm2, years = unique(dat$year_factor))
  mm_pred1 <- make_pred_mm(mm1, years = unique(pos_dat$year_factor))

  # get some close starting values:
  m_bin <- speedglm::speedglm(formula_binomial, data = dat,
    family = binomial(link = "logit"))
  m_pos <- lm(formula_lognormal, data = pos_dat)

  message("Fitting CPUE model ...")
  obj <- TMB::MakeADFun(
    data = list(
      X1_ij = mm1, y1_i = dat$pos_catch,
      X2_ij = mm2,
      y2_i = log(pos_dat$spp_catch/pos_dat$hours_fished), # TODO take from formula
      X1_pred_ij = mm_pred1, X2_pred_ij = mm_pred2),
    parameters = list(
      b1_j = coef(m_bin) + rnorm(length(coef(m_bin)), 0, 0.001),
      b2_j = coef(m_pos) + rnorm(length(coef(m_pos)), 0, 0.001),
      # b1_j = rnorm(ncol(mm_pred1), 0, 0.2),
      # b2_j = rnorm(ncol(mm_pred2), 0, 0.2),
      log_sigma = log(summary(m_pos)$sigma) + rnorm(1, 0, 0.001)),
    # log_sigma = rnorm(1, 0, 0.2)),
    DLL = "deltalognormal")

  opt <- stats::nlminb(
    start = obj$par,
    objective = obj$fn,
    gradient = obj$gr, control = list(iter.max = 1000L,
      eval.max = 1000L))

  message("Getting sdreport ...")
  r <- TMB::sdreport(obj)

  list(model = opt, sdreport = r, max_gradient = max(obj$gr(opt$par)),
    years = sort(unique(dat$year)), mm_bin = mm1, mm_pos = mm2)
}

#' Tidy a delta-lognormal commercial CPUE standardization model
#'
#' @param object A model object from \code{\link{fit_cpue_index}}
#' @param center Should the index be centered by subtracting the mean in log space?
#'
#' @export
#' @family CPUE index functions
#'
tidy_cpue_index <- function(object, center = TRUE) {
  report_sum <- summary(object$sdreport)
  ii <- grep("log_prediction", row.names(report_sum))
  row.names(report_sum) <- NULL
  df <- as.data.frame(report_sum[ii, ])
  df$year <- object$years

  if (center)
    df$Estimate <- df$Estimate - mean(df$Estimate)

  df %>% dplyr::rename(se_log = .data$`Std. Error`) %>%
    dplyr::rename(est_log = .data$Estimate) %>%
    mutate(
      lwr = exp(est_log - 1.96 * se_log),
      upr = exp(est_log + 1.96 * se_log),
      est = exp(est_log)) %>%
    select(year, est_log, se_log, est, lwr, upr)
}

#' Plot a delta-lognormal commercial CPUE standardization model
#'
#' @param dat Input data frame, for example from \code{\link{tidy_cpue_index}}
#'
#' @export
#' @family CPUE index functions
#'
plot_cpue_index <- function(dat) {
  ggplot(dat, aes_string("year", "est", ymin = "upr", ymax = "lwr")) +
    ggplot2::geom_ribbon(alpha = 0.5) +
    geom_line() +
    theme_pbs() +
    labs(y = "CPUE index", x = "")
}

# jackknife_cpue <- function(f_bin, f_pos, terms) {
#
#   mm1 <- model.matrix(f_bin, data = d_retained)
#   mm2 <- model.matrix(f_pos, data = subset(d_retained, pos_catch == 1))
#   mm1 <- make_pred_mm(mm1)
#   mm2 <- make_pred_mm(mm2)
#
#   m_bin <- speedglm::speedglm(f_bin, data = d_retained,
#     family = binomial(link = "logit"))
#   m_pos <- lm(f_pos, data = subset(d_retained, pos_catch == 1))
#
#   p1 <- plogis(mm1 %*% coef(m_bin))
#   p2 <- exp(mm2 %*% coef(m_pos))
#   full <- data.frame(year = 1996:2015, term = "all", pred = p1 * p2)
#
#   fitm <- function(drop_term) {
#     f1 <- update.formula(formula(m_bin), as.formula(paste0(". ~ . -", drop_term)))
#     f2 <- update.formula(formula(m_pos), as.formula(paste0(". ~ . -", drop_term)))
#     mm1 <- model.matrix(f1, data = d_retained)
#     mm2 <- model.matrix(f2, data = subset(d_retained, pos_catch == 1))
#     mm1 <- make_pred_mm(mm1)
#     mm2 <- make_pred_mm(mm2)
#     m_bin <- speedglm::speedglm(f1, data = d_retained,
#       family = binomial(link = "logit"))
#     m_pos <- lm(f2, data = subset(d_retained, pos_catch == 1))
#     p1 <- plogis(mm1 %*% coef(m_bin))
#     p2 <- exp(mm2 %*% coef(m_pos))
#     o <- data.frame(year = 1996:2015, term = drop_term, pred = p1 * p2)
#     o
#   }
#
#   out <- plyr::ldply(terms, fitm)
#   suppressWarnings(out <- bind_rows(full, out))
#   out <- group_by(out, term) %>% mutate(pred = pred / exp(mean(log(pred)))) %>%
#     ungroup()
#   out
# }

# jk <- jackknife_cpue(f1, f2, terms = c("f(depth_band)", "f(vessel_name)",
# "f(latitude_band)", "f(dfo_locality)", "f(month_factor)"))

#' Plot coefficients from a CPUE index standardization model
#'
#' @param object Model output from \code{\link{fit_cpue_index}}
#' @param coef_sub A named character vector of any substitutions to make on the
#'   coefficient names. Can be used to abbreviate coefficient names for the
#'   plot.
#' @param model_prefixes Text to distinguish the binary and positive component
#'   model coefficients
#'
#' @details Note that the coefficients for predictors treated as factors (i.e.
#'   likely all of the predictors), the coefficients represent the difference
#'   from the base level factor, which would be the first factor level
#'   alpha-numericaly. For example, months 02 to 12 represent the estimated
#'   difference between that month and month 01.
#'
#' @return A ggplot
#' @export
#'
#' @family CPUE index functions

plot_coefs_cpue_index <- function(object,
  coef_sub = c(
    "depth_band" = "depth",
    "dfo_locality" = "locality",
    "vessel_name" = "vessel",
    "month_factor" = "month",
    "latitude_band" = "latitude"),
  model_prefixes = c("Bin.", "Pos.")) {

  sm <- summary(object$sdreport)
  pars <- row.names(sm)
  row.names(sm) <- seq_len(nrow(sm))
  sm <- as.data.frame(sm)
  sm$pars <- pars
  sm <- sm %>% dplyr::rename(se = .data$`Std. Error`) %>%
    dplyr::rename(est = .data$Estimate)
  sm$par_name <- c(
    paste(model_prefixes[[1]], colnames(object$mm_bin)),
    paste(model_prefixes[[2]], colnames(object$mm_pos)),
    "log_sigma",
    paste("log-prediction", object$years))

  for (i in seq_along(coef_sub))
    sm$par_name <- sub(names(coef_sub)[i], coef_sub[[i]], sm$par_name)

  sm$par_name <- sub("f\\(", "", sm$par_name)
  sm$par_name <- sub("\\)", "", sm$par_name)
  sm$par_group <- sub("[0-9.]+$", "", sm$par_name)
  sm$par_name <- sub("([0-9.]+$)", " \\1", sm$par_name)
  sm$par_group <- forcats::fct_relevel(sm$par_group , "log_sigma", after = Inf)
  sm <- mutate(sm, se_too_big = se > 10, se = ifelse(se > 10, NA, se))

  filter(sm, !pars %in% c("prediction")) %>%
    filter(!grepl("Intercept", par_name)) %>%
    filter(!grepl("log-prediction", par_name)) %>%
    filter(!grepl("year", par_group)) %>%
    ggplot(aes_string("est", "forcats::fct_rev(par_name)",
      yend = "forcats::fct_rev(par_name)")) +
    geom_vline(xintercept = 0, lty = 2, col = "grey65") +
    ggplot2::geom_segment(aes_string(x = "est - 1.96 * se",
      xend = "est + 1.96 * se"), col = "grey30", lwd = 0.5) +
    ggplot2::geom_segment(aes_string(x = "est - 0.67 * se",
      xend = "est + 0.67 * se"), col = "grey10", lwd = 1.25) +
    geom_point(aes_string(shape = "se_too_big"), bg = "grey95", col = "grey10") +
    ggplot2::scale_shape_manual(values = c("TRUE" = 4, "FALSE" = 21)) +
    facet_wrap(~par_group, scales = "free") +
    theme_pbs() + guides(shape = FALSE) +
    ggplot2::labs(y = "", x = "Coefficient value")
}

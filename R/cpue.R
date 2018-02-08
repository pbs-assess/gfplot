#' Tidy commercial PBS CPUE data
#'
#' @param dat An input data frame from \code{\link{get_pbs_cpue_index}}
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
#' @family tidy data functions
#'
#' @export
#'
# @examples
# tidy_pbs_cpue_index(catch, species = "walleye pollock")
tidy_pbs_cpue_index <- function(dat, species_common,
  year_range = c(1996, Inf),
  lat_range = c(48, Inf),
  min_positive_tows = 100,
  min_annual_positive_trips = 4,
  min_years_with_min_positive_trips = 4,
  area_grep_pattern = "5[CDE]+",
  lat_bands = seq(48, 59, 0.1),
  depth_bands = seq(50, 450, 25),
  gear = "BOTTOM TRAWL") {

  pbs_areas <- PBSsynopsis::pbs_areas[grep(area_grep_pattern,
    PBSsynopsis::pbs_areas$major_stat_area_description), ]
  names(catch) <- tolower(names(catch))
  catch <- inner_join(catch, PBSsynopsis::pbs_species, by = "species_code")

  # basic filtering:
  catch <- catch %>%
    inner_join(pbs_areas, by = "major_stat_area_code") %>%
    mutate(year = lubridate::year(best_date)) %>%
    filter(!fishing_event_id %in% c(0, 1)) %>% # bad events; many duplicates?
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

  # catch for target spp:
  catch <- group_by(catch, fishing_event_id, best_date, month, locality_code,
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

  # figure out which vessels should be considered part of the spp. fleet
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
      depth = factor_bin_clean(best_depth, depth_bands),
      vessel = as.factor(vessel_name),
      latitude = factor_bin_clean(latitude, lat_bands),
      locality = factor_clean(locality_code),
      year_factor = factor_clean(year),
      month = factor_clean(month)) %>%
    mutate(pos_catch = ifelse(spp_catch > 0, 1, 0))

  # anonymize the vessels
  vessel_df <- dplyr::tibble(vessel = unique(d_retained$vessel),
    scrambled_vessel = factor_clean(seq_along(vessel)))
  d_retained <- left_join(d_retained, vessel_df, by = "vessel") %>%
    select(-vessel) %>%
    dplyr::rename(vessel = scrambled_vessel)

  arrange(d_retained, .data$year, .data$vessel) %>%
    dplyr::as_tibble()
}

factor_bin_clean <- function(x, bins, clean = TRUE) {
  out <- bins[findInterval(x, bins)]
  max_char <- max(nchar(out))
  ndec <- ndecimals(out)
  if (clean & ndec == 0)
    out <- sprintf(paste0("%0", max_char, "d"), out) # pad with zeros
  if (clean & ndec > 0)
    out <- sprintf(paste0("%.", ndec, "f"), out) # pad after decimal
  as.factor(out)
}

factor_clean <- function(x) {
  max_char <- max(nchar(x))
  ndec <- ndecimals(x)
  if (ndec == 0)
    out <- sprintf(paste0("%0", max_char, "d"), x) # pad with zeros
  if (ndec > 0)
    out <- sprintf(paste0("%.", ndec, "f"), x) # pad after decimal
  as.factor(out)
}

ndecimals <- function(x) {
  out <- nchar(strsplit(as.character(x), "\\.")[[1]][2])
  if (is.na(out)) out <- 0
  out
}

# make prediction [m]odel [m]atrix
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

# Force factors to be sequential within the positive or binary data sets for TMB:
f <- function(x) as.factor(as.character(x))

#' Fit a delta-lognormal commercial CPUE standardization model
#'
#' @param dat A data frame from \code{\link{tidy_pbs_cpue_index}}, or a similarly
#'   formatted data frame
#' @param formula_binomial Formula for the binomial model
#' @param formula_lognormal Formula for the lognormal model
#'
#' @export
#'
#' @importFrom stats coef model.matrix lm binomial rnorm
#' @family CPUE index functions
#' @family Fitting functions

fit_cpue_index <- function(dat,
  formula_binomial = pos_catch ~ year_factor + f(month) + f(vessel) +
    f(locality) + f(depth) + f(latitude),
  formula_lognormal = log(spp_catch/hours_fished) ~ year_factor +
    f(month) + f(vessel) +
    f(locality) + f(depth) + f(latitude)) {

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
    years = sort(unique(dat$year)), mm_bin = mm1, mm_pos = mm2,
    f_bin = formula_binomial, f_pos = formula_lognormal,
    data = dat)
}

#' Tidy a delta-lognormal commercial CPUE standardization model
#'
#' @param object A model object from \code{\link{fit_cpue_index}}
#' @param center Should the index be centered by subtracting the mean in link space?
#'
#' @export
#' @family CPUE index functions

predict_cpue_index <- function(object, center = FALSE) {

  report_sum <- summary(object$sdreport)
  ii <- grep("log_prediction|linear_prediction1_i|linear_prediction2_i",
    row.names(report_sum))
  row.names(report_sum) <- NULL
  df <- as.data.frame(report_sum[ii, , drop = FALSE])
  df$year <- rep(object$years, 3L)
  df$model <- rep(c("Combined", "Binomial", "Lognormal"), each = nrow(df)/3L)
  df$model <- factor(df$model, levels = c("Combined", "Binomial", "Lognormal"))

  if (center) {
    df <- df %>% group_by(model) %>%
      mutate(Estimate = .data$Estimate - mean(.data$Estimate)) %>%
      ungroup()
  }

  df %>% dplyr::rename(se_link = .data$`Std. Error`) %>%
    dplyr::rename(est_link = .data$Estimate) %>%
    mutate(
      lwr = ifelse(model == "Binomial",
        plogis(est_link - 1.96 * se_link),
        exp(est_link - 1.96 * se_link)),
      upr = ifelse(model == "Binomial",
        plogis(est_link - 1.96 * se_link),
        exp(est_link - 1.96 * se_link)),
      est = ifelse(model == "Binomial",
        plogis(est_link),
        exp(est_link))) %>%
    select(year, model, est_link, se_link, est, lwr, upr)
}

#' Plot a delta-lognormal commercial CPUE standardization model
#'
#' @param dat Input data frame, for example from \code{\link{predict_cpue_index}}
#' @param all_models TODO
#'
#' @export
#' @family CPUE index functions
#' @family plotting functions
#' @return A ggplot object

plot_cpue_index <- function(dat, all_models = TRUE) {

  if (!all_models)
    dat <- filter(dat, model == "Combined")

  g <- ggplot(dat, aes_string("year", "est", ymin = "upr", ymax = "lwr")) +
    ggplot2::geom_ribbon(alpha = 0.5) +
    geom_line() +
    theme_pbs() +
    labs(y = "CPUE index", x = "") +
    ylim(0, NA)
  if (all_models)
    g <- g + facet_wrap(~model, scales = "free_y")

  g
}

#' Plot coefficients from a CPUE index standardization model
#'
#' @param object Model output from \code{\link{fit_cpue_index}}
#'
#' @details Note that the coefficients for predictors treated as factors (i.e.
#'   likely all of the predictors), the coefficients represent the difference
#'   from the base level factor, which would be the first factor level
#'   alpha-numericaly. For example, month 02 represents the estimated
#'   difference between February and January.
#'
#' @return A ggplot object
#' @export
#' @family CPUE index functions
#' @family plotting functions

plot_cpue_index_coefs <- function(object) {

  model_prefixes <- c("Bin.", "Pos.")

  sm <- summary(object$sdreport)
  pars <- row.names(sm)
  row.names(sm) <- seq_len(nrow(sm))
  sm <- as.data.frame(sm)
  sm$pars <- pars

  sm <- sm %>% dplyr::rename(se = .data$`Std. Error`) %>%
    dplyr::rename(est = .data$Estimate) %>%
    filter(!grepl("prediction", pars))
  sm$par_name <- c(
    paste(model_prefixes[[1]], colnames(object$mm_bin)),
    paste(model_prefixes[[2]], colnames(object$mm_pos)),
    "log_sigma")
  sm <- sm %>% filter(!grepl("Intercept", par_name)) %>%
    filter(!grepl("year", par_name))

  sm$par_name <- sub("f\\(", "", sm$par_name)
  sm$par_name <- sub("\\)", "", sm$par_name)
  sm$par_group <- sub("[0-9.]+$", "", sm$par_name)
  sm$par_name <- sub("([0-9.]+$)", " \\1", sm$par_name)
  sm$par_group <- forcats::fct_relevel(sm$par_group , "log_sigma", after = Inf)
  sm <- mutate(sm, se_too_big = se > 10, se = ifelse(se > 10, NA, se))
  sm <- mutate(sm, type = grepl(model_prefixes[[1]], par_group))

  cols <- RColorBrewer::brewer.pal(3, "Blues")

  ggplot(sm, aes_string("est", "forcats::fct_rev(par_name)",
    yend = "forcats::fct_rev(par_name)", colour = "type")) +
    # geom_vline(xintercept = 0, lty = 2, col = "grey65") +
    ggplot2::geom_segment(aes_string(x = "est - 1.96 * se",
      xend = "est + 1.96 * se"), lwd = 0.5) +
    ggplot2::geom_segment(aes_string(x = "est - 0.67 * se",
      xend = "est + 0.67 * se"), lwd = 1.25) +
    geom_point(aes_string(shape = "se_too_big"), bg = "white") +
    ggplot2::scale_shape_manual(values = c("TRUE" = 4, "FALSE" = 21)) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "grey30", "FALSE" = cols[[3]])) +
    facet_wrap(~par_group, scales = "free") +
    theme_pbs() + guides(shape = FALSE, colour = FALSE) +
    ggplot2::labs(y = "", x = "Coefficient value")
}

#' Title
#'
#' @param object TODO
#' @param terms TODO
#'
#' @return TODO
#' @export
#'
#' @family CPUE index functions
#' @family plotting functions

jackknife_cpue_index <- function(object,
  terms = c("f(month)", "f(vessel)", "f(locality)",
    "f(depth)", "f(latitude)")){
  pos_dat <- object$data[object$data$pos_catch == 1, , drop = FALSE]

  mm1 <- model.matrix(pos_catch ~ year_factor, data = object$data)
  mm2 <- model.matrix(log(spp_catch/hours_fished) ~ year_factor, data = pos_dat)
  mm1 <- make_pred_mm(mm1, years = m$years)
  mm2 <- make_pred_mm(mm2, years = m$years)
  m_bin <- speedglm::speedglm(pos_catch ~ year_factor, data = object$data,
    family = binomial(link = "logit"))
  m_pos <- lm(log(spp_catch/hours_fished) ~ year_factor, data = pos_dat)
  p1 <- plogis(mm1 %*% coef(m_bin))
  p2 <- exp(mm2 %*% coef(m_pos))
  none <- data.frame(year = object$years, term = "none", pred = p1 * p2,
    stringsAsFactors = FALSE)

  mm1 <- model.matrix(object$f_bin, data = object$data)
  mm2 <- model.matrix(object$f_pos, data = pos_dat)
  mm1 <- make_pred_mm(mm1, years = m$years)
  mm2 <- make_pred_mm(mm2, years = m$years)
  m_bin <- speedglm::speedglm(object$f_bin, data = object$data,
    family = binomial(link = "logit"))
  m_pos <- lm(object$f_pos, data = pos_dat)
  p1 <- plogis(mm1 %*% coef(m_bin))
  p2 <- exp(mm2 %*% coef(m_pos))
  full <- data.frame(year = object$years, term = "all", pred = p1 * p2,
    stringsAsFactors = FALSE)

  fitm <- function(drop_term) {
    message(paste("Dropping", drop_term, "..."))
    f1 <- update.formula(formula(m_bin), as.formula(paste0(". ~ . -", drop_term)))
    f2 <- update.formula(formula(m_pos), as.formula(paste0(". ~ . -", drop_term)))
    mm1 <- model.matrix(f1, data = object$data)
    mm2 <- model.matrix(f2, data = pos_dat)
    mm1 <- make_pred_mm(mm1, years = m$years)
    mm2 <- make_pred_mm(mm2, years = m$years)
    m_bin <- speedglm::speedglm(f1, data = object$data,
      family = binomial(link = "logit"))
    m_pos <- lm(f2, data = pos_dat)
    p1 <- plogis(mm1 %*% coef(m_bin))
    p2 <- exp(mm2 %*% coef(m_pos))
    o <- data.frame(year = object$years, term = drop_term, pred = p1 * p2,
      stringsAsFactors = FALSE)
    o
  }

  out <- lapply(terms, fitm)
  out <- do.call("rbind", out)
  out <- dplyr::bind_rows(none, full) %>% dplyr::bind_rows(out)
  out <- group_by(out, term) %>%
    mutate(pred = pred / exp(mean(log(pred)))) %>%
    ungroup()

  out$term <- sub("f\\(", "", out$term)
  out$term <- sub("\\)", "", out$term)
  out$term <- paste("Without", out$term)

  all <- filter(out, term == "Without all") %>% select(-term)
  out$term <- sub("Without none", "Unstandardized", out$term)

  ggplot(filter(out, term != "Without all"),
    aes_string("year", "pred", colour = "term")) +
    geom_line(data = all, lty = "31", lwd = 1.0,
      aes_string("year", "pred"), colour = "grey70", inherit.aes = FALSE) +
    geom_line(lwd = 1.0) +
    ggplot2::scale_color_manual(values = cols) +
    theme_pbs() + guides(size = FALSE, colour = FALSE) +
    labs(colour = "Excluded", y = "Relative CPUE index", x = "") +
    facet_wrap(~term)
}

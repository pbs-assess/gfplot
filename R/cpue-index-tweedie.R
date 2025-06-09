load_tmb <- function(file = "tweedie_cpue.cpp") {
  tmb_cpp <- system.file("tmb", file, package = "gfplot")
  TMB::compile(tmb_cpp)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp)))
}

#' Fit CPUE with glmmTMB
#'
#' @param dat Data
#' @param formula Formula
#' @param ... Other arguments to [glmmTMB::glmmTMB()]
#'
#' @export
fit_cpue_index_glmmtmb <- function(dat, formula = cpue ~ year_factor, ...) {
  glmmTMB::glmmTMB(as.formula(formula), data = dat, family = glmmTMB::tweedie(link = "log"),
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 2000, eval.max = 2000),
      profile = TRUE, collect = FALSE), ...)
}

#' @param object Output from [fit_cpue_index_glmmtmb()].
#' @param center Should the predictions be centered by dividing by the geometric
#'   mean?
#'
#' @export
#' @rdname fit_cpue_index_glmmtmb
predict_cpue_index_tweedie <- function(object, center = FALSE) {
  # Following causes NOTE in check()
  if (class(object) != "glmmTMB") stop("Class of `object` must be glmmTMB.")
    sdr <- object$sdr
    yrs <- sort(unique(object$frame$year_factor))

    b <- data.frame(par = names(sdr$par.fixed), est = sdr$par.fixed, se = sqrt(diag(sdr$cov.fixed)))
    b <- b[grepl("^beta$", b$par), , drop = FALSE]
    b <- b[seq_along(yrs), , drop = FALSE]
    b$year <- as.numeric(as.character(yrs))
    b$par <- NULL
    b$se_year <- NA
    b$se_year[1] <- b$se[1]
    b$est_year[1] <- b$est[1]
    df <- data.frame(year = b$year, Estimate = b$est,
      `Std. Error` = b$se, model = "Combined", stringsAsFactors = FALSE)
    names(df) <- gsub("Std..Error", "Std. Error", names(df))

  if (center) {
    df <- df %>%
      group_by(model) %>%
      mutate(Estimate = .data$Estimate - mean(.data$Estimate)) %>%
      ungroup()
  }

  df %>%
    rename(se_link = .data$`Std. Error`) %>%
    rename(est_link = .data$Estimate) %>%
    mutate(
      lwr = ifelse(model == "Binomial",
        plogis(est_link - 1.96 * se_link),
        exp(est_link - 1.96 * se_link)
      ),
      upr = ifelse(model == "Binomial",
        plogis(est_link + 1.96 * se_link),
        exp(est_link + 1.96 * se_link)
      ),
      est = ifelse(model == "Binomial",
        plogis(est_link),
        exp(est_link)
      )
    ) %>%
    select(year, model, est_link, se_link, est, lwr, upr)
}

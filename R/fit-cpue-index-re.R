# Random effect version:
fct_to_tmb_num <- function(x) {
  as.numeric(as.factor(as.character(x))) - 1
}

load_tmb_2re_cpue <- function() {
  tmb_cpp_2re <- system.file("tmb", "deltalognormal_2re.cpp", package = "gfplot")
  TMB::compile(tmb_cpp_2re)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp_2re)))
}

load_tmb_1re_cpue <- function() {
  tmb_cpp_1re <- system.file("tmb", "deltalognormal_1re.cpp", package = "gfplot")
  TMB::compile(tmb_cpp_1re)
  dyn.load(TMB::dynlib(sub("\\.cpp", "", tmb_cpp_1re)))
}

parse_formula <- function(f) {
  terms_ <- attributes(terms(f))$term.labels
  re <- terms_[grepl("1 \\|", terms_)]
  re <- gsub("1 \\| ", "", re)
  fe <- terms_[!grepl("1 \\|", terms_)]
  response <- all.vars(terms(f))[1]
  fe <- as.formula(paste(response, "~", paste(fe, collapse = " + ")))
  list(response = response, fe = fe, re = re)
}

fit_cpue_index_re <- function(dat,
                              formula_binomial = pos_catch ~ year_factor,
                              formula_lognormal = log(spp_catch / hours_fished) ~ year_factor) {

  f_bin <- parse_formula(formula_binomial)
  f_pos <- parse_formula(formula_lognormal)

  if (!identical(f_bin$re, f_pos$re))
    stop("Random effects for the binomial and positive components must be identical.")

  re1 <- NULL
  re2 <- NULL
  if (length(f_bin$re) == 1L) {
    re1 <- f_bin$re[[1]]
  }
  if (length(f_bin$re) == 2L) {
    re1 <- f_bin$re[[1]]
    re2 <- f_bin$re[[2]]
  }

  pos_dat <- dat[dat$pos_catch == 1, , drop = FALSE]

  if (!is.null(re1)) {
    bin_re_id_k <- fct_to_tmb_num(dat[[re1]])
    pos_re_id_k <- fct_to_tmb_num(filter(dat, pos_catch == 1)[[re1]])
  }
  if (!is.null(re2)) {
    bin_re_id_g <- fct_to_tmb_num(dat[[re2]])
    pos_re_id_g <- fct_to_tmb_num(filter(dat, pos_catch == 1)[[re2]])
  }
  mm1 <- model.matrix(f_bin$fe, data = dat)
  mm2 <- model.matrix(f_pos$fe, data = pos_dat)
  mm_pred1 <- make_pred_mm(mm1, years = unique(pos_dat$year_factor))
  mm_pred2 <- make_pred_mm(mm2, years = unique(dat$year_factor))

  dlls <- getLoadedDLLs()
  if (!is.null(re1) && !is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "deltalognormal_2re", FUN.VALUE = TRUE))) {
    load_tmb_2re_cpue()
  }
  if (!is.null(re1) && is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "deltalognormal_1re", FUN.VALUE = TRUE))) {
    load_tmb_1re_cpue()
  }
  if (is.null(re1) && is.null(re2) && !any(vapply(dlls, function(x)
    x[["name"]] == "deltalognormal", FUN.VALUE = TRUE))) {
    load_tmb_cpue()
  }

  # defaults if no random effects:
  random <- NULL
  DLL <- "deltalognormal"

  parameters <- list(
    b1_j = stats::rnorm(ncol(mm1), 0, 0.5),
    b2_j = stats::rnorm(ncol(mm2), 0, 0.5),
    log_sigma = log(1.0))

  data <- list(
    X1_ij = mm1,
    y1_i = dat$pos_catch,
    X2_ij = mm2,
    y2_i = log(pos_dat$spp_catch / pos_dat$hours_fished),
    X1_pred_ij = mm_pred1,
    X2_pred_ij = mm_pred2
  )

  if (!is.null(re1)) {
    data$factor1k_i <- bin_re_id_k
    data$factor2k_i <- pos_re_id_k
    data$nk1 <- length(unique(bin_re_id_k))
    data$nk2 <- length(unique(pos_re_id_k))
    random <- c("z1_k", "z2_k")
    DLL <- "deltalognormal_1re"
    parameters$z1_k <- stats::rnorm(length(unique(bin_re_id_k)), 0, 0.2)
    parameters$z2_k <- stats::rnorm(length(unique(pos_re_id_k)), 0, 0.2)
    parameters$log_sigma_zk1 <- log(0.2)
    parameters$log_sigma_zk2 <- log(0.2)
  }

  if (!is.null(re2)) {
    data$factor1g_i <- bin_re_id_g
    data$factor2g_i <- pos_re_id_g
    data$ng1 <- length(unique(bin_re_id_g))
    data$ng2 <- length(unique(pos_re_id_g))
    random <- c(random, "z1_g", "z2_g")
    DLL <- "deltalognormal_2re"
    parameters$z1_g <- stats::rnorm(length(unique(bin_re_id_g)), 0, 0.2)
    parameters$z2_g <- stats::rnorm(length(unique(pos_re_id_g)), 0, 0.2)
    parameters$log_sigma_zg1 <- log(0.2)
    parameters$log_sigma_zg2 <- log(0.2)
  }

  obj <- TMB::MakeADFun(
    data = data,
    parameters = parameters,
    random = random,
    DLL = DLL,
    checkParameterOrder = FALSE
  )

  opt <- stats::nlminb(
    start = obj$par,
    objective = obj$fn,
    gradient = obj$gr,
    control = list(iter.max = 1000L, eval.max = 1000L)
  )

  message("Getting TMB::sdreport()")
  r <- TMB::sdreport(obj)

  out <- list(
    model = opt, sdreport = r, max_gradient = max(obj$gr(opt$par)),
    years = sort(unique(dat$year)), mm_bin = mm1, mm_pos = mm2,
    f_bin = formula_binomial, f_pos = formula_lognormal,
    data = dat, parameters = parameters, random = random,
    DLL = DLL, re1_bin = NA, re1_pos = NA, re2_bin = NA, re2_pos = NA,
    re1 = NA, re2 = NA
  )
  if (!is.null(re1)) {
    out$re1_bin <- dat[[re1]]
    out$re1_pos <- dat[[re1]]
    out$re1 <- re1
  }
  if (!is.null(re2)) {
    out$re2_bin <- dat[[re2]]
    out$re2_pos <- dat[[re2]]
    out$re2 <- re2
  }

  out
}

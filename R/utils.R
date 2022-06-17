force_three_letter_species_code <- function(x) {
  if (is.numeric(x)) {
    sprintf(paste0("%0", 3L, "d"), x)
  } else {
    as.character(x)
  }
}

all_species_codes <- function(x) {
  all(grepl("[0-9]+", x))
}

first_cap <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
      {
        s <- substring(s, 2)
        if (strict) tolower(s) else s
      },
      sep = "",
      collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

round_any <- function(x, accuracy) {
  round(x / accuracy) * accuracy
}

round_nice <- function(x, thousands_k = TRUE) {
  out <- round_any(x, 100)
  out[out == 0] <- x[out == 0]
  if (thousands_k) {
    out <- as.numeric(out)
    out <- ifelse(out >= 1000, numform::f_thous(out, relative = 0L), out)
    # out <- gsub("\\.0K", "K", out)
  }
  out[x == 0] <- ""
  out
}

mround <- function(x, base) {
  base * round(x / base)
}

round_down_even <- function(x, base = 2) {
  base * floor(x / base)
}

round_up_even <- function(x, base = 2) {
  base * ceiling(x / base)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_dfo <- function() {
  grepl("PBS", Sys.info()[["nodename"]])
}

# is_ip_valid <- function(timeout = 5) {
#   pbs_ip <- getOption("pbs.ip")
#   if (is.null(pbs_ip)) return(FALSE)
#   out <- pingr::ping(pbs_ip, verbose = FALSE,
#     count = 1L, timeout = timeout)
#   !is.na(out)
# }
#
# # Is this a DFO Windows computer or is the server IP accessible?
# sql_server_accessible <- function() {
#   if ((is_windows() && is_dfo()) || is_ip_valid()) TRUE else FALSE
# }

factor_bin_clean <- function(x, bins, clean = TRUE) {
  out <- bins[findInterval(x, bins, rightmost.closed = TRUE)]
  max_char <- max(nchar(out))
  ndec <- ndecimals(out)
  if (clean & ndec == 0) {
    out <- sprintf(paste0("%0", max_char, "d"), out)
  } # pad with zeros
  if (clean & ndec > 0) {
    out <- sprintf(paste0("%.", ndec, "f"), out)
  } # pad after decimal
  as.factor(out)
}

#' Assign areas
#'
#' @param major_stat_area_description A vector of major statistical area
#'   descriptions.
#' @param area_regex A vector of regular expressions describing the areas group.
#' @export
#' @examples
#' x <- c("5D: NORTHERN HECATE STRAIT", "3C: S.W. VANCOUVER ISLAND", "3D: N.W. VANCOUVER ISLAND")
#' assign_areas(x)
assign_areas <- function(major_stat_area_description,
                         area_regex = c("3[CD]+", "5[AB]+", "5[CDE]+")) {
  out <- rep(NA, length(major_stat_area_description))
  for (i in seq_along(area_regex)) {
    out[grepl(area_regex[i], major_stat_area_description)] <-
      gsub("\\^|\\[|\\]|\\+", "", area_regex[i])
  }
  out
}

factor_clean <- function(x) {
  max_char <- max(nchar(x))
  ndec <- ndecimals(x)
  if (ndec == 0) {
    out <- sprintf(paste0("%0", max_char, "d"), x)
  } # pad with zeros
  if (ndec > 0) {
    out <- sprintf(paste0("%.", ndec, "f"), x)
  } # pad after decimal
  as.factor(out)
}

ndecimals <- function(x) {
  ndec <- nchar(lapply(strsplit(as.character(x), "\\."), function(x) x[2]))
  if (!all(is.na(ndec))) {
    ndec <- max(ndec, na.rm = TRUE)
  } else {
    ndec <- 0
  }
  ndec
}

# make prediction [m]odel [m]atrix
make_pred_mm <- function(x, years) {
  mm_pred <- x[seq_along(years), ]
  for (i in 1:ncol(mm_pred)) {
    for (j in 1:nrow(mm_pred)) {
      mm_pred[j, i] <- 0
    }
  }
  mm_pred[, 1] <- 1
  for (i in 1:ncol(mm_pred)) {
    for (j in 1:nrow(mm_pred)) {
      if (i == j) {
        mm_pred[j, i] <- 1
      }
    }
  }
  mm_pred
}

logit_perc <- function(a, b, perc = 0.5) {
  -(log((1 / perc) - 1) + a) / b
}

#' Set the year in the data frame to be the fishing year as defined between the
#' month and day given
#'
#' @rdname plot_catch
#' @param month_fishing_starts The month in which the fishing year starts and
#'   ends. The `year` column of the returned data frame will refer to the
#'   fishing year as determined by this and the `day_fishing_starts` argument.
#' @param day_fishing_starts The day of the month in which the fishing year
#'   starts and ends. See `month_fishing_starts`
#' @param yr_col Name of the column in `dat` holding the fishing year data
#' @param date_col Name of the column in `dat` holding the date data
#' @param ... Absorb unused parameters
#'
#' @return The data frame `dat` with modified year data
#' @export
#' @examples
#' \dontrun{
#' d <- gfdata::get_catch("arrowtooth flounder")
#' d <- set_fishing_year(d, 2, 21) # Feb 21 - Feb 20 is the fishing year
#' }
set_fishing_year <- function(dat,
                             month_fishing_starts = 1,
                             day_fishing_starts = 1,
                             yr_col = "year",
                             date_col = "best_date",
                             ...) {
  stopifnot(yr_col %in% names(dat))
  stopifnot(date_col %in% names(dat))
  stopifnot(month_fishing_starts %in% 1:12)
  stopifnot(day_fishing_starts %in% 1:31)
  if (month_fishing_starts == 2 && day_fishing_starts > 28) {
    stop("day_fishing_starts must be 28 or less for February", call. = FALSE)
  }
  if (month_fishing_starts %in% c(4, 6, 9, 11) && day_fishing_starts > 30) {
    stop("day_fishing_starts must be 30 or less for April, June, September, or November", call. = FALSE)
  }

  yr_col_sym <- sym(yr_col)
  date_col_sym <- sym(date_col)

  dat %>%
    mutate(
      day_of_year = yday(!!date_col_sym),
      cutoff_day = yday(ymd(paste0(!!yr_col_sym, "-", month_fishing_starts, "-", day_fishing_starts))),
      !!yr_col_sym := ifelse(day_of_year < cutoff_day, year - 1, year)
    ) %>%
    select(-day_of_year, -cutoff_day)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

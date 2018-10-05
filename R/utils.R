#' Run SQL
#'
#' @param database The name of the database.
#' @param query The query to run.
#'
#' @details
#' If you need to use a user-password set up to access the databases, then you
#' will need to set the R options in your .Rprofile file: `pbs.uid`, `pbs.pwd`,
#' `pbs.ip`, `pbs.sqldriver`. E.g. `options(pbs.uid="MyUserName")` The default
#' SQL driver will be `"SQL Server"` if not specified in the options. This will
#' probably work for most people. You might try using
#' `usethis::edit_r_profile()` if you need help finding your R profile file.
#'
#' @export
run_sql <- function(database, query) {
  query <- paste(query, collapse = "\n")
  DBI::dbGetQuery(db_connection(database = database), query)
}

db_connection <- function(server = "DFBCV9TWVASP001",
                          database = "GFBioSQL") {

  pbs_uid <- getOption("pbs.uid")
  pbs_pwd <- getOption("pbs.pwd")
  pbs_ip <- getOption("pbs.ip")
  pbs_sqldriver <- getOption("pbs.sqldriver")
  if (!is.null(pbs_uid) && !is.null(pbs_uid) && !is.null(pbs_ip)) {
    DBI::dbConnect(odbc::odbc(),
      driver = if (is.null(pbs_sqldriver)) "SQL Server" else pbs_sqldriver,
      server = pbs_ip, database = database,
      pwd = pbs_pwd, uid = pbs_uid
    )
  } else {
    DBI::dbConnect(odbc::odbc(),
      driver = "SQL Server",
      server = server, database = database
    )
  }
}

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

common2codes <- function(common) {
  if (all_species_codes(common)) {
    return(force_three_letter_species_code(common))
  }

  species <- DBI::dbGetQuery(
    db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES"
  )
  common_df <- data.frame(
    SPECIES_COMMON_NAME = toupper(common),
    order_by = seq_along(common), stringsAsFactors = FALSE
  )
  .d <- filter(species, SPECIES_COMMON_NAME %in% toupper(common))
  # Remove erroneous species codes for basking shark and lingcod:
  .d <- filter(.d, !SPECIES_CODE %in% c("033", "465")) %>%
    left_join(common_df, by = "SPECIES_COMMON_NAME") %>%
    arrange(.data$order_by)
  .d$SPECIES_CODE
}

codes2common <- function(spp_code) {
  species <- DBI::dbGetQuery(
    db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES"
  )
  code_df <- data.frame(
    SPECIES_CODE = spp_code,
    order_by = seq_along(spp_code), stringsAsFactors = FALSE
  )
  .d <- filter(species, SPECIES_CODE %in% spp_code)
  # Remove erroneous species codes for basking shark and lingcod:
  .d <- filter(.d, !SPECIES_CODE %in% c("033", "465")) %>%
    left_join(common_df, by = "SPECIES_COMMON_NAME") %>%
    arrange(.data$order_by)
  .d$SPECIES_COMMON_NAME
}

collapse_filters <- function(x) {
  paste0("'", paste(x, collapse = "','"), "'")
}

inject_filter <- function(sql_precode, species, sql_code,
                          search_flag = "-- insert species here",
                          conversion_func = common2codes) {
  i <- grep(search_flag, sql_code)
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(conversion_func(species)), ")"
  )
  sql_code
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

read_sql <- function(x) {
  if (file.exists(system.file("sql", x, package = "gfplot"))) {
    readLines(system.file("sql", x, package = "gfplot"))
  } else {
    stop("The sql file does not exist.")
  }
}

round_nice <- function(x) {
  out <- plyr::round_any(x, 100)
  out[out == 0] <- x[out == 0]
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

is_ip_valid <- function(timeout = 5) {
  pbs_ip <- getOption("pbs.ip")
  if (is.null(pbs_ip)) return(FALSE)
  out <- pingr::ping(pbs_ip, verbose = FALSE,
    count = 1L, timeout = timeout)
  !is.na(out)
}

# Is this a DFO Windows computer or is the server IP accessible?
sql_server_accessible <- function() {
  if ((is_windows() && is_dfo()) || is_ip_valid()) TRUE else FALSE
}

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

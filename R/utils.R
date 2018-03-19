db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
  DBI::dbConnect(odbc::odbc(),
    driver = "SQL Server",
    server = server, database = database
  )
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

collapse_filters <- function(x) {
  paste0("'", paste(x, collapse = "','"), "'")
}

inject_filter <- function(sql_precode, species, sql_code,
                          search_flag = "-- insert species here", conversion_func = common2codes) {
  i <- grep(search_flag, sql_code)
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(conversion_func(species)), ")"
  )
  sql_code
}

run_sql <- function(database, query) {
  query <- paste(query, collapse = "\n")
  DBI::dbGetQuery(db_connection(database = database), query)
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

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_dfo <- function() {
  grepl("PBS", Sys.info()[["nodename"]])
}

#' Is this a DFO Windows computer?
#'
#' @export
is_dfo_windows <- function() {
  if (is_windows() & is_dfo()) TRUE else FALSE
}

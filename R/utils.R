db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
  DBI::dbConnect(odbc::odbc(), driver = "SQL Server",
    server = server, database = database)
}

common2codes <- function(common) {
  species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES")
  common_df <- data.frame(SPECIES_COMMON_NAME = toupper(common),
    order_by = seq_along(common), stringsAsFactors = FALSE)
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

inject_species_filter <- function(sql_precode, species, sql_code,
  collapse = TRUE) {
  i <- grep("-- insert species here", sql_code)
  sql_code[i] <- paste0(sql_precode, " (",
    collapse_filters(common2codes(species)), ")")
  if (collapse)
    paste(sql_code, collapse = "\n")
}

inject_survey_filter <- function(sql_precode, ssid, sql_code,
  collapse = TRUE) {
  i <- grep("-- insert ssid here", sql_code)
  sql_code[i] <- paste0(sql_precode, " (",
    collapse_filters(ssid), ")")
  if (collapse)
    paste(sql_code, collapse = "\n")
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

read_sql <- function(x) {
  readLines(system.file("sql", x, package = "PBSsynopsis"))
}

round_nice <- function(x) {
  out <- plyr::round_any(x, 100)
  out[out == 0] <- x[out == 0]
  out[x == 0] <- ""
  out
}

mround <- function(x, base){
  base * round(x / base)
}

round_down_even <- function(x, base = 2){
  base * floor(x / base)
}

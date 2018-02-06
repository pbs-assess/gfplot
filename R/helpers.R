db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
  DBI::dbConnect(odbc::odbc(), driver = "SQL Server",
    server = server, database = database)
}

common2codes <- function(common) {
  species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES")
  common_df <- data.frame(SPECIES_COMMON_NAME = toupper(common), order_by = seq_along(common), stringsAsFactors = FALSE)
  dd <- dplyr::filter(species, SPECIES_COMMON_NAME %in% toupper(common))
  # Remove erroneous species codes for basking shark and lingcod
  dd <- dplyr::filter(dd, !SPECIES_CODE %in% c("033", "465"))
  dd <- left_join(dd, common_df, by = "SPECIES_COMMON_NAME") %>%
    arrange(order_by)
  dd$SPECIES_CODE
}

collapse_species_names <- function(x) {
  paste0("'", paste(x, collapse = "','"), "'")
}

inject_species <- function(x, species, sql_code) {
  i <- grep("-- insert species here", sql_code)
  sql_code[i] <- paste0(x, " (", collapse_species_names(common2codes(species)), ")")
  paste(sql_code, collapse = "\n")
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
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

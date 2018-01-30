db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
  DBI::dbConnect(odbc::odbc(), driver = "SQL Server",
    server = server, database = database)
}

common2codes <- function(common) {
  species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES")
  dd <- dplyr::filter(species, SPECIES_COMMON_NAME %in% toupper(common))
  dd$SPECIES_CODE
}

collapse_species_names <- function(x) {
  paste0("'", paste(x, collapse = "','"), "'")
}

inject_species <- function(x, species, sql_code) {
  i <- grep("-- insert species here", sql_code)
  out <-c(sql_code[seq(1,i-1)],
    paste0(x, " (", collapse_species_names(common2codes(species)), ")"),
    sql_code[seq(i+1, length(sql_code))])
  paste(out, collapse = "\n")
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

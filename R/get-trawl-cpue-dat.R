#' Get PBS commercial CPUE data
#'
#' @param species A character vector of species common names
#'
#' @export
get_pbs_trawl_cpue <- function(species) {
  species <- common2codes(species)
  q <- readLines(system.file("sql", "get-trawl-cpue.sql", package = "PBSsynopsis"))
  q <- inject_species("AND C.SPECIES_CODE IN", species, q)
  sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), sql)
  d$SPECIES_COMMON_NAME[d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_scientific_name <- tolower(d$species_scientific_name)
  d
}

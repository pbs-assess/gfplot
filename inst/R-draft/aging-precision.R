# library(dplyr)
#
# db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
#   DBI::dbConnect(odbc::odbc(), driver = "SQL Server",
#     server = server, database = database)
# }
#
# common2codes <- function(common) {
#   species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
#     "SELECT * FROM SPECIES")
#   dd <- dplyr::filter(species, SPECIES_COMMON_NAME %in% toupper(common))
#   dd$SPECIES_CODE
# }
#
#
# collapse_spp_names <- function(x) {
#   paste0("'", paste(x, collapse = "','"), "'")
# }
#
# inject_species <- function(x, spp, q) {
#   i <- grep("-- insert species here", q)
#   out <-c(q[seq(1,i-1)],
#     paste0(x, " (", collapse_spp_names(common2codes(spp)), ")"),
#     q[seq(i+1, length(q))])
#   paste(out, collapse = "\n")
# }
#
# # for testing
# spp <-"canary rockfish"
#
# get_aging_precision <- function(spp) {
#   q <- readLines("inst/sql/aging-precision.sql", package = "PBSsynopsis")
#   q <- inject_species("AND SM.SPECIES_CODE IN", spp, q)
#   dbio <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), q)
#   }
#
#
# source("R/make-spp-list.R")
# species <- get_spp_names()$species_common_name
#
# inner_join(aging_precision, species)
#
# get_aging_precision("canary rockfish")


##############################################
#Final code for getting aging precision data
##############################################

get_aging_precision("canary rockfish") <- function(species) {
  q <- readLines(system.file("sql", "aging-precision.sql", package = "PBSsynopsis"))
  q <- inject_species("AND C.SPECIES_CODE IN", species, q)
  dbio <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), q)
  names(dbio) <- tolower(names(dbio))
  dbio
}

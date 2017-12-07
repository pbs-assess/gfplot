db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
  DBI::dbConnect(odbc::odbc(), driver = "SQL Server",
    server = server, database = database)
}

# save some ID tables for later:
surveys <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT * FROM SURVEY_SERIES")
saveRDS(surveys, file = "../generated-data/survey_series.rds")

# # don't think these are being used:
# species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
#   "SELECT * FROM SPECIES")
# saveRDS(species, file = "../generated-data/species.rds")
#
# trip_survey <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
#   "SELECT * FROM TRIP_SURVEY")
# saveRDS(fishing_event, file = "../generated-data/fishing_event.rds")

# spatial survey tows:
survey_ids <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT S.SURVEY_ID, SS.SURVEY_SERIES_ID, SS.SURVEY_SERIES_DESC
   FROM SURVEY S
   INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  WHERE S.SURVEY_SERIES_ID IN (1, 3, 4, 16)")

library(dplyr)
load("../spp-to-get.rda")
dd <- dplyr::filter(species, SPECIES_COMMON_NAME %in% toupper(spp))
species_codes <- dd$SPECIES_CODE
d_survs <- list()
k <- 0
for (i in seq_along(species_codes)) {
  for (j in seq_along(survey_ids$SURVEY_ID)) {
    k <- k + 1
    d_survs[[k]] <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
      paste0("EXEC proc_catmat_2011 ", survey_ids$SURVEY_ID[j], ", '", species_codes[i], "'"))
  }
}
d_survs_df <- dplyr::bind_rows(d_survs)
d_survs_df <- inner_join(d_survs_df,
  unique(select(survey_ids, SURVEY_SERIES_ID, SURVEY_SERIES_DESC)))
d_survs_df <- inner_join(d_survs_df,
  unique(select(species, SPECIES_CODE, SPECIES_COMMON_NAME, SPECIES_SCIENCE_NAME, SPECIES_DESC)))
names(d_survs_df) <- tolower(names(d_survs_df))
stopifnot(all(species_codes %in% d_survs_df$species_code))
d_survs_df <- d_survs_df %>% mutate(species_science_name = tolower(species_science_name),
  species_desc = tolower(species_desc), species_common_name = tolower(species_common_name))

saveRDS(d_survs_df, file = "../generated-data/select-survey-spatial-tows.rds")

# survey biological samples:
survey_bio_sql <- paste(readLines("../inst/sql/get-survey-biology.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), survey_bio_sql)
saveRDS(d, file = "../generated-data/all-survey-bio.rds")

# commercial biological samples:
survey_com_sql <- paste(readLines("../inst/sql/get-commercial-biology.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), survey_com_sql)
saveRDS(d, file = "../generated-data/all-commercial-bio.rds")

# catch effort:
landings_sql <- paste(readLines("../inst/sql/get-landings.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), landings_sql)
saveRDS(d, file = "../generated-data/all-catches.rds")

# spatial CPUE:
# trawlfootprint feeze = April 2012
landings_sql <- paste(readLines("../inst/sql/get-cpue.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), landings_sql)
saveRDS(d, file = "../generated-data/all-spatial-cpue.rds")

# biomass indices:
survey_boot_sql <- paste(readLines("../inst/sql/get-survey-boot.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), survey_boot_sql)
saveRDS(d, file = "../generated-data/all-boot-biomass-indices.rds")


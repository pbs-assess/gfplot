db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
  DBI::dbConnect(odbc::odbc(), driver = "SQL Server",
    server = server, database = database)
}

con <- db_connection()

surveys <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT * FROM SURVEY_SERIES")
saveRDS(surveys, file = "survey_series.rds")

species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT * FROM SPECIES")
saveRDS(species, file = "species.rds")

trip_survey <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT * FROM TRIP_SURVEY")
saveRDS(fishing_event, file = "fishing_event.rds")

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

d_survs_df <- inner_join(d_survs_df, unique(select(survey_ids, SURVEY_SERIES_ID, SURVEY_SERIES_DESC)))
d_survs_df <- inner_join(d_survs_df, unique(select(species, SPECIES_CODE, SPECIES_COMMON_NAME, SPECIES_SCIENCE_NAME,
  SPECIES_DESC)))
names(d_survs_df) <- tolower(names(d_survs_df))
head(d_survs_df)
nrow(d_survs_df)
stopifnot(all(species_codes %in% d_survs_df$species_code))
d_survs_df <- d_survs_df %>% mutate(species_science_name = tolower(species_science_name), 
  species_desc = tolower(species_desc), species_common_name = tolower(species_common_name))

saveRDS(d_survs_df, file = "select-survey-spatial-tows.rds")

# -----------------
survey_bio_sql <- paste(readLines("get-survey-biology.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), survey_bio_sql)
saveRDS(d, file = "all-survey-bio.rds")
# -----------------

survey_com_sql <- paste(readLines("get-commercial-biology.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), survey_com_sql)
saveRDS(d, file = "all-commercial-bio.rds")

# catch effort:
landings_sql <- paste(readLines("get-landings.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), landings_sql)
saveRDS(d, file = "all-catches.rds")

# spatial CPUE:
# trawlfootprint feeze = April 2012
landings_sql <- paste(readLines("get-cpue.sql"), collapse = "\n")
d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), landings_sql)

head(d)
nrow(d)
saveRDS(d, file = "all-spatial-cpue.rds")

# biomass indices:
d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
" SELECT BH.SURVEY_YEAR AS year,
      CAST(ROUND(ISNULL(BD.BIOMASS,0), 2) AS DECIMAL(15,2)) AS biomass,
  CAST(ROUND(ISNULL(BD.BOOT_LOWER_CI,0), 2) AS DECIMAL(15,2)) AS lowerci,
  CAST(ROUND(ISNULL(BD.BOOT_UPPER_CI,0), 2) AS DECIMAL(15,2)) AS upperci,
  CAST(ROUND(ISNULL(BD.BOOT_RE,0), 5) AS DECIMAL(8,5)) AS re,
  ISNULL(BD.NUM_SETS,0) AS num_sets,
  ISNULL(BD.NUM_POS_SETS,0) AS num_pos_sets,
  BD.SURVEY_SERIES_ID,
  SS.SURVEY_SERIES_DESC,
  SP.SPECIES_COMMON_NAME, SP.SPECIES_SCIENCE_NAME
  FROM (
  SELECT BH.SURVEY_YEAR,
  BH.SURVEY_SERIES_ID,
  BH.BOOT_ID
  FROM GFBioSQL.dbo.BOOT_HEADER BH
  WHERE ACTIVE_IND = 1) BH
  LEFT JOIN (
  SELECT BH.BOOT_ID,
  BD.SURVEY_SERIES_ID,
  BD.SPECIES_CODE,
  BD.BIOMASS,
  BD.BOOT_LOWER_CI,
  BD.BOOT_UPPER_CI,
  BD.BOOT_RE,
  BD.NUM_SETS,
  BD.NUM_POS_SETS
  FROM GFBioSQL.dbo.BOOT_HEADER BH
  INNER JOIN GFBioSQL.dbo.BOOT_DETAIL BD ON
  BH.BOOT_ID = BD.BOOT_ID
  WHERE BH.ACTIVE_IND = 1) BD ON BH.BOOT_ID = BD.BOOT_ID
  INNER JOIN GFBioSQL.dbo.SPECIES SP ON SP.SPECIES_CODE = BD.SPECIES_CODE
  INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = BD.SURVEY_SERIES_ID
  ORDER BY BH.SURVEY_YEAR    
")
head(d)
nrow(d)
saveRDS(d, file = "all-boot-biomass-indices.rds")


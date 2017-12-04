db_connection <- function(server = "DFBCV9TWVASP001", database = "GFBioSQL") {
  DBI::dbConnect(odbc::odbc(), driver = "SQL Server",
    server = server, database = database)
}

con <- db_connection()

d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),"
SELECT SS.SURVEY_SERIES_DESC,
	T.TRIP_ID, 
  FE.FISHING_EVENT_ID, 
  FE.FE_MAJOR_LEVEL_ID AS SET_NUM,
  T.TRIP_START_DATE,
  FE.GROUPING_CODE,

  SM.VESSEL_ID,
  
  FE.FE_MIN_BOTTOM_DEPTH,
  FE.FE_MODAL_BOTTOM_DEPTH,
  FE.FE_MAX_BOTTOM_DEPTH,

  FE.FE_BOTTOM_WATER_TEMPERATURE,
  FE.FE_BOTTOM_WATER_TEMP_DEPTH,

  FE.FE_START_LATTITUDE_DEGREE + (FE.FE_START_LATTITUDE_MINUTE)/60 AS START_LAT,
  FE.FE_END_LATTITUDE_DEGREE + (FE.FE_END_LATTITUDE_MINUTE)/60 AS END_LAT,
  -(FE.FE_START_LONGITUDE_DEGREE + (FE.FE_START_LONGITUDE_MINUTE)/60) AS START_LON,
  -(FE.FE_END_LONGITUDE_DEGREE + (FE.FE_END_LONGITUDE_MINUTE)/60) AS END_LON,
  
  Best_Lat, Best_Long, Best_Depth, 

  FE.FE_DISTANCE_TRAVELLED,

  TSP.TRLSP_DOORSPREAD, TSP.TRLSP_WINGSPREAD, TSP.TRLSP_SPEED,
  TSP.TRLSP_MOUTH_OPENING_HEIGHT, TSP.TRLSP_MOUTH_OPENING_WIDTH,
  FE.FE_BEGIN_BOTTOM_CONTACT_TIME, FE.FE_END_BOTTOM_CONTACT_TIME,

  C.SPECIES_CODE, SP.SPECIES_COMMON_NAME, SP.SPECIES_SCIENCE_NAME,
  C.CATCH_WEIGHT, C.CATCH_COUNT

FROM GFBioSQL.dbo.TRIP T

  INNER JOIN GFBioSQL.dbo.TRIP_SURVEY TS ON TS.TRIP_ID = T.TRIP_ID
  INNER JOIN GFBioSQL.dbo.SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  INNER JOIN GFBioSQL.dbo.FISHING_EVENT FE ON FE.TRIP_ID = T.TRIP_ID
  INNER JOIN GFBioSQL.dbo.FISHING_EVENT_CATCH FEC ON FEC.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  INNER JOIN GFBioSQL.dbo.CATCH C ON C.CATCH_ID = FEC.CATCH_ID
  INNER JOIN GFBioSQL.dbo.SPECIES SP ON SP.SPECIES_CODE = C.SPECIES_CODE
  LEFT JOIN GFBioSQL.dbo.B21_Samples SM ON FE.FISHING_EVENT_ID = SM.FISHING_EVENT_ID

  LEFT JOIN GFBioSQL.dbo.TRAWL_SPECS TSP ON TSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
WHERE ISNULL(TSP.USABILITY_CODE,1) IN (0,1,2,6) AND
  S.SURVEY_SERIES_ID IN (1, 3, 4, 6, 14, 16, 22, 36, 35, 41, 42, 43)
ORDER BY T.TRIP_ID, FE.FE_MAJOR_LEVEL_ID, C.SPECIES_CODE
  ")
head(d)
nrow(d)
saveRDS(d, file = "all-survey-catches.rds")

surveys <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT * FROM SURVEY_SERIES")
saveRDS(surveys, file = "survey_series.rds")

species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT * FROM SPECIES")
saveRDS(species, file = "species.rds")

# fishing_event_catch <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
#   "SELECT * FROM FISHING_EVENT_CATCH")
# saveRDS(fishing_event_catch, file = "fishing_event_catch.rds")
# 
# fishing_event <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
#   "SELECT * FROM FISHING_EVENT")
# saveRDS(fishing_event, file = "fishing_event.rds")

trip_survey <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT * FROM TRIP_SURVEY")
saveRDS(fishing_event, file = "fishing_event.rds")

survey_ids <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
  "SELECT S.SURVEY_ID, SS.SURVEY_SERIES_ID, SS.SURVEY_SERIES_DESC
   FROM SURVEY S
   INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  WHERE S.SURVEY_SERIES_ID IN (1, 3, 4, 16)")
#saveRDS(surveys, file = "survey_series.rds")

library(dplyr)
# species_codes <- dplyr::group_by(d, SPECIES_CODE) %>% 
#   dplyr::summarize(total_catch = sum(CATCH_WEIGHT, na.rm = TRUE)) %>%
#   dplyr::arrange(-total_catch) %>%
#   `[`(1:40, 1:2) %>% # get top 100 by summed total_catch
#   `$`(SPECIES_CODE)

load("../spp-to-get.rda")
dd <- dplyr::filter(species, SPECIES_COMMON_NAME %in% toupper(spp))
species_codes <- dd$SPECIES_CODE

# survey_ids <- c(1, 3, 4, 16)
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

d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),"
  SELECT TRIP_START_DATE,
    S.SURVEY_SERIES_ID,
    SP.SPECIMEN_ID,
    SPECIMEN_SEX_CODE AS SEX,
    SPECIMEN_AGE AS AGE,
    CAST(ROUND(Best_Length / 10.0, 1) AS DECIMAL(8,1)) AS LENGTH,
    MATURITY_CODE,
    MATURITY_CONVENTION_DESC,
    MATURITY_CONVENTION_MAXVALUE,
    ROUND_WEIGHT AS WEIGHT,
    SM.SPECIES_CODE, SPP.SPECIES_COMMON_NAME, SPP.SPECIES_SCIENCE_NAME,
    SM.SPECIES_CATEGORY_CODE,
    TRIP_SUB_TYPE_CODE
  FROM GFBioSQL.dbo.SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_GROUPING SG ON S.SURVEY_ID = SG.SURVEY_ID
    INNER JOIN GFBioSQL.dbo.FISHING_EVENT_GROUPING FEG ON SG.GROUPING_CODE = FEG.GROUPING_CODE
    INNER JOIN GFBioSQL.dbo.B21_Samples SM ON FEG.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
    INNER JOIN GFBioSQL.dbo.TRIP_SURVEY TS ON S.SURVEY_ID = TS.SURVEY_ID
    INNER JOIN GFBioSQL.dbo.FISHING_EVENT FE ON 
      TS.TRIP_ID = FE.TRIP_ID AND FE.FISHING_EVENT_ID = SM.FISHING_EVENT_ID
    INNER JOIN GFBioSQL.dbo.B22_Specimens SP ON SM.SAMPLE_ID = SP.SAMPLE_ID
    INNER JOIN GFBioSQL.dbo.SPECIES SPP ON SPP.SPECIES_CODE = SM.SPECIES_CODE
    INNER JOIN GFBioSQL.dbo.Maturity_Convention MC ON SM.MATURITY_CONVENTION_CODE = MC.MATURITY_CONVENTION_CODE
  WHERE SPECIMEN_SEX_CODE IN (1, 2) AND TRIP_SUB_TYPE_CODE IN (2, 3) 
  ORDER BY SM.SPECIES_CODE, S.SURVEY_SERIES_ID, TRIP_START_DATE")
head(d)
nrow(d)
# WHERE S.SURVEY_SERIES_ID IN (1, 3, 4, 6, 14, 16, 22, 36) AND
saveRDS(d, file = "all-survey-bio.rds")





# TRIP_SUB_TYPE_CODE diff!!!!!!!!
d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),"
  SELECT TRIP_START_DATE,
  SP.SPECIMEN_ID,
  SPECIMEN_SEX_CODE AS SEX,
  SPECIMEN_AGE AS AGE,
  CAST(ROUND(Best_Length / 10.0, 1) AS DECIMAL(8,1)) AS LENGTH,
  MATURITY_CODE,
  MATURITY_CONVENTION_DESC,
  MATURITY_CONVENTION_MAXVALUE,
  ROUND_WEIGHT AS WEIGHT,
  SM.SPECIES_CODE, SPP.SPECIES_COMMON_NAME, SPP.SPECIES_SCIENCE_NAME,
  SM.SPECIES_CATEGORY_CODE,
  TRIP_SUB_TYPE_CODE

 FROM GFBioSQL.dbo.B21_Samples SM
  INNER JOIN GFBioSQL.dbo.B22_Specimens SP ON SM.SAMPLE_ID = SP.SAMPLE_ID
  INNER JOIN GFBioSQL.dbo.SPECIES SPP ON SPP.SPECIES_CODE = SM.SPECIES_CODE
  INNER JOIN GFBioSQL.dbo.Maturity_Convention MC ON SM.MATURITY_CONVENTION_CODE = MC.MATURITY_CONVENTION_CODE

  WHERE TRIP_SUB_TYPE_CODE NOT IN (2, 3) AND SPECIMEN_SEX_CODE IN (1, 2)
  ORDER BY SM.SPECIES_CODE, TRIP_START_DATE")
head(d)
nrow(d)
saveRDS(d, file = "all-commercial-bio.rds")

# catch effort:
d <- DBI::dbGetQuery(db_connection(database = "GFFOS"),
  "SELECT DATABASE_NAME,
     FISHERY_SECTOR,
     GEAR,
     BEST_DATE,
     FE_START_DATE, FE_END_DATE,
     SPECIES_SCIENTIFIC_NAME, SPECIES_COMMON_NAME,
     LANDED_KG, DISCARDED_KG, LANDED_PCS, DISCARDED_PCS
  FROM GFFOS.dbo.GF_MERGED_CATCH MC
  INNER JOIN GFFOS.dbo.SPECIES SP ON SP.SPECIES_CODE = MC.SPECIES_CODE
  ORDER BY BEST_DATE, SPECIES_COMMON_NAME")
head(d)
nrow(d)
saveRDS(d, file = "all-catches.rds")


# spatial CPUE:

# trawlfootprint feeze = April 2012

d <- DBI::dbGetQuery(db_connection(database = "GFFOS"),
"SELECT YEAR(BEST_DATE) AS YEAR,
TRIP_ID,
FISHING_EVENT_ID,
LAT,
LON,
C.VESSEL_REGISTRATION_NUMBER,
SPECIES_SCIENTIFIC_NAME, SPECIES_COMMON_NAME,
(ISNULL(LANDED_ROUND_KG,0) + ISNULL(TOTAL_RELEASED_ROUND_KG,0)) / 
  (DATEDIFF(N, START_DATE, END_DATE) / 60.0) AS cpue
FROM GFFOS.dbo.GF_D_OFFICIAL_FE_CATCH C
INNER JOIN GFFOS.dbo.SPECIES SP ON SP.SPECIES_CODE = C.SPECIES_CODE
WHERE LAT BETWEEN 47.8 AND 55 AND
LON BETWEEN -135 AND -122 AND YEAR(BEST_DATE) > 2012 AND
FISHERY_SECTOR = 'GROUNDFISH TRAWL' AND
ISNULL(LANDED_ROUND_KG,0) + ISNULL(TOTAL_RELEASED_ROUND_KG,0) > 0 AND
END_DATE > START_DATE AND YEAR(START_DATE) = YEAR(END_DATE)
ORDER BY YEAR, SPECIES_COMMON_NAME, C.VESSEL_REGISTRATION_NUMBER")

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



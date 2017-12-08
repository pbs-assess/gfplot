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

get_spatial_survey <- function(spp, survey_codes = c(1, 3, 4, 16)) {
  species_codes <- common2codes(spp)
  library(dplyr)

  q <- paste("SELECT S.SURVEY_ID, SS.SURVEY_SERIES_ID, SS.SURVEY_SERIES_DESC
    FROM SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
    WHERE S.SURVEY_SERIES_ID IN (", paste(survey_codes, collapse = ", "), ")")

  species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES")

  survey_ids <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), q)
    d_survs <- list()
    k <- 0
    for (i in seq_along(species_codes)) {
      message(paste("Extracting spatial survey data for species code", species_codes[i]))
      for (j in seq_along(survey_ids$SURVEY_ID)) {
        k <- k + 1
        d_survs[[k]] <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
          paste0("EXEC proc_catmat_2011 ", survey_ids$SURVEY_ID[j], ", '",
            species_codes[i], "'"))
      }
    }
    d_survs_df <- dplyr::bind_rows(d_survs)
    d_survs_df <- dplyr::inner_join(d_survs_df,
      unique(dplyr::select(survey_ids,
          SURVEY_SERIES_ID,
          SURVEY_SERIES_DESC)), by = "SURVEY_SERIES_ID")
    d_survs_df <- dplyr::inner_join(d_survs_df,
      unique(select(species,
          SPECIES_CODE,
          SPECIES_COMMON_NAME,
          SPECIES_SCIENCE_NAME,
          SPECIES_DESC)), by = "SPECIES_CODE")
    names(d_survs_df) <- tolower(names(d_survs_df))
    stopifnot(all(species_codes %in% d_survs_df$species_code))
    d_survs_df <- dplyr::mutate(d_survs_df,
      species_science_name = tolower(species_science_name),
      species_desc = tolower(species_desc),
      species_common_name = tolower(species_common_name))
    d_survs_df
}

collapse_spp_names <- function(x) {
  paste0("'", paste(x, collapse = "','"), "'")
}

get_survey_specimens <- function(spp) {
  spp <- common2codes(spp)
  q <- readLines("inst/sql/get-survey-biology.sql")
  i <- grep("WHERE", q)
  q <- c(q[1:i],
    paste("AND SM.SPECIES_CODE IN (", collapse_spp_names(spp), ")"),
    q[(i+1):length(q)])
  survey_bio_sql <- paste(q, collapse = "\n")
  dbio <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), survey_bio_sql)

  surveys <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT * FROM SURVEY_SERIES")
  ss <- dplyr::select(surveys, -SURVEY_SERIES_TYPE_CODE)
  names(ss) <- tolower(names(ss))

  names(dbio) <- tolower(names(dbio))
  dbio$species_common_name <- tolower(dbio$species_common_name)
  dbio$species_science_name <- tolower(dbio$species_science_name)
  dbio <- dplyr::mutate(dbio, year = lubridate::year(trip_start_date))
  dbio <- dplyr::inner_join(dbio, ss, by = "survey_series_id")

  warning("Duplicate specimen IDs still present! Filter them yourself.")
  # dbio <- dbio[!duplicated(dbio$specimen_id), ]

  dbio
}

get_commercial_specimens <- function(spp) {
  spp <- common2codes(spp)
  q <- readLines("inst/sql/get-commercial-biology.sql")
  i <- grep("WHERE TRIP", q)
  q <- c(q[1:i],
    paste("AND SM.SPECIES_CODE IN (", collapse_spp_names(spp), ")"),
    q[(i+1):length(q)])
  sql <- paste(q, collapse = "\n")
  dbio_c <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), sql)

  names(dbio_c) <- tolower(names(dbio_c))
  dbio_c$species_common_name <- tolower(dbio_c$species_common_name)
  dbio_c$species_science_name <- tolower(dbio_c$species_science_name)
  dbio_c <- mutate(dbio_c, year = lubridate::year(trip_start_date))
  assertthat::assert_that(sum(duplicated(dbio_c$specimen_id)) == 0)
  dbio_c <- select(dbio_c, species_common_name, species_science_name,
    year, age, length, weight, maturity_code)
  dbio_c
}

get_landings <- function(spp) {
  spp <- common2codes(spp)
  q <- readLines("inst/sql/get-landings.sql")
  i <- grep("ORDER BY BEST", q) - 1
  q <- c(q[1:i],
    paste("WHERE SP.SPECIES_CODE IN (", collapse_spp_names(spp), ")"),
    q[(i+1):length(q)])
  landings_sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), landings_sql)
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_scientific_name <- tolower(d$species_scientific_name)
  d$year <- lubridate::year(d$best_date)
  d <- filter(d, !is.na(species_common_name), !is.na(year)) %>%
    group_by(year, species_common_name, gear) %>%
    summarise(
      landed_kg = sum(landed_kg, na.rm = TRUE),
      discarded_kg = sum(discarded_kg, na.rm = TRUE),
      landed_pcs = sum(landed_pcs, na.rm = TRUE),
      discarded_pcs = sum(discarded_pcs, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(species_common_name, year)
  d
}

get_cpue <- function(spp) {
  spp <- common2codes(spp)
  q <- readLines("inst/sql/get-cpue.sql")
  i <- grep("ORDER BY YEAR", q) - 1
  q <- c(q[1:i],
    paste("AND SP.SPECIES_CODE IN (", collapse_spp_names(spp), ")"),
    q[(i+1):length(q)])
  sql <- paste(q, collapse = "\n")
  dcpue <- DBI::dbGetQuery(db_connection(database = "GFFOS"), sql)
  dcpue$SPECIES_COMMON_NAME[dcpue$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_scientific_name <- tolower(d$species_scientific_name)
  d$year <- lubridate::year(d$best_date)
  d
}

get_bio_indices <- function(spp) {
  spp <- common2codes(spp)
  q <- readLines("inst/sql/get-survey-boot.sql")
  i <- grep("ORDER BY BH.SURVEY_YEAR", q) - 1
  q <- c(q[1:i],
    paste("WHERE SP.SPECIES_CODE IN (", collapse_spp_names(spp), ")"),
    q[(i+1):length(q)])
  sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), sql)
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_science_name <- tolower(d$species_science_name)
  d
}

get_all_data <- function(spp, path = "data-cache") {
  dir.create(path, showWarnings = FALSE)

  d_survs_df <- get_spatial_survey(species)
  saveRDS(d_survs_df, file = file.path(path, "all-survey-spatial-tows.rds"))

  d <- get_survey_specimens(species)
  saveRDS(d, file = file.path(path, "all-survey-bio.rds"))

  d <- get_commercial_specimens(species)
  saveRDS(d, file = file.path(path, "all-commercial-bio.rds"))

  d <- get_landings(species)
  saveRDS(d, file = file.path(path, "all-catches.rds"))

  d <- get_cpue(species)
  saveRDS(d, file = file.path(path, "all-spatial-cpue.rds"))

  d <- get_bio_indices(species)
  saveRDS(d, file = file.path(path, "all-boot-biomass-indices.rds"))
}

load("spp-to-get.rda") # contains `spp`
species <- spp
rm(spp)

get_all_data(species)

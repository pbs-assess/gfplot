#' Title here TODO
#'
#' @export
get_pbs_sample_trips <- function() {
  x <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT SAMPLE_ID, FISHING_EVENT_ID FROM B21_Samples")
  names(x) <- tolower(names(x))
  x
}

#' Title here TODO
#'
#' @export
get_pbs_strata <- function() {
  x <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT SG.SURVEY_ID,
    SG.GROUPING_CODE,
    G.AREA_KM2
    FROM SURVEY_GROUPING SG
    INNER JOIN GROUPING G ON
    SG.GROUPING_CODE = G.GROUPING_CODE")
  names(x) <- tolower(names(x))
  x
}

#' Get PBS spatial trawl survey data
#'
#' @param species A character vector of species common names
#' @param survey_codes A numeric vector of survey series IDs
#'
#' @export
get_pbs_survey <- function(species, survey_codes = c(1, 3, 4, 16)) {
  species_codes <- common2codes(species)

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

#' Get PBS survey specimen data
#'
#' @param species A character vector of species common names
#' @export
get_pbs_survsamples <- function(species) {
  q <- readLines(system.file("sql", "get-survey-biology.sql", package = "PBSsynopsis"))
  q <- inject_species("AND SM.SPECIES_CODE IN", species, sql_code = q)
  dbio <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), q)

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

#blah

#' Get PBS commercial specimen data
#'
#' @param species A character vector of species common names
#' @export
get_pbs_commsamples <- function(species) {
  q <- readLines(system.file("sql", "get-commercial-biology.sql", package = "PBSsynopsis"))
  q <- inject_species("AND SM.SPECIES_CODE IN", species, sql_code = q)
  dbio_c <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), q)
  names(dbio_c) <- tolower(names(dbio_c))
  dbio_c$species_common_name <- tolower(dbio_c$species_common_name)
  dbio_c$species_science_name <- tolower(dbio_c$species_science_name)
  dbio_c <- mutate(dbio_c, year = lubridate::year(trip_start_date))
  assertthat::assert_that(sum(duplicated(dbio_c$specimen_id)) == 0)
  dbio_c
}

#' Get PBS commercial landings data
#'
#' @param species A character vector of species common names
#' @export
get_pbs_catch <- function(species) {
  species <- common2codes(species)
  q <- readLines(system.file("sql", "get-landings.sql", package = "PBSsynopsis"))
  i <- grep("ORDER BY BEST", q) - 1
  q <- c(q[seq(1, i)],
    paste("WHERE SP.SPECIES_CODE IN (", collapse_species_names(species), ")"),
    q[seq(i+1, length(q))])
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

#' Get PBS commercial CPUE data (for mapping)
#'
#' @param species A character vector of species common names
#' @export
get_pbs_cpue <- function(species) {
  species <- common2codes(species)
  q <- readLines(system.file("sql", "get-cpue.sql", package = "PBSsynopsis"))
  i <- grep("ORDER BY YEAR", q) - 1
  q <- c(q[seq(1, i)],
    paste("AND SP.SPECIES_CODE IN (", collapse_species_names(species), ")"),
    q[seq(i+1, length(q))])
  sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), sql)
  d$SPECIES_COMMON_NAME[d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_scientific_name <- tolower(d$species_scientific_name)
  d
}

#' Get PBS catch and effort data for all species for CPUE index calculation
#'
#' Can be used to extract data for \code{\link{prep_pbs_cpue_index}}.
#'
#' @param gear A single gear type to include
#' @param min_year Minimum year to return
#'
#' @export
get_pbs_cpue_index <- function(gear = "bottom trawl", min_year = 1996) {
  q <- readLines(system.file("sql", "get-all-merged-catch.sql", package = "PBSsynopsis"))
  i <- grep("-- insert filters here", q)
  # TODO allow for multiple gear types?
  q[i] <- paste0("GEAR IN('", toupper(gear), "') AND YEAR >= ", min_year, " AND")
  sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), sql)
  d$SPECIES_COMMON_NAME[d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d
}

#' Get PBS ageing precision data
#'
#' @param species A character vector of a species common names
#' @export
get_pbs_ageing_precision <- function(species) {
  q <- readLines(system.file("sql", "ageing-precision.sql", package = "PBSsynopsis"))
  q <- inject_species("AND C.SPECIES_CODE IN", species, q)
  dbio <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), q)
  names(dbio) <- tolower(names(dbio))
  dbio
}

#' Get PBS biological index data
#'
#' @param species A character vector of species common names
#' @export
get_pbs_bioindex <- function(species) {
  species <- common2codes(species)
  q <- readLines(system.file("sql", "get-survey-boot.sql", package = "PBSsynopsis"))
  i <- grep("ORDER BY BH.SURVEY_YEAR", q) - 1
  q <- c(q[seq(1, i)],
    paste("WHERE SP.SPECIES_CODE IN (", collapse_species_names(species), ")"),
    q[seq(i+1, length(q))])
  sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), sql)
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_science_name <- tolower(d$species_science_name)
  d
}

#' Cache all PBS groundfish data for one or more species
#'
#' @param species A character vector of species common names
#' @param path The folder where the cached data will be saved
#' @export
cache_pbs_data <- function(species, path = "data-cache") {
  dir.create(path, showWarnings = FALSE)

  d_survs_df <- get_pbs_survey(species)
  saveRDS(d_survs_df, file = file.path(path, "pbs-survey-tows.rds"))

  d <- get_pbs_survsamples(species)
  saveRDS(d, file = file.path(path, "pbs-survey-specimens.rds"))

  d <- get_pbs_commsamples(species)
  saveRDS(d, file = file.path(path, "pbs-commercial-specimens.rds"))

  d <- get_pbs_catch(species)
  saveRDS(d, file = file.path(path, "pbs-catch.rds"))

  d <- get_pbs_cpue(species)
  saveRDS(d, file = file.path(path, "pbs-cpue.rds"))

  d <- get_pbs_bioindex(species)
  saveRDS(d, file = file.path(path, "pbs-bioindex.rds"))

  d <- get_pbs_sample_trips()
  saveRDS(d, file = file.path(path, "pbs-sample-trips.rds"))

  d <- get_pbs_strata()
  saveRDS(d, file = file.path(path, "pbs-strata.rds"))

  d <- get_pbs_ageing_precision(species)
  saveRDS(d, file = file.path(path, "pbs-ageing-precision.rds"))
}

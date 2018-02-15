#' Get PBS data
#'
#' Long description here
#'
#' @details
#'
#' * `get_sample_trips()` does...
#' * `get_strata()` does...
#' * `get_survey()` does...
#' * `get_survsamples()` does...
#' * `get_commsamples()` does...
#' * `get_catch()` does...
#' * `get_cpue()` does...
#' * `get_cpue_index()` does...
#' * `get_ageing_precision()` does...
#' * `get_sara_dat()` does...
#' * `get_bioindex()` does...
#' * `cache_pbs_data()` does...
#'
#' @param species A character vector of species common names
#' @param survey_codes A numeric vector of survey series IDs
#' @name get
NULL

#' @export
#' @rdname get
get_sample_trips <- function() {
  x <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT SAMPLE_ID, FISHING_EVENT_ID FROM B21_Samples")
  names(x) <- tolower(names(x))
  as_tibble(x)
}

#' @export
#' @rdname get
get_strata <- function() {
  x <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT SG.SURVEY_ID,
    SG.GROUPING_CODE,
    G.AREA_KM2
    FROM SURVEY_GROUPING SG
    INNER JOIN GROUPING G ON
    SG.GROUPING_CODE = G.GROUPING_CODE")
  names(x) <- tolower(names(x))
  as_tibble(x)
}

#' @export
#' @rdname get
get_survey <- function(species, survey_codes = c(1, 3, 4, 16)) {
  species_codes <- common2codes(species)

  sql <- paste(
    "SELECT S.SURVEY_ID,
      SS.SURVEY_SERIES_ID,
      SS.SURVEY_SERIES_DESC
    FROM SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON
      SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
    WHERE S.SURVEY_SERIES_ID IN (", paste(survey_codes, collapse = ", "), ")")

  species <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT * FROM SPECIES")

  survey_ids <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), sql)
  d_survs <- list()
  k <- 0
  for (i in seq_along(species_codes)) {
    for (j in seq_along(survey_ids$SURVEY_ID)) {
      k <- k + 1
      d_survs[[k]] <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
        paste0("EXEC proc_catmat_2011 ", survey_ids$SURVEY_ID[j], ", '",
          species_codes[i], "'"))
    }
  }
  x <- dplyr::bind_rows(d_survs)
  x <- dplyr::inner_join(x,
    unique(dplyr::select(survey_ids,
      SURVEY_SERIES_ID,
      SURVEY_SERIES_DESC)), by = "SURVEY_SERIES_ID")
  x <- dplyr::inner_join(x,
    unique(select(species,
      SPECIES_CODE,
      SPECIES_COMMON_NAME,
      SPECIES_SCIENCE_NAME,
      SPECIES_DESC)), by = "SPECIES_CODE")
  names(x) <- tolower(names(x))
  stopifnot(all(species_codes %in% x$species_code))
  x <- mutate(x,
    species_science_name = tolower(species_science_name),
    species_desc = tolower(species_desc),
    species_common_name = tolower(species_common_name))
  as_tibble(x)
}

#' @export
#' @rdname get
#' @param remove_bad_data Remove known bad data?
get_survsamples <- function(species, remove_bad_data = TRUE) {
  sql <- read_sql("get-survey-biology.sql")
  sql <- inject_species_filter("AND SM.SPECIES_CODE IN", species, sql_code = sql)
  x <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), sql)

  surveys <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT * FROM SURVEY_SERIES")
  ss <- dplyr::select(surveys, -SURVEY_SERIES_TYPE_CODE)
  names(ss) <- tolower(names(ss))

  names(x) <- tolower(names(x))
  x$species_common_name <- tolower(x$species_common_name)
  x$species_science_name <- tolower(x$species_science_name)
  x <- dplyr::mutate(x, year = lubridate::year(trip_start_date))
  x <- dplyr::inner_join(x, ss, by = "survey_series_id")

  warning("Duplicate specimen IDs may still be present. ",
      "Filter them yourself after selecting specific surveys. ",
      "For example, `d <- d[!duplicated(d$specimen_id), ]`")

  if (remove_bad_data) {
    x <- x[!(x$length > 600 &
      x$species_common_name == "north pacific spiny dogfish"), drop = FALSE]
    x <- x[!(x$length > 600 &
      x$species_common_name == "big skate"), drop = FALSE]
    x <- x[!(x$length > 600 &
      x$species_common_name == "longnose skate"), drop = FALSE]
    x <- x[!(x$length > 60 &
      x$species_common_name == "pacific tomcod"), drop = FALSE]
    x <- x[!(x$length > 50 &
      x$species_common_name == "quillback-rockfish"), drop = FALSE]
    x <- x[!(x$length < 10 & x$weight/1000 > 1.0 &
      x$species_common_name == "pacific flatnose"), drop = FALSE]
  }

  as_tibble(x)
}

#' @export
#' @rdname get
get_commsamples <- function(species) {
  q <- read_sql("get-commercial-biology.sql")
  q <- inject_species_filter("AND SM.SPECIES_CODE IN", species, sql_code = q)
  x <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), q)
  names(x) <- tolower(names(x))
  x$species_common_name <- tolower(x$species_common_name)
  x$species_science_name <- tolower(x$species_science_name)
  x <- mutate(x, year = lubridate::year(trip_start_date))
  assertthat::assert_that(sum(duplicated(x$specimen_id)) == 0)
  as_tibble(x)
}

#' @export
#' @rdname get
get_catch <- function(species) {
  species <- common2codes(species)
  q <- read_sql("get-landings.sql")
  i <- grep("ORDER BY BEST", q) - 1
  q <- c(q[seq(1, i)],
    paste("WHERE SP.SPECIES_CODE IN (", collapse_filters(species), ")"),
    q[seq(i + 1, length(q))])
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
  as_tibble(d)
}

#' @export
#' @rdname get
get_cpue <- function(species) {
  species <- common2codes(species)
  q <- read_sql("get-cpue.sql")
  i <- grep("ORDER BY YEAR", q) - 1
  q <- c(q[seq(1, i)],
    paste("AND SP.SPECIES_CODE IN (", collapse_filters(species), ")"),
    q[seq(i + 1, length(q))])
  sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), sql)
  d$SPECIES_COMMON_NAME[d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_scientific_name <- tolower(d$species_scientific_name)
  as_tibble(d)
}

#' @param gear The gear type(s) to include. Will be converted to uppercase.
#' @param min_year Minimum year to return.
#' @export
#' @rdname get
get_cpue_index <- function(gear = "bottom trawl", min_year = 1996) {
  q <- read_sql("get-all-merged-catch.sql")
  i <- grep("-- insert filters here", q)
  # TODO allow for multiple gear types?
  q[i] <- paste0("GEAR IN(", collapse_filters(toupper(gear)),
    ") AND YEAR(BEST_DATE) >= ", min_year, " AND")
  sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFFOS"), sql)
  names(d) <- tolower(names(d))
  as_tibble(d)
}

#' @export
#' @rdname get
get_ageing_precision <- function(species) {
  q <- read_sql("ageing-precision.sql")
  q <- inject_species_filter("AND C.SPECIES_CODE IN", species, q)
  x <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), q)
  names(x) <- tolower(names(x))
  as_tibble(x)
}

#' @export
#' @rdname get
get_bioindex <- function(species) {
  species <- common2codes(species)
  q <- read_sql("get-survey-boot.sql")
  i <- grep("ORDER BY BH.SURVEY_YEAR", q) - 1
  q <- c(q[seq(1, i)],
    paste("WHERE SP.SPECIES_CODE IN (", collapse_filters(species), ")"),
    q[seq(i + 1, length(q))])
  sql <- paste(q, collapse = "\n")
  d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"), sql)
  names(d) <- tolower(names(d))
  d$species_common_name <- tolower(d$species_common_name)
  d$species_science_name <- tolower(d$species_science_name)
  as_tibble(d)
}

#' @export
#' @rdname get
get_sara_dat <- function() {
  h <- xml2::read_html(
    "http://www.registrelep-sararegistry.gc.ca/sar/index/default_e.cfm")
  d <- h %>% rvest::html_nodes("table") %>%
    .[[1]] %>%
    rvest::html_table() %>%
    .[- (1:2), ] %>%
    dplyr::as_tibble() %>%
    dplyr::filter(.data$Taxon %in% "Fishes") %>%
    dplyr::filter(!grepl("Salmon", .data$`Common name *`))
  names(d) <- tolower(names(d))
  names(d) <- gsub(" ", "_", names(d))
  names(d) <- gsub("_\\*", "", names(d))
  as_tibble(d)
}

#' @param path The folder where the cached data will be saved
#' @export
#' @rdname get
cache_pbs_data <- function(species, path = "data-cache") {
  dir.create(path, showWarnings = FALSE)

  d_survs_df <- get_survey(species)
  saveRDS(d_survs_df, file = file.path(path, "pbs-survey-tows.rds"))

  d <- get_survsamples(species)
  saveRDS(d, file = file.path(path, "pbs-survey-specimens.rds"))

  d <- get_commsamples(species)
  saveRDS(d, file = file.path(path, "pbs-commercial-specimens.rds"))

  d <- get_catch(species)
  saveRDS(d, file = file.path(path, "pbs-catch.rds"))

  d <- get_cpue(species)
  saveRDS(d, file = file.path(path, "pbs-cpue.rds"))

  d <- get_bioindex(species)
  saveRDS(d, file = file.path(path, "pbs-bioindex.rds"))

  d <- get_sample_trips()
  saveRDS(d, file = file.path(path, "pbs-sample-trips.rds"))

  d <- get_strata()
  saveRDS(d, file = file.path(path, "pbs-strata.rds"))

  d <- get_ageing_precision(species)
  saveRDS(d, file = file.path(path, "pbs-ageing-precision.rds"))
}

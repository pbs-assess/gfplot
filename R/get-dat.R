#' Get PBS data
#'
#' Long description here
#'
#' @details
#' * `get_surv_tows()` does...
#' * `get_surv_samples()` does...
#' * `get_comm_samples()` does...
#' * `get_catch()` does...
#' * `get_cpue_spatial()` does...
#' * `get_cpue_index()` does...
#' * `get_age_precision()` does...
#' * `get_sara_dat()` does...
#' * `get_surv_index()` does...
#' * `cache_pbs_data()` does...
#'
#' @param species A character vector of species common names
#' @param survey_codes A numeric vector of survey series IDs
#' @name get
NULL

#' @rdname get
get_sample_trips <- function() {
  x <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT SAMPLE_ID, FISHING_EVENT_ID FROM B21_Samples")
  names(x) <- tolower(names(x))
  as_tibble(x)
}

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
get_surv_tows <- function(species, ssid = c(1, 3, 4, 16)) {
  species_codes <- common2codes(species)

  .q <- paste(
    "SELECT S.SURVEY_ID,
      SS.SURVEY_SERIES_ID,
      SS.SURVEY_SERIES_DESC
    FROM SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON
      SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
    WHERE S.SURVEY_SERIES_ID IN (", paste(ssid, collapse = ", "), ")")

  species <- run_sql("GFBioSQL", "SELECT * FROM SPECIES")

  survey_ids <- run_sql("GFBioSQL", .q)
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
  .d <- dplyr::bind_rows(d_survs)
  .d <- dplyr::inner_join(.d,
    unique(dplyr::select(survey_ids,
      SURVEY_SERIES_ID,
      SURVEY_SERIES_DESC)), by = "SURVEY_SERIES_ID")
  .d <- dplyr::inner_join(.d,
    unique(select(species,
      SPECIES_CODE,
      SPECIES_COMMON_NAME,
      SPECIES_SCIENCE_NAME,
      SPECIES_DESC)), by = "SPECIES_CODE")
  names(.d) <- tolower(names(.d))
  stopifnot(all(species_codes %in% .d$species_code))
  .d <- mutate(.d,
    species_science_name = tolower(species_science_name),
    species_desc = tolower(species_desc),
    species_common_name = tolower(species_common_name))
  as_tibble(.d)
}

#' @export
#' @rdname get
#' @param ssid TODO
#' @param remove_bad_data Remove known bad data?
get_surv_samples <- function(species, ssid = NULL, remove_bad_data = TRUE) {
  .q <- read_sql("get-surv-samples.sql")
  .q <- inject_species_filter("AND SM.SPECIES_CODE IN", species, sql_code = .q)
  if (!is.null(ssid))
    .q <- inject_survey_filter("AND S.SURVEY_SERIES_ID IN", ssid, sql_code = .q)
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)

  surveys <- run_sql("GFBioSQL", "SELECT * FROM SURVEY_SERIES")
  surveys <- select(surveys, -SURVEY_SERIES_TYPE_CODE)
  names(surveys) <- tolower(names(surveys))

  .d <- inner_join(.d, surveys, by = "survey_series_id")
  .d <- as_tibble(.d)

  warning("Duplicate specimen IDs may still be present. ",
      "Filter them yourself after selecting specific surveys. ",
      "For example, `dat <- dat[!duplicated(dat$specimen_id), ]`")

  if (remove_bad_data) {
    .d <- .d[!(.d$length > 600 &
    .d$species_common_name == "north pacific spiny dogfish"), ]
    .d <- .d[!(.d$length > 600 & .d$species_common_name == "big skate"), ]
    .d <- .d[!(.d$length > 600 & .d$species_common_name == "longnose skate"), ]
    .d <- .d[!(.d$length > 60 & .d$species_common_name == "pacific tomcod"), ]
    .d <- .d[!(.d$length > 50 &
        .d$species_common_name == "quillback-rockfish"), ]
    .d <- .d[!(.d$length < 10 & .d$weight / 1000 > 1.0 &
      .d$species_common_name == "pacific flatnose"), ]
  }

}

#' @export
#' @rdname get
get_comm_samples <- function(species) {
  .q <- read_sql("get-comm-samples.sql")
  .q <- inject_species_filter("AND SM.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)
  .d <- mutate(.d, year = lubridate::year(trip_start_date))
  assertthat::assert_that(sum(duplicated(.d$specimen_id)) == 0)
  as_tibble(.d)
}

#' @export
#' @rdname get
get_catch <- function(species) {
  species <- common2codes(species)
  .q <- read_sql("get-catch.sql")
  .q <- inject_species_filter("WHERE SP.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFFOS", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  .d$year <- lubridate::year(.d$best_date)
  as_tibble(.d)
}

#' @export
#' @rdname get
get_cpue_spatial <- function(species) {
  species <- common2codes(species)
  .q <- read_sql("get-cpue-spatial.sql")
  .q <- inject_species_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFFOS", .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  as_tibble(.d)
}

#' @param gear The gear type(s) to include. Will be converted to uppercase.
#' @param min_year Minimum year to return.
#' @export
#' @rdname get
get_cpue_index <- function(gear = "bottom trawl", min_year = 1996) {
  .q <- read_sql("get-cpue-index.sql")
  i <- grep("-- insert filters here", .q)
  .q[i] <- paste0("GEAR IN(", collapse_filters(toupper(gear)),
    ") AND YEAR(BEST_DATE) >= ", min_year, " AND")
  .d <- run_sql("GFFOS", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

#' @export
#' @rdname get
get_age_precision <- function(species) {
  .q <- read_sql("get-age-precision.sql")
  .q <- inject_species_filter("AND C.SPECIES_CODE IN", species, .q)
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

#' @export
#' @rdname get
get_surv_index <- function(species, ssid = NULL) {
  species <- common2codes(species)
  .q <- read_sql("get-surv-index.sql")
  .q <- inject_species_filter("WHERE SP.SPECIES_CODE IN", species, .q)
  if (!is.null(ssid))
    .q <- inject_survey_filter("WHERE BD.SURVEY_SERIES_ID IN", ssid, .q)
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)
  as_tibble(.d)
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

  d_survs_df <- get_surv_tows(species)
  saveRDS(d_survs_df, file = file.path(path, "pbs-surv-tows.rds"))

  d <- get_surv_samples(species)
  saveRDS(d, file = file.path(path, "pbs-surv-samples.rds"))

  d <- get_comm_samples(species)
  saveRDS(d, file = file.path(path, "pbs-comm-samples.rds"))

  d <- get_catch(species)
  saveRDS(d, file = file.path(path, "pbs-catch.rds"))

  d <- get_cpue_spatial(species)
  saveRDS(d, file = file.path(path, "pbs-cpue-spatial.rds"))

  d <- get_surv_index(species)
  saveRDS(d, file = file.path(path, "pbs-survindex.rds"))

  d <- get_age_precision(species)
  saveRDS(d, file = file.path(path, "pbs-age-precision.rds"))
}

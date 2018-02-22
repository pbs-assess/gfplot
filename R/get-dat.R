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
#' @param species One or more species common names (e.g. `"pacific ocean
#'   perch"`) or one or more species codes (e.g. `396`). Species codes can be
#'   specified as numeric vectors `c(396, 442`) or characters `c("396", "442")`.
#'   Numeric values shorter than 3 digits will be expanded to 3 digits and
#'   converted to character objects (`1` turns into `"001"`). Species common
#'   names and species codes should not be mixed. If any element is missing a
#'   species code, then all elements will be assumed to be species, common
#'   names.
#' @param ssid A numeric vector of survey series IDs. Run `get_ssids()` if you
#'   want to look up a survey series ID via a surveys series description.
#' @name get
NULL

#' @export
#' @rdname get
get_ssids <- function() {
  .d <- run_sql("GFBioSQL",
    "SELECT SURVEY_SERIES_ID, SURVEY_SERIES_DESC
      FROM SURVEY_SERIES")
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname get
get_age_methods <- function() {
  .d <- DBI::dbGetQuery(db_connection(database = "GFBioSQL"),
    "SELECT AGEING_METHOD_CODE, AGEING_METHOD_DESC, ROW_VERSION
    FROM AGEING_METHOD")
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

get_sample_trips <- function() {
  run_sql("GFBioSQL",
    "SELECT SAMPLE_ID, FISHING_EVENT_ID FROM B21_Samples")
}

get_strata_areas <- function() {
  run_sql("GFBioSQL",
    "SELECT SG.SURVEY_ID,
    SG.GROUPING_CODE,
    G.AREA_KM2
    FROM SURVEY_GROUPING SG
    INNER JOIN GROUPING G ON
    SG.GROUPING_CODE = G.GROUPING_CODE")
}

get_survey_ids <- function(ssid) {
  .q <- paste(
    "SELECT S.SURVEY_ID,
    SS.SURVEY_SERIES_ID,
    SS.SURVEY_SERIES_DESC
    FROM SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON
    SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
    WHERE S.SURVEY_SERIES_ID IN (", paste(ssid, collapse = ", "), ")")
  run_sql("GFBioSQL", .q)
}

#' @export
#' @rdname get
get_surv_tows <- function(species, ssid = c(1, 3, 4, 16)) {
  species_codes <- common2codes(species)

  species_df <- run_sql("GFBioSQL", "SELECT * FROM SPECIES")
  sample_trip_ids <- get_sample_trips()
  areas <- get_strata_areas()
  survey_ids <- get_survey_ids(ssid)

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
  .d <- bind_rows(d_survs)

  .d <- inner_join(.d,
    unique(select(survey_ids,
      SURVEY_SERIES_ID,
      SURVEY_SERIES_DESC)), by = "SURVEY_SERIES_ID")

  .d <- inner_join(.d,
    unique(select(species_df,
      SPECIES_CODE,
      SPECIES_COMMON_NAME,
      SPECIES_SCIENCE_NAME,
      SPECIES_DESC)), by = "SPECIES_CODE")

  .d <- left_join(.d, sample_trip_ids, by = "FISHING_EVENT_ID") %>%
    left_join(areas, by = c("SURVEY_ID", "GROUPING_CODE"))

  names(.d) <- tolower(names(.d))
  .d <- mutate(.d,
    species_science_name = tolower(species_science_name),
    species_desc = tolower(species_desc),
    species_common_name = tolower(species_common_name))

  stopifnot(all(species_codes %in% .d$species_code))
  as_tibble(.d)
}

#' @export
#' @rdname get
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
  .d$usability <- ifelse(.d$usability == 1L, TRUE, FALSE) # TODO: test

  surveys <- run_sql("GFBioSQL", "SELECT * FROM SURVEY_SERIES")
  surveys <- select(surveys, -SURVEY_SERIES_TYPE_CODE)
  names(surveys) <- tolower(names(surveys))

  .d <- inner_join(.d, surveys, by = "survey_series_id")
  .d <- as_tibble(.d)

  if (length(.d$specimen_id) > length(unique(.d$specimen_id)))
    warning("Duplicate specimen IDs are present because of overlapping survey ",
      "stratifications. If working with the data yourelf, filter them after ",
      "selecting specific surveys. For example, ",
      "`dat <- dat[!duplicated(dat$specimen_id), ]`. ",
      "Tidying and plotting functions with PBSsynopsis will do this for you.")

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

#' @export
#' @rdname get
get_cpue_spatial_ll <- function(species) {
  .q <- read_sql("get-cpue-spatial-ll.sql")
  .q <- inject_species_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFFOS", .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$gear <- tolower(.d$gear)
  .d$fishery_sector <- tolower(.d$fishery_sector)
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
  .h <- xml2::read_html(
    "http://www.registrelep-sararegistry.gc.ca/sar/index/default_e.cfm")
  .d <- .h %>% rvest::html_nodes("table") %>%
    .[[1]] %>%
    rvest::html_table() %>%
    .[- (1:2), ] %>%
    dplyr::as_tibble() %>%
    dplyr::filter(.data$Taxon %in% "Fishes") %>%
    dplyr::filter(!grepl("Salmon", .data$`Common name *`))
  names(.d) <- tolower(names(.d))
  names(.d) <- gsub(" ", "_", names(.d))
  names(.d) <- gsub("_\\*", "", names(.d))
  as_tibble(.d)
}

#' @param path The folder where the cached data will be saved
#' @export
#' @rdname get
cache_pbs_data <- function(species, path = "data-cache") {

  if (!is_dfo_windows())
    stop("Not on a PBS windows machine. Cannot access data.")

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

  d <- get_cpue_spatial_ll(species)
  saveRDS(d, file = file.path(path, "pbs-cpue-spatial-ll.rds"))

  d <- get_surv_index(species)
  saveRDS(d, file = file.path(path, "pbs-surv-index.rds"))

  d <- get_age_precision(species)
  saveRDS(d, file = file.path(path, "pbs-age-precision.rds"))
}

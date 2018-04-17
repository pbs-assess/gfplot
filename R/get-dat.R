#' Get PBS data
#'
#' Automates fisheries and research survey data extraction from DFO Pacific
#' groundfish databases. The output datasets feed into other functions (eg.
#' tidy, plot, modelling) for data visualization, which can be used as
#' prodcuts themselves or can be fed into automated DFO Pacific groundfish
#' data synopsis report production.
#'
#' @details
#' * `get_ssids()` produces a lookup table for survey series IDs and
#'    descriptions
#' * `get_age_methods()` produces a lookup table for ageing method codes
#'    and descriptions
#' * `get_sample_trips()`produces a lookup table for sample ID and
#'    fishing event ID
#' * `get_strata_areas()` produces a lookup table for surveyed area for
#'    each stratum within surveys
#' * `get_survey_ids()` produces lookup table for survey IDs for a given
#'    survey series ID
#' * `get_survey_sets()` extracts survey catch data and spatial data for
#'    plotting survey catchs on a map of British Columbia.
#' * `get_survey_samples()` extracts all biological sample specimen records
#'    from research surveys for given species and survey series IDs from GFBio
#' * `get_comm_samples()` extracts all biological sample specimen records
#'    from commercial data for given species from GFBio
#' * `get_catch()` extracts all landing and discard records for a given species
#'    from GFFOS.GF_MERGED_CATCH
#' * `get_cpue_spatial()` extracts catch, effort and spatial data from
#'    GFFOS.GF_D_OFFICIAL_CATCH for the groundfish trawl fishery
#' * `get_cpue_spatial_ll()` extracts catch, effort and spatial data from
#'    GFFOS.GF_D_OFFICIAL_CATCH for the longline fishery
#' * `get_cpue_index()` extracts catch and effort data from
#'    GFFOS.GF_MERGED_CATCH for the groundfish trawl fishery
#' * `get_age_precision()` extracts age readings from biological samples for a
#'    given species where there is a second ('precision') age reading
#' * `get_sara_dat()` scrubs Species At Risk website for up-to-date species
#'    status and listings
#' * `get_survey_index()` extracts survey catch data for given species
#'    and survey series id's
#' * `cache_pbs_data()` runs all 'get' functions in the gfplot package
#'    and caches extracted data to a given folder
#'
#' @section Note:
#' `get_*` functions only extract data when performed on a computer connected to
#' the Pacific Biological Station DFO network. The `get_*` functions attempt to
#' detect this and will not run unless "PBS" is in `Sys.info()["nodename"]` and
#' the computer is a Windows computer.
#'
#' @family get data functions
#'
#' @examples
#' \dontrun{
#' get_survey_sets(species = "lingcod", ssid = 1)
#'
#' get_survey_samples(species = 442, ssid = c(1,3,4,16))
#' get_comm_samples(c(442, 397))
#'
#' species <- c("lingcod", "pacific cod")
#' get_catch(species)
#'
#' get_cpue_spatial(species)
#' get_cpue_spatial_ll("yelloweye rockfish")
#'
#' get_cpue_index(gear = "bottom trawl", min_year = 2012, max_year = 2017)
#'
#' get_cpue_spatial("pacific cod")
#' get_cpue_spatial_ll("arrowtooth flounder")
#' }
#'
#' @param species One or more species common names (e.g. `"pacific ocean
#'   perch"`) or one or more species codes (e.g. `396`). Species codes can be
#'   specified as numeric vectors `c(396, 442`) or characters `c("396", "442")`.
#'   Numeric values shorter than 3 digits will be expanded to 3 digits and
#'   converted to character objects (`1` turns into `"001"`). Species common
#'   names and species codes should not be mixed. If any element is missing a
#'   species code, then all elements will be assumed to be species common
#'   names.
#' @param ssid A numeric vector of survey series IDs. Run
#' \code{\link{get_ssids}} if you for a look-up table of available survey
#' series IDs with surveys series descriptions.
#' @name get
NULL

#' @export
#' @rdname get
get_ssids <- function() {
  .d <- run_sql(
    "GFBioSQL",
    "SELECT SURVEY_SERIES_ID, SURVEY_SERIES_DESC,
     CASE
       WHEN SURVEY_SERIES_ALT_DESC IS NULL THEN SURVEY_SERIES_TYPE_ALT_DESC
	     WHEN SURVEY_SERIES_TYPE_ALT_DESC IS NULL THEN SURVEY_SERIES_ALT_DESC
       WHEN SURVEY_SERIES_TYPE_ALT_DESC = 'OTHER' THEN SURVEY_SERIES_ALT_DESC
       ELSE (SURVEY_SERIES_TYPE_ALT_DESC + ' ' + SURVEY_SERIES_ALT_DESC)
       END AS SURVEY_ABBREV
     FROM SURVEY_SERIES SS
     INNER JOIN SURVEY_SERIES_TYPE SST ON
      SST.SURVEY_SERIES_TYPE_CODE = SS.SURVEY_SERIES_TYPE_CODE"
  )
  #names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname get
get_major_areas <- function() {
  .d <- run_sql(
    "GFFOS",
    "SELECT MAJOR_STAT_AREA_CODE, MAJOR_STAT_AREA_DESCRIPTION
    FROM MAJOR_STAT_AREA"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname get
get_age_methods <- function() {
  .d <- DBI::dbGetQuery(
    db_connection(database = "GFBioSQL"),
    "SELECT AGEING_METHOD_CODE, AGEING_METHOD_DESC, ROW_VERSION
    FROM AGEING_METHOD"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

get_sample_trips <- function() {
  run_sql(
    "GFBioSQL",
    "SELECT SPECIES_CODE, SAMPLE_ID, FISHING_EVENT_ID FROM B21_Samples"
  )
}

get_strata_areas <- function() {
  run_sql(
    "GFBioSQL",
    "SELECT SG.SURVEY_ID,
    SG.GROUPING_CODE,
    G.AREA_KM2
    FROM SURVEY_GROUPING SG
    INNER JOIN GROUPING G ON
    SG.GROUPING_CODE = G.GROUPING_CODE"
  )
}

get_survey_ids <- function(ssid) {
  .q <- paste(
    "SELECT S.SURVEY_ID,
    SS.SURVEY_SERIES_ID,
    SS.SURVEY_SERIES_DESC
    FROM SURVEY S
    INNER JOIN GFBioSQL.dbo.SURVEY_SERIES SS ON
    SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
    WHERE S.SURVEY_SERIES_ID IN (", paste(ssid, collapse = ", "), ")"
  )
  run_sql("GFBioSQL", .q)
}

#' @export
#' @param join_sample_ids TODO
#' @rdname get
get_survey_sets <- function(species, ssid = c(1, 3, 4, 16, 2, 14, 22, 36),
                            join_sample_ids = FALSE, verbose = FALSE) {
  trawl <- run_sql("GFBioSQL", "SELECT
    S.SURVEY_SERIES_ID
    FROM SURVEY_SERIES SS
    LEFT JOIN SURVEY S ON S.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
    LEFT JOIN TRIP_SURVEY TS ON TS.SURVEY_ID = S.SURVEY_ID
    LEFT JOIN FISHING_EVENT FE ON FE.TRIP_ID = TS.TRIP_ID
    WHERE GEAR_CODE IN(1, 6, 8, 11, 14, 16) AND
    S.SURVEY_SERIES_ID <> 0
    GROUP BY S.SURVEY_SERIES_ID, [SURVEY_SERIES_DESC]
    ,[SURVEY_SERIES_TYPE_CODE]
    ,[SURVEY_SERIES_ALT_DESC],
    TRAWL_IND, GEAR_CODE
    ORDER BY S.SURVEY_SERIES_ID")
  trawl <- unique(trawl$SURVEY_SERIES_ID)

  ll <- run_sql("GFBioSQL", "SELECT
    S.SURVEY_SERIES_ID
    FROM SURVEY_SERIES SS
    LEFT JOIN SURVEY S ON S.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
    LEFT JOIN TRIP_SURVEY TS ON TS.SURVEY_ID = S.SURVEY_ID
    LEFT JOIN FISHING_EVENT FE ON FE.TRIP_ID = TS.TRIP_ID
    WHERE GEAR_CODE IN(4,5,7,10,12) AND
    S.SURVEY_SERIES_ID <> 0
    GROUP BY S.SURVEY_SERIES_ID, [SURVEY_SERIES_DESC]
    ,[SURVEY_SERIES_TYPE_CODE]
    ,[SURVEY_SERIES_ALT_DESC],
    TRAWL_IND, GEAR_CODE
    ORDER BY S.SURVEY_SERIES_ID")
  ll <- unique(ll$SURVEY_SERIES_ID)

  missing <- setdiff(ssid, c(trawl, ll))
  if (length(missing) > 0) {
    stop("ssid(s) ", missing, " is/are not supported. Must be one of ",
      paste(sort(c(trawl, ll)), collapse = ", "), ". ",
      "See the function `get_ssids()` for help identifying ",
      "survey series IDs.",
      call. = FALSE
    )
  }

  species_codes <- common2codes(species)

  species_df <- run_sql("GFBioSQL", "SELECT * FROM SPECIES")
  sample_trip_ids <- get_sample_trips()
  areas <- get_strata_areas()
  survey_ids <- get_survey_ids(ssid)
  surveys <- get_ssids()

  fe <- run_sql("GFBioSQL", "SELECT
    FISHING_EVENT_ID,
    FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
    -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE, FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M
    FROM B21_Samples")

  d_survs <- list()
  k <- 0
  for (i in seq_along(species_codes)) {
    for (j in seq_along(survey_ids$SURVEY_ID)) {
      if (survey_ids$SURVEY_SERIES_ID[j] %in% trawl) {
        sql_proc <- "proc_catmat_2011"
      }
      if (survey_ids$SURVEY_SERIES_ID[j] %in% ll) {
        sql_proc <- "proc_catmat_ll_2013"
      }

      k <- k + 1
      if (verbose) {
        message("extracting data for survey ID ", survey_ids$SURVEY_ID[j],
          " and species code ", species_codes[i])
      }
      d_survs[[k]] <- DBI::dbGetQuery(
        db_connection(database = "GFBioSQL"),
        paste0(
          "EXEC ", sql_proc, " ", survey_ids$SURVEY_ID[j], ", '",
          species_codes[i], "'"
        )
      )
    }
  }
  .d <- bind_rows(d_survs)

  if (nrow(.d) < 1)
    stop("No survey set data for selected species.")

  .d <- inner_join(.d,
    unique(select(
      surveys,
      SURVEY_SERIES_ID,
      SURVEY_SERIES_DESC,
      SURVEY_ABBREV
    )),
    by = "SURVEY_SERIES_ID"
  )

  .d <- inner_join(.d,
    unique(select(
      species_df,
      SPECIES_CODE,
      SPECIES_COMMON_NAME,
      SPECIES_SCIENCE_NAME,
      SPECIES_DESC
    )),
    by = "SPECIES_CODE"
  )

  if (join_sample_ids) {
    # give us each sample_id associated with each fishing_event_id and species:
    .d <- left_join(.d, sample_trip_ids,
      by = c("SPECIES_CODE", "FISHING_EVENT_ID")
    ) %>%
      left_join(areas, by = c("SURVEY_ID", "GROUPING_CODE"))
  }

  names(.d) <- tolower(names(.d))
  .d <- mutate(.d,
    species_science_name = tolower(species_science_name),
    species_desc = tolower(species_desc),
    species_common_name = tolower(species_common_name)
  )

  missing_species <- setdiff(species_codes, .d$species_code)
  if (length(missing_species) > 0)
    warning("The following species codes do not have survey set data in GFBio.",
      paste(missing_species, collapse = ", "))
  as_tibble(.d)
}

#' @export
#' @rdname get
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'  length or weight values.
get_survey_samples <- function(species, ssid = NULL, remove_bad_data = TRUE,
  unsorted_only = TRUE) {
  .q <- read_sql("get-survey-samples.sql")
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
  if (!is.null(ssid)) {
    .q <- inject_filter("AND S.SURVEY_SERIES_ID IN", ssid,
      sql_code = .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)

    if (unsorted_only) {
    .d <- filter(.d, sampling_desc == 'UNSORTED')
  }

  if (length(.d$specimen_id) > length(unique(.d$specimen_id))) {
    warning(
      "Duplicate specimen IDs are present because of overlapping survey ",
      "stratifications. If working with the data yourelf, filter them after ",
      "selecting specific surveys. For example, ",
      "`dat <- dat[!duplicated(dat$specimen_id), ]`. ",
      "The tidying and plotting functions within gfplot will do this for you."
    )
  }

  # remove ages from unaccepted ageing methods:
  .d <- mutate(.d, age = ifelse(is.na(ageing_method), NA, age))

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

  as_tibble(.d)
}

#' @export
#' @param unsorted_only TODO
#' @rdname get
get_comm_samples <- function(species, unsorted_only = TRUE) {
  .q <- read_sql("get-comm-samples.sql")
  .q <- inject_filter("AND SM.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_science_name <- tolower(.d$species_science_name)
  .d <- mutate(.d, year = lubridate::year(trip_start_date))
  assertthat::assert_that(sum(duplicated(.d$specimen_id)) == 0)

  if (unsorted_only) {
    .d <- filter(.d, sampling_desc == 'UNSORTED')
  }

  # remove ages from unaccepted ageing methods:
  .d <- mutate(.d, age = ifelse(is.na(ageing_method), NA, age))

  as_tibble(.d)
}

#' @export
#' @rdname get
get_catch <- function(species) {
  .q <- read_sql("get-catch.sql")
  .q <- inject_filter("WHERE SP.SPECIES_CODE IN", species, sql_code = .q)
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
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
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
  .q <- inject_filter("AND SP.SPECIES_CODE IN", species, sql_code = .q)
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
  .q[i] <- paste0(
    "GEAR IN(", collapse_filters(toupper(gear)),
    ") AND YEAR(BEST_DATE) >= ", min_year, " AND "
  )
  .d <- run_sql("GFFOS", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

#' @export
#' @rdname get
get_age_precision <- function(species) {
  .q <- read_sql("get-age-precision.sql")
  .q <- inject_filter("AND C.SPECIES_CODE IN", species, .q)
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  as_tibble(.d)
}

#' @export
#' @rdname get
get_survey_index <- function(species, ssid = NULL) {
  .q <- read_sql("get-survey-index.sql")
  .q <- inject_filter("WHERE SP.SPECIES_CODE IN", species, .q)
  if (!is.null(ssid)) {
    .q <- inject_filter("AND BD.SURVEY_SERIES_ID IN", ssid, .q,
      search_flag = "-- insert ssid here", conversion_func = I
    )
  }
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
    "http://www.registrelep-sararegistry.gc.ca/sar/index/default_e.cfm"
  )
  .d <- .h %>%
    rvest::html_nodes("table") %>%
    .[[1]] %>%
    rvest::html_table() %>%
    .[-(1:2), ] %>%
    dplyr::as_tibble() %>%
    filter(.data$Taxon %in% "Fishes") %>%
    filter(!grepl("Salmon", .data$`Common name *`))
  names(.d) <- tolower(names(.d))
  names(.d) <- gsub(" ", "_", names(.d))
  names(.d) <- gsub("_\\*", "", names(.d))
  as_tibble(.d)
}

#' @param path The folder where the cached data will be saved
#' @param compress Compress the `.rds` file? Defaults to `FALSE` for faster
#'   reading and writing at the expense of disk space.
#' @param min_cpue_year Minimum year for the CPUE data.
#' @export
#' @rdname get
cache_pbs_data <- function(species, path = "data-cache", compress = FALSE,
                           min_cpue_year = 1996, unsorted_only = TRUE,
                           survey_sets = TRUE, verbose = TRUE) {
  if (!sql_server_accessible()) {
    stop("Not on a PBS windows machine. Cannot access data.")
  }

  dir.create(path, showWarnings = FALSE)

  if(survey_sets){
    d_survs_df <- get_survey_sets(species, join_sample_ids = TRUE, verbose = verbose)

    saveRDS(d_survs_df, file = file.path(path, "pbs-survey-sets.rds"),
            compress = compress)
  }

  d <- get_survey_samples(species)
  saveRDS(d, file = file.path(path, "pbs-survey-samples.rds"),
    compress = compress)

  d <- get_comm_samples(species, unsorted_only = unsorted_only)
  saveRDS(d, file = file.path(path, "pbs-comm-samples.rds"),
    compress = compress)

  d <- get_catch(species)
  saveRDS(d, file = file.path(path, "pbs-catch.rds"),
    compress = compress)

  d <- get_cpue_spatial(species)
  saveRDS(d, file = file.path(path, "pbs-cpue-spatial.rds"),
    compress = compress)

  d <- get_cpue_spatial_ll(species)
  saveRDS(d, file = file.path(path, "pbs-cpue-spatial-ll.rds"),
    compress = compress)

  d <- get_survey_index(species)
  saveRDS(d, file = file.path(path, "pbs-survey-index.rds"),
    compress = compress)

  d <- get_age_precision(species)
  saveRDS(d, file = file.path(path, "pbs-age-precision.rds"),
    compress = compress)

  d <- get_cpue_index(gear = "bottom trawl", min_year = min_cpue_year)
  saveRDS(d, file = file.path(path, "pbs-cpue-index.rds"),
    compress = compress)

  message("All data extracted and saved in the folder `", path, "`.")
}

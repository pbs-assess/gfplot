#' Get PBS data
#'
#' Automates fisheries and research survey data extraction from DFO Pacific
#' groundfish databases. The output datasets feed into other functions (`tidy_`,
#' `plot_`, or `fit_` functions) for data visualization, which can be used as
#' products themselves or can be fed into automated DFO Pacific groundfish data
#' synopsis report production.
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
#' * `get_commercial_samples()` extracts all biological sample specimen records
#'    from commercial data for given species from GFBio
#' * `get_catch()` extracts all landing and discard records for a given species
#'    from GFFOS.GF_MERGED_CATCH
#' * `get_cpue_spatial()` extracts catch, effort and spatial data from
#'    GFFOS.GF_D_OFFICIAL_CATCH for the groundfish trawl fishery
#' * `get_cpue_spatial_ll()` extracts catch, effort and spatial data from
#'    GFFOS.GF_D_OFFICIAL_CATCH for the longline fishery
#' * `get_cpue_index()` extracts catch and effort data from
#'    GFFOS.GF_MERGED_CATCH for the groundfish trawl fishery since 1996.
#' * [get_cpue_historical()] extracts historical catch and effort data back into
#'    the 1950s. It's help file is on a separate page; see the link.
#' * `get_age_precision()` extracts age readings from biological samples for a
#'    given species where there is a second ('precision') age reading
#' * `get_sara_dat()` scrubs Species At Risk website for up-to-date species
#'    status and listings
#' * `get_survey_index()` extracts survey catch data for given species
#'    and survey series IDs
#' * `get_iphc_sets()` extracts IPHC survey data at the set level for given
#'    species, from 2003 to present (excluding 2013 which is not in database)
#' * `get_iphc_sets_info()` extracts IPHC survey data regarding each set, with no
#'    species information, to give one unique row (with lat, lon etc.) for each
#'    set, from 2003 to present (excluding 2013 which is not in database)
#' * `get_iphc_skates_info()` extracts IPHC survey data regarding each skate,
#'    with no species information, to give one unique row (with lat, lon etc.)
#'    for each set, from 2003 to present (excluding 2013 which is not in database);
#'    needed for the hooks per skate
#' * `get_iphc_hooks()` extracts IPHC survey data at the hook level for given
#'    species, from 2003 to present (excluding 2013 which is not in database)
#' * `cache_pbs_data()` runs all 'get' functions in the gfplot package
#'    and caches extracted data to a given folder
#'
#' @section Note:
#' `get_*` functions only extract data when performed on a computer connected to
#' the Pacific Biological Station DFO network.
#'
#' @examples
#' \dontrun{
#' ## Import survey catch density and location data by tow or set for plotting
#' ## Specify single or multiple species by common name or species code and
#' ## single or multiple survey series id(s).
#' get_survey_sets(species = "lingcod", ssid = 1)
#'
#' ## Import survey or commercial biological data for various plots
#' (eg. length frequency, growth, age frequency, maturity, etc.)
#' get_survey_samples(species = 442, ssid = c(1, 3, 4, 16))
#'
#' get_commercial_samples(c(442, 397))
#'
#' ## Import catch data by species for barcharts of landings by fishing area,
#' ## geartype, and year.
#' get_catch("lingcod")
#'
#' ## Import spatial commercial catch per unit effort data for trawl or longline
#' ## data by species for plotting along BC coast.
#' get_cpue_spatial("lingcod")
#' get_cpue_spatial_ll("yelloweye rockfish")
#'
#' ## Import catch and effort data by gear type for modelling commercial trawl
#' ## cpue index.
#' get_cpue_index(gear = "bottom trawl", min_cpue_year = 2012)
#'
#' ## Import survey bootstrapped biomass estimates for plotting relative biomass
#' ## indices by specified survey series.
#' get_survey_index("pacific cod", ssid = c(1, 3, 4, 16))
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
#' @param ssid A numeric vector of survey series IDs. Run [get_ssids()] for a
#'   look-up table of available survey series IDs with surveys series
#'   descriptions.
#' @name get_data
NULL

#' @export
#' @rdname get_data
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
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
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
#' @rdname get_data
get_management_areas <- function() {
  .d <- run_sql(
    "PacManagement",
    "SELECT Area_Code, Area_Description
    FROM Area"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_fishery_ids <- function() {
  .d <- run_sql(
    "PacManagement",
    "SELECT Fishery_Id, Fishery_Description
    FROM Fishery"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_species_groups <- function() {
  .d <- run_sql(
    "PacManagement",
    "SELECT SG.Species_Group_Code, Species_Group_Description, Species_Code,
      Species_Common_Name
    FROM Species_Group SG
      INNER JOIN Species_Group_Species SGS ON
        SG.Species_Group_Code = SGS.Species_Group_Code"
  )
  names(.d) <- tolower(names(.d))
  .d <- unique(.d)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_gear_types <- function() {
  .d <- run_sql(
    "GFFOS",
    "SELECT GEAR
    FROM GFFOS.dbo.GF_MERGED_CATCH C
    GROUP BY GEAR"
  )
  names(.d) <- tolower(names(.d))
  .d$gear <- tolower(.d$gear)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
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
#' @rdname get_data
get_other_surveys <- function() {
  .q <- read_sql("get-other-surveys.sql")
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d <- .d %>% filter(count_surveys_since_2008 >1) %>%
    select(survey_series_desc, count_surveys_since_2008)
  .d
  }

#' @export
#' @param join_sample_ids If `TRUE` then the sample IDs will be joined in. This
#'   may result in repeated rows of data if the same sample ID is part of
#'   different survey stratifications.
#' @param verbose If `TRUE` then extra messages were reprinted during data
#'   extraction. Useful to monitor progress.
#' @rdname get_data
get_survey_sets <- function(species, ssid = c(1, 3, 4, 16, 2, 14, 22, 36),
  join_sample_ids = FALSE, verbose = FALSE) {
# Just to pull out up to date list of ssids associated with trawl/ll gear type.
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
    -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS
      LONGITUDE, FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M
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
#' @rdname get_data
#' @param remove_bad_data Remove known bad data, such as unrealistic
#'  length or weight values.
#' @param usability A vector of usability codes to include. Defaults to all.
#'   IPHC codes may be different to other surveys.
get_survey_samples <- function(species, ssid = NULL, remove_bad_data = TRUE,
  unsorted_only = TRUE, usability = NULL) {
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

  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)
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
#' @rdname get_data
get_iphc_sets <- function(species, usability = NULL) {
  .q <- read_sql("get-iphc-set-level.sql")
  .q <- inject_filter("AND C.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFBioSQL", .q)
  .d$species <- tolower(.d$species)
#
#    if (!is.null(usability)) {
#     .d <- filter(.d, usability_code %in% usability)
#   }
  as_tibble(.d)
}

##' @rdname get_data
##' @export
get_iphc_sets_info <- function() {
  .q <- read_sql("get-iphc-set-info.sql")
  .d <- run_sql("GFBioSQL", .q)
  .d <- mutate(.d, usable = ifelse(iphcUsabilityCode %in% c(1, 52),
                                   "Y", "N"))
  as_tibble(.d)
}

##' @rdname get_data
##' @export
get_iphc_skates_info <- function() {
  .q <- read_sql("get-iphc-skate-info.sql")
  .d <- run_sql("GFBioSQL", .q)
  # Calculating here:
  # lastHook - hook number of final hook on that skate
  # firstHook - hook number of 1st hook on that skate
  # hook20 - hook number of 20th hook on that skate
  # lastHookTemp needed since need before firstHook, but want to re-order
  # chum... are just the chum-bait ones
  # Hook numbering starts at 1 for each set until 2006 (and so goes 1-800ish),
  #  then resets at 1 for each skate for each skate for 2007 onwards (and so
  #  goes 1-100ish). Hence the <2006.5 in lastHookTemp.
  .d <- mutate(group_by(.d, setID),
               lastHookTemp = cumsum(deplHooksPerSkate) * (year < 2006.5) +
                              deplHooksPerSkate * (year > 2006.5),
               firstHook = lastHookTemp - deplHooksPerSkate + 1,
               lastHook = lastHookTemp,
               hook20 = firstHook +
                        (deplHooksPerSkate < 19.5) * deplHooksPerSkate +
                        (deplHooksPerSkate > 19.5) * 20 - 1,
               chumDeplHooksPerSkate = deplHooksPerSkate * (bait == 110),
               chumObsHooksPerSkate = obsHooksPerSkate * (bait == 110),
               obsHooksPerSkate20 = 20,  # **Remove this line when Elise updates
                                         #  sql query, Issue #40
               chumObsHooksPerSkate20 = obsHooksPerSkate20 * (bait == 110)
               ) %>%
               select(-lastHookTemp)
  as_tibble(.d)
}


#' @export
#' @rdname get_data
get_iphc_hooks <- function(species, usability = NULL) {
  .q <- read_sql("get-iphc-hook-level.sql")
  .q <- inject_filter("AND C.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFBioSQL", .q)
  .d$species <- tolower(.d$species)
#
#    if (!is.null(usability)) {
#     .d <- filter(.d, usability_code %in% usability)
#   }
  as_tibble(.d)
}


#' @export
#' @param unsorted_only Remove sorted biological data ('keepers' and 'discards'
#'  and unknown). Default = TRUE.
#' @rdname get_data
get_commercial_samples <- function(species, unsorted_only = TRUE,
  usability = NULL) {
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

  if (!is.null(usability)) {
    .d <- filter(.d, usability_code %in% usability)
  }

  # remove ages from unaccepted ageing methods:
  .d <- mutate(.d, age = ifelse(is.na(ageing_method), NA, age))

  as_tibble(.d)
}

get_comm_samples <- function(...) {
  warning("Depreciated: please use get_commercial_samples() instead.")
  get_commercial_samples(...)
}

#' @export
#' @rdname get_data
get_catch <- function(species) {
  .q <- read_sql("get-catch.sql")
  .q <- inject_filter("WHERE SP.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql("GFFOS", .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  .d$species_scientific_name <- tolower(.d$species_scientific_name)
  .d$year <- lubridate::year(.d$best_date)
  as_tibble(.d)
}

#' @param ... Other arguments to pass.
#' @rdname get_cpue_historical
#' @export
get_cpue_historic <- function(...) {
  warning("get_cpue_historic() is depreciated. Please use the grammatically",
    "correct get_cpue_historical() instead.")
  get_cpue_historical(...)
}

#' Get all fishing catch and effort to calculate historic commercial CPUE
#'
#' @param species Species to filter for positive fishing events. Leave as `NULL`
#'   to include all fishing events. One or more species common names (e.g.
#'   `"pacific ocean perch"`) or one or more species codes (e.g. `396`). Species
#'   codes can be specified as numeric vectors `c(396, 442`) or characters
#'   `c("396", "442")`. Numeric values shorter than 3 digits will be expanded to
#'   3 digits and converted to character objects (`1` turns into `"001"`).
#'   Species common names and species codes should not be mixed. If any element
#'   is missing a species code, then all elements will be assumed to be species
#'   common names.
#' @param alt_year_start_date Alternative year starting date specified as a
#'   month-day combination. E.g. "03-01" for March 1st. Can be used to create
#'   'fishing years'.
#' @param areas Area groupings as a vector of regular expressions.
#'   See [base::regex()].
#' @param end_year Specify the last calendar year to be extracted.
#' @export
get_cpue_historical <- function(species = NULL,
  alt_year_start_date = "04-01", areas = c("3[CD]+", "5[AB]+", "5[CDE]+"),
  end_year = NULL) {
  .q <- read_sql("get-cpue-historic.sql")
  if (!is.null(species))
    .q <- inject_filter("AND MC.SPECIES_CODE IN", species, sql_code = .q)
  .d <- run_sql(database = c("GFFOS", "GFCatch", "PacHarvest"), .q)
  .d$SPECIES_COMMON_NAME[.d$SPECIES_COMMON_NAME == "SPINY DOGFISH"] <-
    toupper("north pacific spiny dogfish") # to match GFBioSQL
  names(.d) <- tolower(names(.d))
  .d <- rename(.d, total = totcatch_kg, minor_stat_area_code = min)
  .d$hours_fished <- as.numeric(as.character(.d$hours_fished))
  .d$database_name <- tolower(.d$database_name)
  .d$gear <- tolower(.d$gear)
  .d$locality_description <- tolower(.d$locality_description)


  # Filter out fishing records after last year required
  if (!is.null(end_year)){
    .d <- .d %>% filter(year <= end_year)
  }

  # Create possibly alternate starting date:

  if (alt_year_start_date != "01-01") {
    .d <- dplyr::mutate(.d, .year_start_date =
        lubridate::ymd_hms(paste0(year, "-", alt_year_start_date, " 00:00:00")))
    .d <- dplyr::mutate(.d, .time_diff = best_date - .year_start_date)
    .d <- dplyr::mutate(.d, alt_year = ifelse(.time_diff > 0, year, year - 1L))
    .d <- dplyr::select(.d, -.time_diff, -.year_start_date)
  }

  .d$area <- assign_areas(.d$major_stat_area_description, areas)
  .d$specific_area <- assign_areas(.d$major_stat_area_description,
    area_regex = c("3C", "3D", "5A", "5B", "5C", "5D", "5E"))

  as_tibble(.d)
}

#' @export
#' @rdname get_data
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
#' @rdname get_data
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

#' @param gear The gear type(s) to include for CPUE. Will be converted to
#'  uppercase. Run [get_gear_types()] for a look-up table of available
#'  gear types to select from.
#' @param min_cpue_year Minimum year for the CPUE data.
#' @export
#' @rdname get_data
get_cpue_index <- function(gear = "bottom trawl", min_cpue_year = 1996) {
  .q <- read_sql("get-cpue-index.sql")
  i <- grep("-- insert filters here", .q)
  .q[i] <- paste0(
    "GEAR IN(", collapse_filters(toupper(gear)),
    ") AND YEAR(BEST_DATE) >= ", min_cpue_year, " AND "
  )
  .d <- run_sql("GFFOS", .q)
  names(.d) <- tolower(names(.d))
  as_tibble(.d)
}

#' @export
#' @rdname get_data
get_age_precision <- function(species) {
  .q <- read_sql("get-age-precision.sql")
  .q <- inject_filter("AND C.SPECIES_CODE IN", species, .q)
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  as_tibble(.d)
}

#' @export
#' @rdname get_data
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

#' @param species_group Species group code(s) to include (see lookup table
#'   [get_species_groups()]). Defaults to all.
#' @param fishery The fishery_id code(s) (see lookup table [get_fishery_ids()])
#'   for fisheries to include in data extraction. Defaults to all.
#' @param area The fishery area(s) (see lookup table [get_management_areas()])
#'   to include in data extraction (eg. '5A'; c('3C', '3D', '5A', '5B')).
#' @param start_year The minimum year to include management actions.
#'   Defaults to all.
#' @export
#' @rdname get_data
get_management <- function(species = NULL, species_group = NULL, fishery = NULL,
  area = NULL, start_year = NULL) {
  .q <- read_sql("get-management.sql")
  if (!is.null(species)) {
    .q <- inject_filter("AND Species_Code IN", species, .q,
      search_flag = "-- insert species here", conversion_func = common2codes
    )
  }
  if (!is.null(fishery)) {
    .q <- inject_filter("AND M.Fishery_Id IN", fishery, .q,
      search_flag = "-- insert fishery here", conversion_func = I
    )
  }
  if (!is.null(species_group)) {
    .q <- inject_filter("AND M.Species_Group_Code IN", species_group, .q,
      search_flag = "-- insert species group here", conversion_func = I
    )
  }
  if (!is.null(area)) {
    .q <- inject_filter("AND MA.Area_Code IN", area, .q,
      search_flag = "-- insert area here", conversion_func = I
    )
  }
  if (!is.null(start_year)) {
    .q <- inject_filter("AND YEAR(Action_Start_Date) >=", start_year, .q,
      search_flag = "-- insert start year here", conversion_func = I
    )
  }
  .d <- run_sql("GFBioSQL", .q)
  names(.d) <- tolower(names(.d))
  .d$species_common_name <- tolower(.d$species_common_name)
  as_tibble(.d)
  .d %>% arrange(dplyr::desc(action_start_date))
}

#' @export
#' @rdname get_data
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

#' @param path The folder where the cached data will be saved.
#' @param compress Compress the `.rds` file? Defaults to `FALSE` for faster
#'   reading and writing at the expense of disk space.
#' @param historic_cpue Logical for whether historical CPUE should be included.
#' @param survey_sets Logical for whether the survey set data should be
#'   extracted. You might set this to `FALSE` if you don't need these data and
#'   you want to substantially speed up data extraction.
#' @export
#' @return The `get_*` functions return a data frame. The [cache_pbs_data()]
#' function writes an `.rds` file to `path` for each specified species. A data
#' object for a single species is a named list object with each element
#' containing a data frame from a `get_*` function. The element name of the list
#' reflects the function name with the `get_` part removed. For example, the
#' output from [get_survey_samples()] is in a list element named
#' `survey_samples()`.
#' @details
#' This [cache_pbs_data()] function caches data from
#' * [get_survey_samples()]
#' * [get_commercial_samples()]
#' * [get_catch()]
#' * [get_cpue_spatial()]
#' * [get_cpue_spatial_ll()]
#' * [get_survey_index()]
#' * [get_age_precision()]
#' * and optionally from [get_survey_sets()] and [get_cpue_historic()]

#' @rdname get_data
cache_pbs_data <- function(species, path = ".", compress = FALSE,
  unsorted_only = TRUE, historic_cpue = FALSE,
  survey_sets = FALSE, verbose = TRUE) {

  if (!sql_server_accessible()) {
    stop("SQL server is not accessible. Either you are not on the DFO network or your",
         " Rprofile file does not contain the required variables options pbs.ip,",
         " pbs.uid, and pbs.pwd. On Windows, this file is located at",
         " C:\\R\\etc\\Rprofile.site")
  }
  dir.create(path, showWarnings = FALSE)

  for (sp_i in seq_along(species)) {
    this_sp <- species[[sp_i]]
    this_sp_clean <- gsub("/", "-", gsub(" ", "-", this_sp))
    message("Extracting data for ", this_sp)

    out <- list()
    if (survey_sets){
      out$survey_sets      <- get_survey_sets(this_sp, join_sample_ids = TRUE,
        verbose = verbose)
    }
    if (historic_cpue) {
      out$cpue_historic    <- get_cpue_historic(this_sp)
    }
    out$survey_samples     <- get_survey_samples(this_sp)
    out$commercial_samples <- get_commercial_samples(this_sp, unsorted_only = unsorted_only)
    out$catch              <- get_catch(this_sp)
    out$cpue_spatial       <- get_cpue_spatial(this_sp)
    out$cpue_spatial_ll    <- get_cpue_spatial_ll(this_sp)
    out$survey_index       <- get_survey_index(this_sp)
    out$management         <- get_management(this_sp)
    out$age_precision      <- get_age_precision(this_sp)

    saveRDS(out, file = paste0(file.path(path, this_sp_clean), ".rds"),
      compress = compress)
  }

  message("All data extracted and saved in the folder `", path, "`.")
}

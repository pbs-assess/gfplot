#' Tidy age or length composition data for plotting or modelling
#'
#' These functions tidy, filter, and calculate frequencies for ages or lengths
#' over time for survey or commercial data. They can calculate raw frequencies
#' or frequencies in which the samples are weighted. See [weight_comps()] for
#' details on the weighting procedure.
#'
#' @param dat The input samples data frame from [gfdata::get_commercial_samples()] or
#'   [gfdata::get_survey_samples()].
#' @param survey A character vector of survey names to use. These should match
#'   the survey abbreviations in GFBio. All of the survey listed here will be
#'   rendered in the final plot in the order that they are specified to this
#'   argument.
#' @param year_range An optional range of years to plot.
#' @param spp_cat_code A numeric vector of species category codes to include
#'   for the commercial samples. Defaults to `1`, which refers to unsorted
#'   samples.
#' @param area_grep_pattern A [grep()] pattern to match the major statistical
#'   area descriptions. The pattern `"*"` will return all areas. For example,
#'   `"5[CDE]+"` would return areas 5C, 5D, and 5E. See [base::regex()].
#' @param ageing_method_codes A numeric vector of ageing method codes to filter
#'   on. Default to `NULL`, which brings in all valid ageing codes.
#'   See [gfdata::get_age_methods()].
#' @param usability_codes An optional vector of usability codes.
#'   All usability codes not in this vector will be omitted.
#'   Set to `NULL` to include all samples.
#' @param bin_size Bin size for length binning.
#' @param age_length Should the function operate on ages or lengths?
#' @param sample_type Are the samples from a commercial or survey source?
#' @param frequency_type Should the frequencies or proportions be based on raw
#'   value or with weighted samples?
#' @param dat_survey_sets A data frame from `gfdata::get_survey_sets(..., join_sample_ids = TRUE)`. Needed for
#'   weighted samples if `sample_type = "survey"`.
#' @param dat_catch A data frame from [gfdata::get_catch()]. Needed for weighted samples
#'   if `sample_type = "commercial"`.
#' @param remove_unsexed Logical
#'
#' @export
#' @details
#'
#' The function `tidy_comps()` is the main workhorse function, but as a user you
#' can use the helper functions `tidy_ages_raw()`, `tidy_ages_weighted()`,
#' `tidy_lengths_raw()`, and `tidy_lengths_weighted()`. These functions
#' simply call `tidy_comps()` with appropriate argument values for `age_length`
#' and `frequency_type`.
#'
#' Note that the `length_bin` column will contain the mid value of that length
#' bin. E.g. `13` with `bin_size = 2` would represent a bin from `12` to `14`.
#'
#' @examples
#' \dontrun{
#'
#' # # extract data with get_*() functions:
#' # # main age/length data:
#' # rs_comm_samples <- gfdata::get_commercial_samples("redstripe rockfish",
#' #   discard_keepers = TRUE)
#' # rs_survey_samples <- gfdata::get_survey_samples("redstripe rockfish")
#' #
#' # # for weighting:
#' # rs_catch <- gfdata::get_catch("redstripe rockfish")
#' # rs_survey_sets <- gfdata::get_survey_sets("redstripe rockfish")
#'
#' # calculate raw age frequencies for survey data:
#' tidy_ages_raw(rs_survey_samples,
#'   sample_type = "survey")
#'
#' # calculate weighted age frequencies for survey data:
#' tidy_ages_weighted(rs_survey_samples,
#'   sample_type = "survey",
#'   dat_survey_sets = rs_survey_sets)
#'
#' # calculate raw length frequencies for survey data:
#' tidy_lengths_raw(rs_survey_samples,
#'   sample_type = "survey",
#'   bin_size = 2)
#'
#' # calculate raw age frequencies for commercial data:
#' tidy_ages_raw(rs_comm_samples,
#'   sample_type = "commercial")
#'
#' # calculate weighted age frequencies for commercial data:
#' tidy_ages_weighted(rs_comm_samples,
#'   sample_type = "commercial",
#'   dat_catch = rs_catch)
#'
#' # calculate weighted length frequencies for commercial data:
#' tidy_lengths_weighted(rs_comm_samples,
#'   sample_type = "commercial",
#'   bin_size = 2,
#'   dat_catch = rs_catch)
#' }
#' @name tidy_comps
NULL

#' @export
#' @param ... Arguments to pass to `tidy_comps()`.
#' @rdname tidy_comps
tidy_ages_raw <- function(...) {
  tidy_comps(..., age_length = "age", frequency_type = "raw")
}

#' @export
#' @rdname tidy_comps
tidy_ages_weighted <- function(...) {
  tidy_comps(..., age_length = "age", frequency_type = "weighted")
}

#' @export
#' @rdname tidy_comps
tidy_lengths_raw <- function(...) {
  tidy_comps(..., age_length = "length", frequency_type = "raw")
}

#' @export
#' @rdname tidy_comps
tidy_lengths_weighted <- function(...) {
  tidy_comps(..., age_length = "length", frequency_type = "weighted")
}

#' @export
#' @rdname tidy_comps
#' @param ... Arguments to pass to [set_fishing_year()]
tidy_comps <- function(dat,
                       survey = c(
                         "SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI", "HBLL OUT N",
                         "HBLL OUT S", "IPHC FISS"
                       ),
                       year_range = NULL,
                       spp_cat_code = 1,
                       area_grep_pattern = "*",
                       ageing_method_codes = NULL,
                       usability_codes = c(0, 1, 2, 6),
                       bin_size = 2,
                       age_length = c("age", "length"),
                       sample_type = c("survey", "commercial"),
                       frequency_type = c("raw", "weighted"),
                       dat_survey_sets = NULL,
                       dat_catch = NULL,
                       remove_unsexed = TRUE,
                       ...) {

  if(sample_type == "commercial" && !is.null(dat_catch)){
    dat_catch <- set_fishing_year(dat_catch, ...)
  }

  age_length <- match.arg(age_length)
  sample_type <- match.arg(sample_type)
  frequency_type <- match.arg(frequency_type)

  if (frequency_type == "weighted" && sample_type == "survey" &&
    is.null(dat_survey_sets)) {
    stop("A data frame must be supplied for `dat_survey_sets` ",
      "if `frequency_type = 'weighted'`. ",
      "Extract it with `gfdata::get_survey_sets('your species')`.",
      call. = FALSE
    )
  }
  if (frequency_type == "weighted" && sample_type == "commercial" &&
    is.null(dat_catch)) {
    stop("A data frame must be supplied for `dat_catch` ",
      "if `frequency_type = 'weighted'`. ",
      "Extract it with `gfdata::get_catch('your species')`.",
      call. = FALSE
    )
  }

  # -------------------------------------------
  # Filter down data (basics):
  if (remove_unsexed) {
    dat <- filter(dat, sex %in% c(1, 2))
  }
  dat <- filter(dat, !is.na(year))
  dat$sex <- dplyr::case_when(
    dat$sex == 1 ~ "M",
    dat$sex == 2 ~ "F",
    TRUE ~ "U"
  )

  if (is.null(year_range)) year_range <- c(min(dat$year), max(dat$year))
  dat <- filter(dat, year >= min(year_range), year <= max(year_range))

  # -------------------------------------------
  # Filter down usability codes:
  if (!is.null(usability_codes)) {
    dat <- filter(dat, .data$usability_code %in% usability_codes)
  }

  # -------------------------------------------
  # Filter down data (commercial):
  if (sample_type == "commercial") {
    dat <- filter(dat, species_category_code %in% spp_cat_code)
    if (nrow(dat) == 0) {
      warning("No data available.", call. = FALSE)
      return(NA)
    }
    dat$survey_abbrev <- "Commercial"

    pbs_areas <- gfplot::pbs_areas[grep(
      area_grep_pattern,
      gfplot::pbs_areas$major_stat_area_description
    ), , drop = FALSE]
    dat <- semi_join(dat, pbs_areas, by = "major_stat_area_code")
  }

  # -------------------------------------------
  # Filter down data (survey):
  if (sample_type == "survey") {
    dat <- filter(dat, survey_abbrev %in% survey)
    if (nrow(dat) == 0) {
      warning("No data available.", call. = FALSE)
      return(NA)
    }
  }

  # -------------------------------------------
  # Filter down data (ages):
  if (age_length == "age") {
    if (!is.null(ageing_method_codes)) {
      dat <- filter(dat, ageing_method %in% ageing_method_codes)
    }
    dat <- filter(dat, !is.na(age))
    if (nrow(dat) == 0) {
      warning("No data available.", call. = FALSE)
      return(NA)
    }
  }

  # -------------------------------------------
  # Filter down data (lengths):
  if (age_length == "length") {
    dat <- filter(dat, !is.na(.data$length))
    if (nrow(dat) == 0) {
      warning("No data available.", call. = FALSE)
      return(NA)
    }
  }

  dat <- dat[!duplicated(dat$specimen_id), , drop = FALSE] # critical

  # -------------------------------------------
  # Retain only necessary columns:
  if (frequency_type == "raw") {
    dat <- dat %>%
      select(
        species_common_name, survey_abbrev, year, sex, age,
        .data$length
      )
  } else {
    if (sample_type == "survey") {
      dat <- dat %>%
        select(
          species_common_name, survey_abbrev, year, sex, age,
          .data$length, sample_id, grouping_code, survey_id
        )
    } else {
      dat <- dat %>%
        select(
          species_common_name, survey_abbrev, year, sex, age,
          .data$length, sample_id, trip_start_date, trip_id, catch_weight
        )
    }
  }

  # -------------------------------------------
  # Assign survey levels:
  if (sample_type == "survey") {
    dat$survey_abbrev <- factor(dat$survey_abbrev, levels = survey)
  } else {
    dat$survey_abbrev <- "Commercial"
  }

  # -------------------------------------------
  # Calculate the actual age or length frequencies:

  # -------------------------------------------
  # Raw (commercial or survey):
  if (age_length == "age" && frequency_type == "raw") {
    freq <- dat %>%
      group_by(species_common_name, year, age, sex, survey_abbrev) %>%
      summarise(n = n()) %>%
      group_by(year, survey_abbrev) %>%
      mutate(proportion = n / sum(n)) %>%
      group_by(survey_abbrev) %>%
      mutate(proportion = proportion / max(proportion)) %>%
      select(-n) %>%
      ungroup()
  }

  if (age_length == "length" && frequency_type == "raw") {
    freq <- dat %>%
      dplyr::do(bin_lengths(dat, value = length, bin_size = bin_size)) %>%
      rename(length_bin = .data$length) %>%
      group_by(species_common_name, year, length_bin, sex, survey_abbrev) %>%
      summarise(n = n()) %>%
      group_by(year, survey_abbrev) %>%
      mutate(proportion = n / sum(n)) %>%
      select(-n) %>%
      ungroup()
  }

  # -------------------------------------------
  # Weighted + survey:
  if (age_length == "age" && frequency_type == "weighted" &&
    sample_type == "survey") {
    freq <- dat %>%
      group_by(species_common_name, survey_abbrev) %>%
      dplyr::do(tidy_comps_survey(., dat_survey_sets, value = age)) %>%
      # -(1:2) excludes the 'species' and 'survey_abbrev' columns from weight_comps():
      dplyr::do(weight_comps(.[, -(1:2)])) %>%
      ungroup()
  }

  if (age_length == "length" && frequency_type == "weighted" &&
    sample_type == "survey") {
    freq <- dat %>%
      group_by(species_common_name, survey_abbrev) %>%
      dplyr::do(tidy_comps_survey(., dat_survey_sets,
        value = length, bin_size = bin_size
      )) %>%
      dplyr::do(weight_comps(.[, -(1:2)])) %>%
      ungroup()
  }

  # -------------------------------------------
  # Weighted + commercial:
  if (age_length == "age" && frequency_type == "weighted" &&
    sample_type == "commercial") {
    freq <- dat %>%
      group_by(species_common_name, survey_abbrev) %>%
      dplyr::do(tidy_comps_commercial(., dat_catch, value = age)) %>%
      dplyr::do(weight_comps(.[, -(1:2)])) %>%
      ungroup()
  }

  if (age_length == "length" && frequency_type == "weighted" &&
    sample_type == "commercial") {
    freq <- dat %>%
      group_by(species_common_name, survey_abbrev) %>%
      dplyr::do(tidy_comps_commercial(., dat_catch,
        value = length, bin_size = bin_size
      )) %>%
      dplyr::do(weight_comps(.[, -(1:2)])) %>%
      ungroup()
  }

  if (age_length == "age" && frequency_type == "weighted") {
    freq <- rename(freq, age = value)
  }
  if (age_length == "length" && frequency_type == "weighted") {
    freq <- rename(freq, length_bin = value)
  }
  if (frequency_type == "weighted") {
    freq <- rename(freq, proportion = weighted_prop)
  }

  # -------------------------------------------
  # Join in the counts for labels:
  if (age_length == "age") {
    counts <- group_by(dat, year, species_common_name, survey_abbrev) %>%
      summarise(total = n()) %>%
      ungroup()
    freq <- left_join(freq, counts,
      by = c("species_common_name", "year", "survey_abbrev")
    )
    freq <- select(
      freq, species_common_name, survey_abbrev, year, sex, age,
      proportion, total
    )
  }

  if (age_length == "length") { # also by year
    counts <- group_by(dat, year, species_common_name, survey_abbrev) %>%
      summarise(total = n()) %>%
      ungroup()
    freq <- left_join(freq, counts,
      by = c("species_common_name", "year", "survey_abbrev")
    )
    freq <- select(
      freq, species_common_name, survey_abbrev, year, sex, length_bin,
      proportion, total
    )
  }

  arrange(freq, species_common_name, survey_abbrev, year, sex)
}

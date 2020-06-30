#' PBS groundfish areas
#'
#' A dataset containing the major statistical area codes and descriptions.
#'
#' @format A data frame:
#' \describe{
#'   \item{major_stat_area_code}{major statistical area code}
#'   \item{major_stat_area_description}{major statistical area description}
#' }
"pbs_areas"

#' PBS groundfish species
#'
#' A dataset containing species codes and names. Used in [tidy_cpue_index()].
#'
#' @format A data frame:
#' \describe{
#'   \item{species_code}{species code}
#'   \item{species_scientific_name}{species scientific name}
#'   \item{species_common_name}{species common name}
#' }
"pbs_species"

#' Example Pacific Ocean Perch biological samples from QCS.
#'
#' Originally retrieved with `get_survey_samples("pacific ocean perch")`. Has
#' been filtered in various ways to condense the data set for use in examples
#' throughout the package.
#'
#' @format A data frame
"pop_samples"

#' Example Pacific Ocean Perch survey data from QCS in 2015
#'
#' Originally retrieved with `get_survey_sets("pacific ocean perch")`.
#'
#' @format A data frame
"pop_surv"

#' A 2km x 2km grid for the HBLL survey
#'
#' A grid that extends over the domain of the hard bottom long line (PHMA)
#' survey. For use with [plot_survey_sets()].
#'
#' @format Data
"hbll_grid"

#' A 2km x 2km grid for the HBLL Outside North survey
#'
#' @format Data
"hbll_n_grid"

#' A 2km x 2km grid for the HBLL Outside South survey
#'
#' @format Data
"hbll_s_grid"

#' A 2km x 2km grid for the HBLL Inside North survey
#'
#' @format Data
"hbll_inside_n_grid"

#' A 2km x 2km grid for the HBLL Inside South survey
#'
#' @format Data
"hbll_inside_s_grid"

#' Survey boundaries
#'
#' Survey domain polygons for the synoptic trawl surveys. For use with
#' [plot_survey_sets()].
#'
#' @format A data frame
"survey_boundaries"

#' Synoptic grid
#'
#' The official synoptic survey grid.
#'
#' @format A data frame
"synoptic_grid"

#' A grid for dogfish survey
#'
#' @format Data
"dogfish_grid"

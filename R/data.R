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

#' A 2km x 2km grid for the HBLL North survey
#'
#' @format Data
"hbll_n_grid"

#' A 2km x 2km grid for the HBLL South survey
#'
#' @format Data
"hbll_s_grid"

#' Survey boundaries
#'
#' Survey domain polygons for the synoptic trawl surveys. For use with
#' [plot_survey_sets()].
#'
#' @format A data frame
"survey_boundaries"

#' Station data for 1995 IPHC survey
#'
#' A dataset containing details of the Setline Grid stations for the 1995 IPHC
#'  survey; one set was deployed at each station.
#'
#' @format A tibble:
#' \describe{
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{station}{station name}
#'   \item{effSkate}{effective skate number of the set, as calculated by IPHC}
#'   \item{usable}{whether or not the set is usable, as determined by IPHC plus
#'                 four extra that had no hooks enumerated except for Pacific
#'                 Halibut, and so are not usable by us}
#'
#' }
#' @source Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
#'         received from Aaron Ranta at the IPHC.
"setData1995"

#' Count data for 1995 IPHC survey
#'
#' A dataset containing counts of each species for each Setline Grid station for
#' the 1995 IPHC survey.
#'
#' @format A tibble:
#' \describe{
#'   \item{year}{1995 for this data set}
#'   \item{station}{station name, to link with setData1995}
#'   \item{specName}{species name for the counts for that set}
#'   \item{specCount}{count of that species for that set}
#' }
#' @source Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
#'         received from Aaron Ranta at the IPHC.
"countData1995"

#' Station data for 1995 IPHC survey for RS stations
#'
#' A dataset containing details of the RS (Random Sample?) stations for the 1995
#' IPHC survey; one set was deployed at each station. We are not using these data
#' but including here for completeness.
#'
#' @format A tibble:
#' \describe{
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{station}{station name}
#'   \item{effSkate}{effective skate number of the set, as calculated by IPHC}
#'   \item{usable}{whether or not the set is usable, as determined by IPHC plus
#'                 four extra that had no hooks enumerated except for Pacific
#'                 Halibut, and so are not usable by us}
#' }
#' @source Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
#'         received from Aaron Ranta at the IPHC.
"setData1995rs"

#' Count data for 1995 IPHC survey for RS stations
#'
#' A dataset containing counts of the RS (Random Sample?) stations for
#' the 1995 IPHC survey.
#'
#' @format A tibble:
#' \describe{
#'   \item{year}{1995 for this data set}
#'   \item{station}{station name, to link with setData1995}
#'   \item{specName}{species name for the counts for that set}
#'   \item{specCount}{count of that species for that set}
#' }
#' @source Originally from file 1995IPHCSetlineData.xlsx that Lynne Yamanaka
#'         received from Aaron Ranta at the IPHC.
"countData1995rs"


#' Count data for 2013 IPHC survey
#'
#' A dataset containing counts of each species for each
#' station for the 2013 IPHC survey.
#'
#' @format A tibble:
#' \describe{
#'
#'   \item{year}{2013 for this data set}
#'   \item{station}{station name}
#'   \item{spNameIPHC}{species name for the counts for that set}
#'   \item{specCount}{count of that species for that set}
#' }
#' @source Originally from file 2013.20-SampleInfo.csv from a spreadsheet
#'          from the IPHC.
#'          See private yeye15Reproduce repo by Andrew Edwards for data-raw.
"countData2013"

#' Station data for 2013 IPHC survey
#'
#' A dataset containing details of the stations for the 2013
#' IPHC survey; one set was deployed at each station.
#'
#' @format A tibble:
#' \describe{
#'   \item{year}{2013 for this data set}
#'   \item{station}{station name}
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{avgDepthlon}{average depth of set}
#'   \item{effSkateIPHC}{effective skate number of the set, as calculated by IPHC}
#'   \item{E_it20}{effective skate number of the set based on the first 20 hooks
#'     only, calculated as the (number of hooks observed)/(number of hooks retrieved) * effSkateIPHC
#'   \item{usable}{whether or not the set is usable, as determined by IPHC}
#' }
#' @source Originally from file 2013.20-SetInfo.csv from a spreadsheet from the
#'   IPHC, with further calculations done in private yeye15Reproduce repo of
#'   Andrew Edwards (see data-raw there, will put into gfplot).
"setData2013"


#' Set-by-set level data (with species counts) from 1996 to 2002.
#'
#' A dataset containing details of the catches at each station from 1996 to 2002
#'  for the IPHC survey. Note that 1996 is based on all hooks being enumerated
#'  while 1997-2002 is for first 20 only. This is taken into account in
#'  [get_iphc_1996to2002()] which should be used to extract these data.
#'  The values of effSkateIPHC here have already been corrected to
#'  give an effective skate number based on the true number of hooks observed for
#'  1997-2002.
#' @format A tibble:
#' \describe{
#'   \item{year}{year}
#'   \item{station}{station name}
#'   \item{set}{set number within a trip (double check that)}
#'   \item{lat}{latitude of station}
#'   \item{lon}{longitude of station}
#'   \item{depthAvge}{average depth of set}
#'   \item{spCodeIPHC}{species code as used by the IPHC}
#'   \item{spNameIPHC}{species name as used by the IPHC}
#'   \item{E_it}{effective skate number of the set, as calculated by IPHC (based on
#'     observed hook numbers)}
#'   \item{catchCount}{count of that species for that set}
#'   \item{skates}{number of skates on that set}
#'   \item{hooksObserved}{number of hooks observed on that set}
#'   \item{usable}{whether or not the set is usable, as determined by IPHC}
#' }
#' @source Originally from the file 2B AllSpecies 96-02 roundlll.xls that came
#'   from the IPHC, which Rowan Haigh extracted into 'IPHC 2B Catch.rda', Andrew
#'   Edwards preprocessed in iphc9602.Snw for Yelloweye 2014 assessment, and then
#'   tidied further in private yeye15reproduce repository.
"data1996to2002"

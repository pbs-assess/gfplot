#' Prepare PBS age data for \code{\link{plot_ages}}
#'
#' @param species_common_name A species common name
#' @param path A path to cached data
#' @param survey_series_desc A character vector of survey series to include
#' @param survey A character vector of shorter/cleaner survey names to use in
#'   the same order as \code{survey_series_desc}
#'
#' @return A data frame formatted for \code{\link{plot_ages}}
#' @export
#'
#' @examples
#' \dontrun{
#' prep_pbs_ages("canary rockfish")
#' }

prep_pbs_ages <- function(species_common_name, path = "data-cache",
  survey_series_desc = c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey"),
  survey = c("WCHG", "HS", "QCS", "WCVI")) {

  dbio <- readRDS(file.path(path, "all-survey-bio.rds"))
  dbio <- filter(dbio, survey_series_desc %in% survey_series_desc)
  dbio <- dbio[!duplicated(dbio$specimen_id), ] # critical!!
  dup <- group_by(dbio, species_common_name) %>%
    summarise(n_spp = length(unique(species_science_name))) %>%
    arrange(-n_spp) %>%
    filter(n_spp > 1)
  stopifnot(nrow(dup) == 0)

  dbio <- dbio %>%
    select(species_common_name, species_science_name, year, age,
      length, weight, maturity_code, sex, survey_series_desc)

  # bad data:
  dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "north pacific spiny dogfish"), ]
  dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "big skate"), ]
  dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "longnose skate"), ]
  dbio <- dbio[!(dbio$length > 60 & dbio$species_common_name == "pacific tomcod"), ]
  dbio <- dbio[!(dbio$length > 50 & dbio$species_common_name == "quillback-rockfish"), ]

  surv <- tibble(survey_series_desc = survey_series_desc, survey = survey)
  dbio$survey <- NULL # extra careful
  dbio <- inner_join(dbio, surv, by = "survey_series_desc")

  dbio_ages <- filter(dbio, !is.na(age)) %>%
    group_by(species_common_name, survey_series_desc) %>%
    mutate(n_ages = n()) %>%
    ungroup()

  d <- dplyr::filter(dbio_ages, species_common_name == species_common_name,
    !is.na(age))
  ds <- d %>% mutate(sex = ifelse(sex == 2, "F", "M"))

  all_surveys <- tibble(survey = survey)

  complete_df <- expand.grid(
    survey = survey,
    age = 1,
    year = seq(min(dbio_ages$year), max(dbio_ages$year), 1),
    sex = NA,
    stringsAsFactors = FALSE
  )
  if (nrow(ds) == 0)
    ds <- complete_df

  ds <- full_join(ds, all_surveys, by = "survey") %>%
    select(-survey_series_desc)
  ds
}


#' Plot age frequencies with bubble plots
#'
#' @param dat A properly formatted data frame. For example, from
#'   \code{\link{prep_pbs_ages}}. The input data frame must have the columns (in
#'   any order): \code{year}, \code{sex} (coded as \code{"M"} and \code{"F"}),
#'   \code{age}, \code{survey}, \code{n_scaled}.
#' @param max_size Maximum dot size (passed to
#'   \code{\link[ggplot2]{scale_size_area}})
#' @param sex_gap Horizontal gap between male and female bubbles
#' @param year_increment Increment between year labels on x-axis
#' @param ylab Y-axis label
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- prep_pbs_ages("canary rockfish")
#' plot_ages(d)
#' }

plot_ages <- function(dat, max_size = 5, sex_gap = 0.2, year_increment = 2,
  ylab = "Age (years)") {

  year_min <- min(dat$year, na.rm = TRUE)
  year_max <- max(dat$year, na.rm = TRUE)
  age_max <- max(dat$age, na.rm = TRUE)

  dat <- dat %>% mutate(year = ifelse(sex == "M",
    year + sex_gap/2, year - sex_gap/2)) %>%
    group_by(year, age, sex, survey) %>%
    summarise(n = n()) %>%
    group_by(year, survey) %>%
    mutate(n_scaled = n / max(n)) %>%
    ungroup()

  ggplot(dat, aes_string("year", "age")) +
    geom_vline(xintercept = seq(year_min, year_max, 1), col = "grey92", lwd = 0.4) +
    geom_hline(yintercept = seq(0, age_max, 10), col = "grey92",
      lwd = 0.4) +
    geom_point(aes_string(size = "n_scaled", group = "sex", colour = "sex"),
      pch = 21, alpha = 0.9) +
    facet_wrap(~survey, nrow = 1) +
    scale_fill_manual(values = c("M" = "grey50", "F" = "#f44256")) +
    scale_colour_manual(values = c("M" = "grey50", "F" = "#f44256")) +
    scale_x_continuous(breaks = seq(year_min, year_max, year_increment)) +
    xlab("") +
    ylab(ylab) +
    scale_size_area(max_size = max_size) +
    coord_cartesian(
      xlim = c(year_min, year_max) + c(-0.5 - sex_gap/2, 0.5 + sex_gap/2),
      ylim = c(0, age_max + 1), expand = FALSE) +
    guides(colour = FALSE, size = FALSE, fill = FALSE) +
    theme_pbs() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

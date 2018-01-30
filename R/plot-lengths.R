plot_lengths <- function() {

  survs <- c(
    "West Coast Haida Gwaii Synoptic Survey",
    "Hecate Strait Synoptic Survey",
    "Queen Charlotte Sound Synoptic Survey",
    "West Coast Vancouver Island Synoptic Survey",
    "IPHC Longline Survey",
    "IRF Longline Survey (South)",
    "IRF Longline Survey (North)")

  dbio <- readRDS("data-cache/all-survey-bio.rds")

  dbio <- filter(dbio, survey_series_desc %in% survs)
  dbio <- dbio[!duplicated(dbio$specimen_id), ] # critical!!
  dbio <- dbio %>%
    select(species_common_name, .data$species_science_name,
      .data$year, .data$age, .data$length, .data$weight,
      .data$maturity_code, .data$sex, .data$survey_series_desc)

  # bad data:
  dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "north pacific spiny dogfish"), ]
  dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "big skate"), ]
  dbio <- dbio[!(dbio$length > 600 & dbio$species_common_name == "longnose skate"), ]
  dbio <- dbio[!(dbio$length > 60 & dbio$species_common_name == "pacific tomcod"), ]
  dbio <- dbio[!(dbio$length > 50 & dbio$species_common_name == "quillback rockfish"), ]
  dbio <- dbio[!(dbio$length < 10 & dbio$weight/1000 > 1.0 &
      dbio$species_common_name == "pacific flatnose"), ]

  dat <- filter(dbio, species_common_name == "walleye pollock")

  #
  #
  # # bio_specs <- readRDS("data-cache/all-commercial-bio.rds") %>%
  # #   filter(species_common_name %in% "pacific ocean perch")
  # # catch <- readRDS("data-cache/all-catches.rds") %>%
  # #   filter(species_common_name %in% "pacific ocean perch")
  # survey_specimens <- readRDS("data-cache/all-survey-bio.rds") %>%
  #   filter(species_common_name %in% "pacific ocean perch")
  # survey_tows <- readRDS("data-cache/all-survey-spatial-tows.rds") %>%
  #   filter(species_common_name %in% "pacific ocean perch")
  #
  #

  #
  # out <- purrr::map_df(survs, function(x) {
  #   surv_spec <- dplyr::filter(survey_specimens, survey_series_desc == x)
  #   surv_tows <- dplyr::filter(survey_tows, survey_series_desc == x)
  #   o <- surv_spec %>% join_comps_survey(surv_tows, length, bin_size = 2) %>%
  #     weight_comps()
  #   o$survey_series_desc <- x
  #   o
  # })
  #
  # ggplot(out, aes_string("value", "weighted_prop")) +
  #   ggplot2::geom_col(width = 2) +
  #   facet_grid(forcats::fct_rev(as.character(year))~forcats::fct_relevel(survey_series_desc,
  #     survs)) +
  #   theme_pbs() +
  #   theme(panel.spacing = unit(-0.5, "lines")) +
  #   ylim(0, NA) +
  #   coord_cartesian(expand = FALSE)
  #


  lengths <- seq(0, 300, 2)

  su <- dplyr::tibble(survey = c("WCHG", "HS", "QCS", "WCVI", "IPHC", "IRF LL (S)", "IRF LL (N)"),
    survey_series_desc = survs)

  dd <- dat %>% full_join(su, by = "survey_series_desc") %>%
    filter(!is.na(length)) %>%
    mutate(length = lengths[findInterval(length, lengths)]) %>%
    group_by(year, sex, survey, length) %>%
    summarise(n = n()) %>%
    filter(n >= 5) %>%
    group_by(year, survey) %>%
    mutate(total = sum(n), n = n / max(n)) %>%
    mutate(sex = ifelse(sex == 1, "M", "F")) %>%
    ungroup()

  counts <- select(dd, total, year, survey) %>% unique()

  # plot_lengths <- function()
  ggplot(dd, aes_string("length", "n")) +
    ggplot2::geom_col(width = 2, aes_string(colour = "sex",
      fill = "sex"),
      position = ggplot2::position_identity()) +
    ggplot2::facet_grid(forcats::fct_rev(as.character(year))~
        forcats::fct_relevel(survey,
          su$survey), switch = "y") +
    theme_pbs() +
    scale_fill_manual(values = c("M" = "grey90", "F" = "#FF000010")) +
    scale_colour_manual(values = c("M" = "grey50", "F" = "red")) +
    theme(panel.spacing = unit(-0.5, "lines")) +
    ylim(0, NA) +
    coord_cartesian(expand = FALSE) +
    xlab("Length (cm)") +
    ylab("") +
    theme(axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()) +
    labs(colour = "Sex", fill = "Sex") +
    geom_text(data = counts, x = max(dd$length) * 0.05, y = 0.8,
      aes_string(label = "total"),
      inherit.aes = FALSE, colour = "grey50", size = 2.75, hjust = 0)
}

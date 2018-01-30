# library(dplyr)
# 
# dbio <- readRDS("data-cache/all-survey-bio.rds")
# dbio <- dbio[!duplicated(dbio$specimen_id), ]
# dbio <- dbio %>%
#   select(species_common_name, species_science_name,
#     year, age, length, weight,
#     maturity_code, sex, survey_series_desc,
#     maturity_convention_desc, maturity_convention_maxvalue,
#     specimen_id, sample_id, trip_start_date)
# 
# dbio <- mutate(dbio, month = lubridate::month(trip_start_date))
# 
# library(readr)
# 
# file <- system.file("extdata", "maturity_assignment.csv", package = "PBSsynopsis")
# 
# mat_df <- readr::read_csv(file,
#   col_types = cols(
#   maturity_convention_code = col_integer(),
#   maturity_convention_description = col_character(),
#   specimen_sex_code = col_integer(),
#   maturity_convention_maxvalue = col_integer(),
#   mature_at = col_integer())) %>%
#   rename(sex = specimen_sex_code,
#     maturity_convention_desc = maturity_convention_description) %>%
#   select(-maturity_convention_maxvalue)
# 
# dbio <- left_join(dbio, mat_df, by = c("sex", "maturity_convention_desc"))
# dbio <- mutate(dbio, mature = maturity_code >= mature_at)
# 
# dbio <- dplyr::filter(dbio, !is.na(mature), !is.na(age), !is.na(sex),
#   species_common_name == "pacific cod")
# 
# plot(dbio$age, dbio$mature)
# 
# xx <- dbio
# library(glmmTMB)
# m_re <- glmmTMB(mature ~ age + (1 | sample_id), data = xx, family = binomial)
# 
# m <- glm(mature ~ age, data = xx, family = binomial)
# 
# confint(m)
# confint(m_re)
# b <- fixef(m_re)[[1]]
# 
# l <- seq(min(xx$age), max(xx$age), length.out = 100)
# nd <- expand.grid(age = l, sample_id = unique(xx$sample_id), stringsAsFactors = FALSE)
# nd$l <- l
# nd$glm <- plogis(predict(m, newdata = nd))
# nd$glmm <- predict(m_re, newdata = nd, se.fit = FALSE)
# nd$glmm_fe <- plogis(b[[1]] + b[[2]] * nd$l)
# nd_fe <- filter(nd, sample_id == xx$sample_id[[1]]) %>%
#   select(-glmm)
# 
# library(ggplot2)
# g <- reshape2::melt(nd_fe, id.vars = c("age", "sample_id")) %>%
#   ggplot(aes(age, value,
#     colour = variable, alpha = variable, size = variable)) +
#   geom_line(data = nd, aes(age, glmm, group = sample_id),
#      inherit.aes = FALSE, alpha = 0.03) +
#   geom_line() +
#   theme_light() +
#   scale_alpha_manual(values = c("glm" = 1, "glmm_fe" = 1, "glmm" = 0.1)) +
#   scale_size_manual(values = c("glm" = 1.5, "glmm_fe" = 1.5, "glmm" = 0.3)) +
#   scale_colour_manual(values = c("glm" = "blue", "glmm_fe" = "black", "glmm" = "black")) +
#   ggtitle("Sample ID random intercepts") +
#   labs(subtitle = "P. cod") + ylab("Probability mature") + xlab("age")
# 
# g

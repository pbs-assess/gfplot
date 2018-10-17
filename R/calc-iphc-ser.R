#' Calculate Series A, B, C and D from the IPHC data
#'
#' Calculate all four series for as many years as possible, including bootstrapped
#'  values. Series with no data are treated slightly different to each other
#'  (ser_C ends up as an empty tibble, which may cause problems down the road).
#' @details The four series are:
#'
#'  Series A: first 20 hooks from each skate, only north of WCVI
#'
#'  Series B: all hooks from each skate, only north of WCVI
#'
#'  Series C: all hooks from each skate, full coast
#'
#'  Series D: first 20 hooks from each skate, full coast
#'
#' @param set_counts species-specific set-level counts from [tidy_iphc_survey()]
#'  or **other.
#' @param lat_cut_off cut off below which sets are excluded for Series A and B,
#'   default is that used in YYR 2014 assessment.
#' @return list containing four tibbles, one for each survey (ser_A, ser_B, ser_C
#'   and ser_D). Each tibble has one row for each set in each year, with columns
#'   year, station, lat, lon, E_it (effective skate number), N_it (number of
#'   fish caught of the given species), C_it (catch rate of given species, as
#'   numbers per effective skate), E_it20, N_it20 and C_it20 are the same but
#'   considering the first 20 hooks of each skate only, usable (whether the set
#'   should be used, based on IPHC codes; note that some should not be used for
#'   geospatial analysis but are included here).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' **yelloweye = tidy_iphc_survey(get_iphc_hooks("yelloweye rockfish"),
#'                              get_iphc_skates_info(),
#'                              get_iphc_sets_info() )
#' }
calc_iphc_ser_all <- function(set_counts, lat_cut_off=50.6) {
    set_counts_usable <- filter(set_counts, usable == "Y")

    # Series A
    ser_A_counts <- filter(set_counts_usable, lat > lat_cut_off,
                                              !is.na(E_it20))
    ser_A_boot <- boot_iphc(select(ser_A_counts,
                                   year,
                                   C_it20))

    ser_A <- summarise(group_by(ser_A_counts, year),
                       Sets = n(),
                       NoYYR20 = sum(C_it20 == 0) / n(),
                       I_t20SampleMean = mean(C_it20)) %>%
             left_join(ser_A_boot, by = "year")
    names(ser_A)[ names(ser_A) == "I_tBootMean"] = "I_t20BootMean"
    names(ser_A)[ names(ser_A) == "I_tBootLow"] = "I_t20BootLow"
    names(ser_A)[ names(ser_A) == "I_tBootHigh"] = "I_t20BootHigh"
    names(ser_A)[ names(ser_A) == "I_tBootCV"] = "I_t20BootCV"


    # Series B
    ser_B_counts <- filter(set_counts_usable, lat > lat_cut_off,
                           !is.na(E_it))

    ser_B_boot <- boot_iphc(select(ser_B_counts,
                                   year,
                                   C_it))

    ser_B <- summarise(group_by(ser_B_counts, year),
                       Sets = n(),
                       NoYYR = sum(C_it == 0) / n(),
                       I_tSampleMean = mean(C_it))  %>%
             left_join(ser_B_boot, by = "year")

    # Years that full coast is covered (have stations off WCVI):
    years_full_coast = as.data.frame(summarise(group_by(set_counts, year),
                                           wcvi = sum(lat<lat_cut_off))) %>%
                       filter(wcvi > 3)        # want >3 stations just in case,
                                               #  though >0 is fine (up to 2017)

    # Series C
    ser_C_counts <- filter(set_counts_usable, year %in% years_full_coast$year,
                           !is.na(E_it))
    #if(nrow(ser_C_counts) == 0) ser_C_counts[1,] <-
    #                                c(2003, rep(NA, ncol(ser_C_counts)-1))

    ser_C_boot <- boot_iphc(select(ser_C_counts,
                                   year,
                                   C_it))

    ser_C <- summarise(group_by(ser_C_counts, year),
                       Sets = n(),
                       NoYYR = sum(C_it == 0) / n(),
                       I_tSampleMean = mean(C_it)) %>%
             left_join(ser_C_boot, by = "year")

    # Series D
    ser_D_counts <- filter(set_counts_usable, year %in% years_full_coast$year,
                           !is.na(E_it20))

    ser_D_boot <- boot_iphc(select(ser_D_counts,
                                   year,
                                   C_it20))

    ser_D <- summarise(group_by(ser_D_counts, year),
                       Sets = n(),
                       NoYYR20 = sum(C_it20 == 0) / n(),
                       I_t20SampleMean = mean(C_it20)) %>%
             left_join(ser_D_boot, by = "year")
    names(ser_D)[ names(ser_D) == "I_tBootMean"] = "I_t20BootMean"
    names(ser_D)[ names(ser_D) == "I_tBootLow"] = "I_t20BootLow"
    names(ser_D)[ names(ser_D) == "I_tBootHigh"] = "I_t20BootHigh"
    names(ser_D)[ names(ser_D) == "I_tBootCV"] = "I_t20BootCV"

    list(ser_A = ser_A, ser_B = ser_B, ser_C = ser_C, ser_D = ser_D)
}
##' Do bootstrap calculations on each IPHC Series
##'
##' To be called from within [calc_iphc_ser_all()],
##'  once for each series.
##' @param ser_year_rates Tibble of two columns containing just year and catch
##'   rates (either C_it or C_it20 depending on the Series).
##' @param num.boots Number of bootstrap replicates to do.
##' @param seed_val Seed for random number generator.
##' @return Tibble containg one row for each year, with columns year, I_tBootMean,
##'   I_tBootLow, I_tBootHigh and I_tBootCV.
boot_iphc <- function(ser_year_rates,
                      num.boots = 10000,
                      seed_val = 42){
    if(dim(ser_year_rates)[2] != 2) stop("Tibble must have only two columns.")

    return_NA = FALSE
    if( dim(ser_year_rates)[1] == 0 ) {
           return_NA = TRUE } else {
        if(is.na(unique(ser_year_rates[,2]) ) ) {
           return_NA = TRUE } }
    if(return_NA) {
        return(  tibble(year = 2003,      # pull(ser_A_counts[1,1]),
                        I_tBootMean = NA,
                        I_tBootLow = NA,
                        I_tBootHigh = NA,
                        I_tBootCV = NA) )  # Just one row (year), even though gets
                                           #  left_join'ed in calc_iphc_ser_all
                                           #  with multiple years
    }

    meanFun = function(x, I) mean(x[I])

    unique_years <- unique(ser_year_rates$year)
    bcaConf <- tibble(year = unique_years) %>%
               mutate(I_tBootMean = NA,
                      I_tBootLow = NA,
                      I_tBootHigh = NA,
                      I_tBootCV = NA)
    set.seed(seed_val)
    bool <- list()       # list of boot results
    boolCI <- list()     # list of boot.ci results
    for(i in 1:length(unique_years))
    {
       this_year_rates <- filter(ser_year_rates, year == unique_years[i]) %>%
                          pull(names(ser_year_rates)[2])

       bool[[i]] <- boot::boot(this_year_rates, meanFun, R = num.boots)

       bcaConf[bcaConf$year == unique_years[i], "I_tBootMean"] <-
           mean(bool[[i]]$t)

       boolCI[[i]] = boot::boot.ci(bool[[i]], type="bca")
       bcaConf[bcaConf$year == unique_years[i], "I_tBootLow"] <-
           boolCI[[i]]$bca[4]
       bcaConf[bcaConf$year == unique_years[i], "I_tBootHigh"] <-
           boolCI[[i]]$bca[5]

       bcaConf[bcaConf$year == unique_years[i], "I_tBootCV"] <-
           sd(bool[[i]]$t) /
           bcaConf[bcaConf$year == unique_years[i], "I_tBootMean"]
   }
   bcaConf
}


##' Calculate Series AB and test if the scaled A and B are significantly different
##'
##' Calculate Series AB (Series A with 1995 and 1996 appropriately scaled from
##'  Series B)
##' @param series_all List of tibbles, one for each of Series A, B, C and D,
##'   resulting from [calc_iphc_ser_all()]
##' @return List containing
##'
##'   ser_longest: the longest time series possible from
##'   Series A and B,
##'
##'   test_AB: the results from the paired t-test,
##'
##'   G_A, G_B: geometric means of Series A and Series D (based on bootstrapped
##'    means).
##'
##'   Longest series is either
##'
##'   (i) Series AB (Series A with 1995 and 1996 appropriately scaled from
##'   Series B) if `test_AB$p.value > 0.05`, because the p-value means that we
##'   cannot reject the null hypothesis that the true difference
##'   in means of the rescaled (by their geometric means) Series A and B equals 0,
##'
##'   (ii) Series A, which is the longest series available if the rescaled
##'  series are significantly different.
calc_iphc_ser_AB <- function(series_all) {
    years_AB <- intersect(series_all$ser_A$year, series_all$ser_B$year)

    # Geometric means of each series for the overlapping years
    G_A <- exp( mean( log( filter(series_all$ser_A,
                                     year %in% years_AB)$I_t20BootMean)))

    G_B <- exp( mean( log( filter(series_all$ser_B,
                                  year %in% years_AB)$I_tBootMean)))

    # Scale by G_A, geometricetric mean of bootstrapped means.
    ser_A_scaled <- filter(series_all$ser_A,
                           year %in% years_AB) %>%
                    mutate(I_t20SampleMean = I_t20SampleMean / G_A,
                           I_t20BootMean = I_t20BootMean / G_A,
                           I_t20BootLow = I_t20BootLow / G_A,
                           I_t20BootHigh = I_t20BootHigh / G_A)
    # exp(mean(log(ser_A_scaled$I_t20BootMean)))  # =1

    ser_B_scaled <- filter(series_all$ser_B,
                           year %in% years_AB) %>%
                    mutate(I_tSampleMean = I_tSampleMean / G_B,
                           I_tBootMean = I_tBootMean / G_B,
                           I_tBootLow = I_tBootLow / G_B,
                           I_tBootHigh = I_tBootHigh / G_B)
    # exp(mean(log(ser_B_scaled$I_tBootMean)))  # =1

    t_AB <- stats::t.test( ser_A_scaled$I_t20BootMean,
                          ser_B_scaled$I_tBootMean,
                          paired = TRUE)

    if(t_AB$p.value >= 0.05){   # Can't reject null hypothesis that true difference
                                #  in means equals 0
                   # Multiply the ser_B years not in years_AB by G_A/G_B,
                   #  naming columns with 20 since rescaling (and to combine with
                   #  series_all$ser_A. Note that NoYYR20 is not scaled (as
                   #  we're implicitly scaling all the catch rates, but the zeros
                   #  wouldn't change).
                   ser_AB <- filter(series_all$ser_B, !year %in% years_AB) %>%
                             mutate(NoYYR20 = NoYYR,
                                    I_t20SampleMean = I_tSampleMean * G_A / G_B,
                                    I_t20BootMean = I_tBootMean * G_A / G_B,
                                    I_t20BootLow = I_tBootLow * G_A / G_B,
                                    I_t20BootHigh = I_tBootHigh * G_A / G_B,
                                    I_t20BootCV = I_tBootCV) %>%
                             select(-c("NoYYR",
                                       "I_tSampleMean",
                                       "I_tBootMean",
                                       "I_tBootLow",
                                       "I_tBootHigh",
                                       "I_tBootCV")) %>%
                             rbind(series_all$ser_A)
        return(list(ser_longest = ser_AB,
                    test_AB = list(t_AB = t_AB,
                                   G_A = G_A,
                                   G_B = G_B) ) )
     } else {
         return(list(ser_longest = series_all$ser_A,
                     test_AB = list(t_AB = t_AB,
                                    G_A = G_A,
                                    G_B = G_B) ) )
     }
}

##' Compare Series A and D to see if A can be considered a relative index for the whole coast
##'
##' Compare Series A and D to see if A can be considered a relative index for
##'  the whole coast.
##' @param series_all List of tibbles, one for each of Series A, B, C and D,
##'   resulting from [calc_iphc_ser_all()]
##' @return List of t_AD (results of the paired t-test), and geometric means G_A
##'   and G_D.
compare_iphc_ser_A_D <- function(series_all) {
    years_AD <- intersect(series_all$ser_A$year, series_all$ser_D$year)

    # Geometric means of each series for the overlapping years
    G_A <- exp( mean( log( filter(series_all$ser_A,
                                     year %in% years_AD)$I_t20BootMean)))

    G_D <- exp( mean( log( filter(series_all$ser_D,
                                  year %in% years_AD)$I_t20BootMean)))

    # Scale by G_A, geometricetric mean of bootstrapped means.
    ser_A_scaled <- filter(series_all$ser_A,
                           year %in% years_AD) %>%
                    mutate(I_t20SampleMean = I_t20SampleMean / G_A,
                           I_t20BootMean = I_t20BootMean / G_A,
                           I_t20BootLow = I_t20BootLow / G_A,
                           I_t20BootHigh = I_t20BootHigh / G_A)
    # exp(mean(log(ser_A_scaled$I_t20BootMean)))  # =1

    ser_D_scaled <- filter(series_all$ser_D,
                           year %in% years_AD) %>%
                    mutate(I_t20SampleMean = I_t20SampleMean / G_D,
                           I_t20BootMean = I_t20BootMean / G_D,
                           I_t20BootLow = I_t20BootLow / G_D,
                           I_t20BootHigh = I_t20BootHigh / G_D)

    t_AD <- stats::t.test( ser_A_scaled$I_t20BootMean,
                          ser_D_scaled$I_t20BootMean,
                          paired = TRUE)
    list(t_AD = t_AD, G_A = G_A, G_D = G_D)
}


##' Compare Series B and C to see if A can be considered a relative index for the whole coast
##'
##' Compare Series B and C to see if A can be considered a relative index for
##'  the whole coast
##' @param series_all List of tibbles, one for each of Series A, B, C and D,
##'   resulting from [calc_iphc_ser_all()]
##' @return List of t_BC (results of the paired t-test), and geometric means G_B
##'   and G_C.
compare_iphc_ser_B_C <- function(series_all) {
    years_BC <- intersect(series_all$ser_B$year, series_all$ser_C$year)

    # Geometric means of each series for the overlapping years
    G_B <- exp( mean( log( filter(series_all$ser_B,
                                     year %in% years_BC)$I_tBootMean)))

    G_C <- exp( mean( log( filter(series_all$ser_C,
                                  year %in% years_BC)$I_tBootMean)))

    # Scale by G_B, geometricetric mean of bootstrapped means.
    ser_B_scaled <- filter(series_all$ser_B,
                           year %in% years_BC) %>%
                    mutate(I_tSampleMean = I_tSampleMean / G_B,
                           I_tBootMean = I_tBootMean / G_B,
                           I_tBootLow = I_tBootLow / G_B,
                           I_tBootHigh = I_tBootHigh / G_B)
    # exp(mean(log(ser_B_scaled$I_tBootMean)))  # =1

    ser_C_scaled <- filter(series_all$ser_C,
                           year %in% years_BC) %>%
                    mutate(I_tSampleMean = I_tSampleMean / G_C,
                           I_tBootMean = I_tBootMean / G_C,
                           I_tBootLow = I_tBootLow / G_C,
                           I_tBootHigh = I_tBootHigh / G_C)

    t_BC <- stats::t.test( ser_B_scaled$I_tBootMean,
                          ser_C_scaled$I_tBootMean,
                          paired = TRUE)
    list(t_BC = t_BC, G_B = G_B, G_C = G_C)
}


##' Do all the calculations for the IPHC survey.
##'
##' From species-specific set-level counts, derive all the required Series and
##'  test their equivalence.
##' @param set_counts species-specific set-level counts from [tidy_iphc_survey()]
#'  or **other.
##' @param sp Species names (as used in gfplot).
##' @return List containing
##'
##' ser_longest: tibble for the longest time series that can be made for this
##'     species, as output from [calc_iphc_ser_AB()]; either Series A or AB.
##'
##' full_coast: whether or not the longest time series can be considered
##'     representative of the full coast (based on the paired t-tests).
##'
##' ser_all: Series A, B, C and D, as output from [calc_iphc_ser_all()].
##'
##' test_AB: t-test results from [calc_iphc_ser_AB()]
##'
##' test_AD: t-test results from [compare_iphc_ser_A_D()]
##'
##' test_BC: t-test results from [compare_iphc_ser_B_C()]
##'
##' If no observations at all for the species then return NA.
calc_iphc_full_res <- function(set_counts, sp)
   {
        if(is.na(unique(c(set_counts$N_it20, set_counts$N_it20)))){
           return(NA)
        }
        series_all <- calc_iphc_ser_all(set_counts)
        iphc_ser_longest <- calc_iphc_ser_AB(series_all)
                                                   # list of longest series and
                                                   #  paired t-test results
        test_AD <- compare_iphc_ser_A_D(series_all)
        # test_AD$t_AD$p.value                       # Need to say if >0.05 then
                                                   #  Series A representative
                                                   #  of whole coast (having
                                                   # compared with Series D)

        test_BC <- compare_iphc_ser_B_C(series_all)
                                                   # Need to say if >0.05 then
                                                   #  Series B representative
                                                   #  of whole coast (having
                                                   #  compared with Series C)
        # full_coast is TRUE if the longest time series is representative of
        #  the full coast:
        if(test_AD$t_AD$p.value >= 0.05) {
            full_coast <- (test_AD$t_AD$p.value >= 0.05 &
                           test_BC$t_BC$p.value)} else {
            full_coast <- (test_AD$t_AD$p.value >= 0.05)} # as A is the long Series
    list(ser_longest = iphc_ser_longest$ser_longest,
         full_coast = full_coast,
         ser_all = series_all,
         test_AB = iphc_ser_longest$test_AB,
         test_AD = test_AD,
         test_BC = test_BC)
    }

##' Plot just the IPHC survey index, though works for all surveys
##'
##' Plot just the IPHC survey index (mainly for testing before including in gfplot).
##' @param iphc_set_counts_sp_format Set counts for a given species formatted
##'  in the same way as the other survey series, using [format_iphc_longest()]
##' @return ggplot objet of the plot
plot_iphc_index <- function(iphc_set_counts_sp_format){
    survey_cols = c(RColorBrewer::brewer.pal(7L, "Set2"),
       "#303030", "#a8a8a8", "#a8a8a8", "#a8a8a8")
    iphc_plot <- iphc_set_counts_sp_format %>%
                 plot_survey_index(col = c("grey60", "grey20"),
                                   survey_cols = survey_cols,
                                   xlim = c(1984, 2017)) +
                 ggplot2::theme(
                                axis.title.y = element_blank(),
                                axis.text.y = element_blank(),
                                axis.ticks.y = element_blank()
                               ) +
                 ggplot2::ggtitle("Survey relative biomass indices")
    iphc_plot
    }

##' Format the longest IPHC time series index to agree with other surveys
##'
##' Format the longest IPHC time series index to agree with other surveys so
##'   that [plot_survey_index()] works automatically. So the mean catch rate
##'   gets renames as `biomass' even though it's numbers per effective skate.
##'   **NOTE** num_sets is currently hardwired as 130, Issue 45.
##' @param ser_longest Output from [calc_iphc_full_res()], but only want the
##'   ser_longest component.
##' @return Renamed ser_longest, with some required columns calculated.
##' @examples
##' \dontrun{
##' sp = "yelloweye rockfish"
##' set_counts <- get_all_iphc_set_counts(sp)
##' iphc_set_counts_sp <- calc_iphc_full_res(set_counts, sp)
##' format_iphc_longest(iphc_set_counts_sp$ser_longest)
##' }
##'
format_iphc_longest <- function(ser_longest){
      new_names <- select(ser_longest,
                          year = year,
                          biomass = I_t20BootMean,
                          lowerci = I_t20BootLow,
                          upperci = I_t20BootHigh,
                          prop_empty_sets = NoYYR20
                          ) %>%
                   mutate(mean_cv = mean(ser_longest$I_t20BootCV, na.rm=TRUE),
                          num_sets = 130,
                          num_pos_sets = num_sets * (1 - prop_empty_sets),
                          survey_abbrev = "IPHC FISS") %>%
                   select(survey_abbrev,
                          everything(),
                          -prop_empty_sets)
      new_names
    }


##' Get data, do calculations and plot longest series for the IPHC survey
##'
##' Get data, do calculations and plot longest series for the IPHC survey for
##'  a given species. Will take a while since queries GFbio (and need to be on DFO
##'  network).
##' @param sp Species names (as used in gfplot).
##' @return For the given species, list containing
##'
##'   iphc_set_counts_sp: list returned from [calc_iphc_full_res()]
##'
##'   iphc_set_counts_sp_format: just the longest series, returned from
##'     [format_iphc_longest()] with calculations and formatting to match that
##'     of other surveys
##'
##'   g_iphc_index: plot of just the iphc data (useful for testing all species)
iphc_get_calc_plot <- function(sp)
{
     set_counts <- get_all_iphc_set_counts(sp)
     iphc_set_counts_sp <- calc_iphc_full_res(set_counts, sp)
     iphc_set_counts_sp_format <-
                             format_iphc_longest(iphc_set_counts_sp$ser_longest)

     g_iphc_index <- plot_iphc_index(iphc_set_counts_sp_format)

     list(iphc_set_counts_sp = iphc_set_counts_sp,
          iphc_set_counts_sp_format = iphc_set_counts_sp_format,
          g_iphc_index = g_iphc_index)
    }


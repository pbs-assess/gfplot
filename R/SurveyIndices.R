
get.sql.data <- function(sql, db="PacHarvest", svr="DFBCV9TWVASP001", ...) {
#svr="(local)"
   # Requires the RODBC package
   require("RODBC")
   # Construct a connection string
   constr <- paste("Driver={SQL Server};Server=",svr,";Database=",db,";", sep="")
   # Connect to the database
   cnn <- odbcDriverConnect(constr)
   # Execute the SQL command and store the results in a data frame
   df <- sqlQuery(cnn, sql, ...)
   # Close the connection
   odbcClose(cnn)

   # Return the results
   return(df)
}

calc.biomass <- function(dat, i, sa, is_ll=FALSE, is_sable=FALSE, is_jig=FALSE,
   is_dog=FALSE) {

   # If i is only a single number, use a vector of the row indices instead
   # (used for testing)
   if (length(i)==1)
      i <- 1:nrow(dat)

   # Resample from the data frame using the vector i
   dat <- dat[i,]

   # Calculate the mean biomass (kg/km2) for each stratum.  The "if-else" structure
   # is for handling the special cases of a longline (is_ll), sablefish
   # (is_sable), and jig (is_jig) surveys that use different measures of catch.
   mu <- numeric(nrow(sa))
   if (is_sable) {
      mb <- tapply(dat$CPUE_PPT, list(dat$GROUPING_CODE), mean)
      mu[sa$GROUPING_CODE %in% names(mb)] <- mb
      res <- mean(mu)
   } else if (is_ll) {
      mb <- tapply(dat$DENSITY_PPKM2, list(dat$GROUPING_CODE), mean)
      mu[sa$GROUPING_CODE %in% names(mb)] <- mb
      res <- sum(mu * sa$AREA_KM2)
   } else if (is_jig) {
      mb <- tapply(dat$CPUE_PPH, list(dat$GROUPING_CODE), mean)
	  mu[sa$GROUPING_CODE %in% names(mb)] <- mb
	  res <- mean(mu)
   } else if (is_dog) {
      mb <- tapply(dat$DENSITY_PPKM2, list(dat$GROUPING_CODE), mean)
      mu[sa$GROUPING_CODE %in% names(mb)] <- mb
      res <- mean(mu)
   } else {
      mb <- tapply(dat$DENSITY_KGPM2 * 1000000, list(dat$GROUPING_CODE), mean)
      mu[sa$GROUPING_CODE %in% names(mb)] <- mb
      res <- sum(mu * sa$AREA_KM2)
   }
   #mu[sa$GROUPING_CODE %in% names(mb)] <- mb

   # Expand mean biomass per stratum to the entire survey area to get total biomass
   #return(sum(mu * sa$AREA_KM2))
   return(list(res = res, grouping_code = sa$GROUPING_CODE,
     mean_per_strat = as.numeric(mu), area = sa$AREA_KM2))

}

boot.species <- function(survey_id, species_code, stat=calc.biomass, r=1000,
   is_ll=FALSE, is_sable=FALSE, is_jig=FALSE, is_dog=FALSE, resample=FALSE,
   return_strata_densities = FALSE) {

   # Requires the boot package
   require(boot)

   # Get a dataframe of the number of sets and area per survey stratum
   sa <- get.sql.data(paste("EXEC proc_stratum_info", survey_id), "GFBioSQL")

   # Get a data frame of the catch density per fishing event for the species/survey
   # Sablefish surveys have to be handled by a seperate procedure.
   if (is_sable) {
      dm <- get.sql.data(paste("EXEC proc_catmat_sable ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL",
         as.is=c(F,F,T,T,F,F,F,F,F,F,F,F,F,F,F,F,F))
   } else if (is_ll) {
      dm <- get.sql.data(paste("EXEC proc_catmat_ll_2013 ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL",
         as.is=c(T,F,F,F,F,F,F,F,F,F,F,F))
   } else if (is_jig) {
      dm <- get.sql.data(paste("EXEC proc_catmat_jig ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL",
         as.is=c(F,F,T,T,F,F,F,F,F,F,F,F,F,F,F))
   } else if (is_dog) {
      dm <- get.sql.data(paste("EXEC proc_catmat_dog ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL",
         as.is=c(T,F,F,F,F,F,F,F,F,F,F))
   } else {
      dm <- get.sql.data(paste("EXEC proc_catmat_2011 ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL",
         as.is=c(F,F,T,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F))
   }

   # This is a kludge for the Hecate Strait multispecies survey, where only the
   # first 7 strata are used for biomass indices calculation.
   if (survey_id >= 4 & survey_id <= 14) {
      dm <- dm[dm$GROUPING_CODE >= 77 & dm$GROUPING_CODE <= 83,]
      sa <- sa[sa$GROUPING_CODE >= 77 & sa$GROUPING_CODE <= 83,]
   }

   # Are we resampling?
   if (resample) {
      dm <- resample(survey_id, dm)
   }

   # Construct a data frame to store results
   smy <- data.frame("species_code"=character(1), "biomass"=numeric(1),
      "boot_mean"=numeric(1), "boot_median"=numeric(1), "boot_lower_ci"=numeric(1),
      "boot_upper_ci"=numeric(1), "boot_re"=numeric(1), "catch_weight"=numeric(1),
      "num_sets"=numeric(1), "num_pos_sets"=numeric(1), "year"=numeric(1),
      "survey_id"=numeric(1), "survey_series_id"=numeric(1),
      "survey"=character(1), stringsAsFactors=FALSE)

   # Perform the bootstrap analysis.  If the data frame has no rows or if the
   # catch densities or catch counts are all zero (i.e. the species wasn't caught)
   # then return "NA"
   if (nrow(dm) > 0 & !(!is_ll & all(dm$DENSITY_KGPM2==0)) |
                      !(!is_ll & all(dm$DENSITY_PPKM2==0))) {
     if (!return_strata_densities) {
      # Perform the bootstrap
      boot_obj <- boot(dm, stat, r, strata=dm$GROUPING_CODE, sa=sa, is_ll=is_ll,
         is_sable=is_sable, is_jig=is_jig, is_dog=is_dog, sim="ordinary")

      # Calculate the bias corrected and adjusted confidence limits
      bootci_obj <- boot.ci(boot_obj, type="perc")

      # Save various elements to the results data frame
      smy$species_code <- species_code
      smy[1,2:13] <- c(boot_obj$t0, mean(boot_obj$t), median(boot_obj$t),
         bootci_obj$bca[1,4], bootci_obj$perc[c(4,5)],
         sd(boot_obj$t) / mean(boot_obj$t),
         ifelse(all(dm$CATCH_WEIGHT==0), sum(dm$CATCH_COUNT, na.rm=TRUE),
            sum(dm$CATCH_WEIGHT, na.rm=TRUE)),
         nrow(dm),
         max(length(dm$CATCH_WEIGHT[dm$CATCH_WEIGHT > 0]),
            length(dm$CATCH_COUNT[dm$CATCH_COUNT > 0])),
         dm$YEAR[1],
         dm$SURVEY_ID[1], dm$SURVEY_SERIES_ID[1])
      smy[1,14] <- dm$SURVEY_DESC[1]

      # Build a list of final results
      results <- list("stratum_info"=sa, "catch_matrix"=dm, "boot_obj"=boot_obj,
         "bootci_obj"=bootci_obj, "summary"=smy, "species_code"=smy$species_code,
         "survey_id"=survey_id)
      class(results) <- "bootres"
     } else {
       out <- stat(dm, i = 1, sa=sa, is_ll=is_ll,
         is_sable=is_sable, is_jig=is_jig, is_dog=is_dog)
       results <- data.frame(
         grouping_code = out$grouping_code,
         mean_per_strat = out$mean_per_strat,
         area = out$area,
         species_code = species_code,
         survey_series_id = dm$SURVEY_SERIES_ID[1],
         year = dm$YEAR[1], stringsAsFactors = FALSE)
     }
   } else {
      warning(paste("Survey Id", survey_id, "Species", species_code,
      "produces an empty catch matrix"))
      results <- NA
   }

   # Return the results list
   return(results)

}

get_species_stratum_densities <- function(species_code, survey_series_id,
  is_ll=FALSE, is_sable=FALSE, is_jig=FALSE, is_dog=FALSE, resample=FALSE) {

  # Get the survey ids for all surveys in the survey series
  sql <- paste("SELECT SURVEY_ID, SURVEY_DESC FROM SURVEY WHERE SURVEY_SERIES_ID =",
    survey_series_id, "ORDER BY SURVEY_ID")
  sids <- get.sql.data(sql, "GFBioSQL")

  # For each survey, run the boot.species function for the given species and
  # store each result
  out <- list()
  for (i in seq_along(sids$SURVEY_ID)) {
    out[[i]] <- boot.species(sids$SURVEY_ID[i], species_code,
      is_ll=is_ll, is_sable=is_sable, is_jig=is_jig, is_dog=is_dog, resample=resample,
      return_strata_densities = TRUE) # <- key difference here
  }

  out <- out[sapply(out, function(x) is.data.frame(x))]
  res <- do.call("rbind", out)

  if (is.null(res))
    res <- data.frame(
      grouping_code = NA,
      mean_per_strat = NA,
      area = NA,
      species_code = species_code,
      survey_series_id = survey_series_id,
      year = NA, stringsAsFactors = FALSE)

  return(res)

}

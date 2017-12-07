############# - stop
# -------------------------------------
# ######## inla
# 
# unique(d$survey_series_desc)
# [1] "Queen Charlotte Sound Shrimp Survey"          
# [2] "Sablefish Research and Assessment Survey"     
# [3] "Sablefish Inlet Standardized"                 
# [4] "Sablefish Offshore Standardized"              
# [5] "Queen Charlotte Sound Synoptic Survey"        
# [6] "IPHC Longline Survey"                         
# [7] "Sablefish Stratified Random"                  
# [8] "West Coast Vancouver Island Synoptic Survey"  
# [9] "Hecate Strait Synoptic Survey"                
# [10] "West Coast Haida Gwaii Synoptic Survey"       
# [11] "PHMA Rockfish Longline Survey - Outside North"
# [12] "PHMA Rockfish Longline Survey - Outside South"

dtemp <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds") 

names(dtemp) <- tolower(names(dtemp))
dtemp$species_common_name <- tolower(dtemp$species_common_name)
dtemp <- dplyr::filter(dtemp, survey_series_desc %in% c("Hecate Strait Synoptic Survey"))
sort(table(dtemp$species_common_name))

dd1 <- get_surv_data("arrowtooth flounder",
  c("Hecate Strait Synoptic Survey"), 
  years = c(1996:2017))

dd1 <- unique(dd1) # FIXME!!
table(dd1$year, dd1$present)
yrs <- unique(dd1$year)
dd1 <- filter(dd1, year %in% yrs[(length(yrs)-0):length(yrs)])
table(dd1$year, dd1$present)

b <- join_noaa_bathy(dd1)
dd2 <- b$data
dd3 <- scale_predictors(dd2)
m <- fit_inla(dd3)
pg <- make_prediction_grid(dd3, b$bath, n = 125L)
pred <- predict_inla(model_bin = m$bin, model_pos = m$pos, n = 300L,
  mesh = m$mesh, pred_grid = pg)

# plot:
library(PBSmapping)
data("nepacLLhigh")

attr(nepacLLhigh, "zone") <- 8
nepacUTM <- convUL(clipPolys(nepacLLhigh, xlim = range(dd3$lon) + c(-1, 1), 
  ylim = range(dd3$lat) + c(-1, 1)))

main_scale <- viridis::scale_fill_viridis(option = "C")
binary_scale <- scale_fill_gradient2(low = scales::muted("blue"), mid = "grey90", high = scales::muted("red"), 
  midpoint = 0.5, limits = c(0, 1))
spatial_scale <- scale_fill_gradient2(low = scales::muted("blue"), mid = "grey90", high = scales::muted("red"))

plot_bc_map(pred$pred, dd3, "sqrt(pred_delta)", main_scale)
plot_bc_map(pred$pred, dd3, "pred_binary", binary_scale)
plot_bc_map(pred$pred, dd3, "sqrt(pred_positive)", main_scale)
plot_bc_map(pred$pred, dd3, "spatial_field_binary", spatial_scale)
plot_bc_map(pred$pred, dd3, "spatial_field_positive", spatial_scale)

reshape2::melt(dplyr::bind_rows(pred$params)) %>% ggplot(aes(value)) +
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = "free") +
  geom_vline(xintercept = 0, col = "red", lty = 2) +
  theme_light()

x <- seq(min(dd3$depth_scaled), max(dd3$depth_scaled), length.out = 1000)
x_real <- exp((x * dd3$depth_sd[1]) + dd3$depth_mean[1])

post <- sapply(x, function(i) pred$params$b0 + pred$params$b1 * i + pred$params$b2 * i^2)
l <- apply(post, 2, quantile, probs = 0.1)
u <- apply(post, 2, quantile, probs = 0.9)
plot(x_real, apply(post, 2, quantile, probs = 0.5), type = "l", xlab = "Depth (m)", ylab = "log(density)",
  ylim = range(c(l, u)))
polygon(c(x_real, rev(x_real)), c(l, rev(u)),
  col = "#00000030", border = NA)

post <- sapply(x, function(i)  plogis(pred$params$b0_bin + pred$params$b1_bin * i + pred$params$b2_bin * i^2))
l <- apply(post, 2, quantile, probs = 0.1)
u <- apply(post, 2, quantile, probs = 0.9)
plot(x_real, apply(post, 2, median), type = "l", xlab = "Depth (m)", ylab = "Probability of observing")
polygon(c(x_real, rev(x_real)), c(l, rev(u)),
  col = "#00000030", border = NA)


# matplot(x_real, t(post), type = "l", col = "#00000010", lty = 1)

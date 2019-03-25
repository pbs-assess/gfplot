gfplot::get_sensor_attributes()

d_trawl <- gfplot::get_sensor_data_trawl(ssid = c(1, 3, 4, 16), spread_attributes = FALSE)
saveRDS(d_trawl, file = "inst/dat-sensor-trawl.rds")

d_ll <- gfplot::get_sensor_data_ll_ctd(c(22, 36), sensor_min_max = TRUE)
saveRDS(d_ll, file = "inst/dat-sensor-ll.rds")

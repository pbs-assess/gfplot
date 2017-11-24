d <- readRDS("~/Dropbox/dfo/data/all-catches.rds")
names(d) <- tolower(names(d))
d$species_common_name <- tolower(d$species_common_name)
d$species_scientific_name <- tolower(d$species_scientific_name)
d$year <- lubridate::year(d$best_date)

library(tidyverse)

catches <- d %>% filter(!is.na(species_common_name), !is.na(year)) %>% 
  group_by(year, species_common_name) %>% 
  summarise(landed_kg = sum(landed_kg, na.rm = TRUE), discarded_kg = sum(discarded_kg, na.rm = TRUE),
    landed_pcs = sum(landed_pcs, na.rm = TRUE), discarded_pcs = sum(discarded_pcs, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(species_common_name) %>% 
  mutate(total_catch = sum(landed_kg)) %>% 
  ungroup()

catches <- mutate(catches, species_common_name = forcats::fct_reorder(species_common_name, -total_catch))

cm <- reshape2::melt(filter(catches, total_catch > 1e6, !species_common_name %in% "unknown fish"), 
  id.vars = c("year", "species_common_name"))
unique(cm$species_common_name) %>% length()



pdf("catches.pdf", width = 16, height = 10)
filter(cm, variable %in% c("landed_kg", "discarded_kg")) %>% 
  ggplot(aes(year, value)) +
  geom_col(aes(fill = variable)) +
  facet_wrap(~species_common_name, scales = "free_y") +
  ggsidekick::theme_sleek() +
  scale_fill_manual(values = c("grey50", "grey10"))

filter(cm, variable %in% c("landed_pcs", "discarded_pcs")) %>% 
  ggplot(aes(year, value)) +
  geom_col(aes(fill = variable)) +
  facet_wrap(~species_common_name, scales = "free_y") +
  ggsidekick::theme_sleek() +
  scale_fill_manual(values = c("grey50", "grey10"))
dev.off()

pal <- RColorBrewer::brewer.pal(5, "Blues")

#' @param xfrac The fraction over from the left side.
#' @param yfrac The fraction down from the top.
#' @param label The text to label with.
#' @param pos Position to pass to text()
#' @param ... Anything extra to pass to text(), e.g. cex, col.
add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}

plot_catch <- function(a, b, lab) {
  plot(1, 1, type = "n", xlim = c(1960, 2017) + c(-0.5, 0.5), ylim = c(0, max(a + b)*1.03), 
    axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
  abline(v = seq(1900, 2020, 10), col = "grey90")
  # polygon(c(x$year, rev(x$year)), c(rep(0, nrow(x)), rev(x$landed_kg + x$discarded_kg)), col = "red")
  # polygon(c(x$year, rev(x$year)), c(rep(0, nrow(x)), rev(x$discarded_kg)), col = "blue")
  rect(xleft = x$year - 0.5, xright = x$year + 0.5, ybottom = rep(0, nrow(x)), ytop = a + b,
    col = pal[[3]], border = "#00000090", lwd = 0.5)
  rect(xleft = x$year - 0.5, xright = x$year + 0.5, ybottom = rep(0, nrow(x)), ytop = b,
    col = pal[[2]], border = "#00000090", lwd = 0.5)
  box(col = "grey50")
  axis(2, col = "grey60", col.ticks = "grey70", col.axis = "grey30", las = 1)
  
  add_label(-0.008, 0.09, lab, col = "grey30", cex = 1)

}

for(i in unique(cm$species_common_name)) {
  pdf(paste0("catch/", gsub("/", "-", gsub(" ", "-", i)), ".pdf"), width = 3, height = 2.8)
  x <- filter(catches, species_common_name == i)
  par(mfrow = c(2, 1), oma = c(2, 1, .5, .5), cex = 0.7, mar = c(0, 2.5, 0, 0), mgp = c(2, 0.4, 0),
    tcl = -0.3)
  plot_catch(x$landed_kg/100000, x$discarded_kg/100000, "Trawl (10000 kg)")
  plot_catch(x$landed_pcs/1000, x$discarded_pcs/1000, "Longline (1000 fish)")
  axis(1, col = "grey60", col.ticks = "grey70", col.axis = "grey30")
  dev.off()
}



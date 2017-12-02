d <- readRDS("~/Dropbox/dfo/data/all-catches.rds")
names(d) <- tolower(names(d))
d$species_common_name <- tolower(d$species_common_name)
d$species_scientific_name <- tolower(d$species_scientific_name)
d$year <- lubridate::year(d$best_date)

library(tidyverse)

d2 <- d %>% mutate(gear = dplyr::recode(gear,
  UNKNOWN = "Unknown/trawl",
  `BOTTOM TRAWL` = "Bottom trawl",
  `HOOK AND LINE` = "Hook and line",
  `MIDWATER TRAWL` = "Midwater trawl",
  `TRAP` = "Trap",
  `UNKNOWN TRAWL` = "Unknown/trawl"))

catches <- d2 %>% filter(!is.na(species_common_name), !is.na(year)) %>% 
  group_by(year, species_common_name, gear) %>% 
  summarise(landed_kg = sum(landed_kg, na.rm = TRUE), discarded_kg = sum(discarded_kg, na.rm = TRUE),
    landed_pcs = sum(landed_pcs, na.rm = TRUE), discarded_pcs = sum(discarded_pcs, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(species_common_name) %>% 
  mutate(total_catch = sum(landed_kg)) %>% 
  ungroup()

catches <- mutate(catches, species_common_name = forcats::fct_reorder(species_common_name, -total_catch))

catches$species_common_name <- as.character(catches$species_common_name)
catches$species_common_name[catches$species_common_name == "spiny dogfish"] <- "north pacific spiny dogfish"

# cm <- reshape2::melt(filter(catches, total_catch > 2e5, !species_common_name %in% "unknown fish"), 
#   id.vars = c("year", "species_common_name", "gear"))
# unique(cm$species_common_name) %>% length()
# 
# 
# landings <- filter(cm, variable %in% c("landed_kg"))
# discards <- filter(cm, variable %in% c("discarded_kg"))
# 
# landings$gear <- as.character(landings$gear)
# discards$gear <- as.character(discards$gear)
# 
# discards$gear <- "Discarded"
# 
# all_landings <- bind_rows(landings, discards)
# 
# all_landings <- mutate(all_landings, gear = forcats::fct_relevel(gear,
#   "Bottom trawl",
#   "Midwater trawl",
#   "Hook and line",
#   "Trap",
#   "Unknown/trawl",
#   "Discarded"))
# 
# all_landings <- group_by(all_landings, year, species_common_name, gear) %>% 
#   summarise(value = sum(value, na.rm = TRUE)) %>% 
#   ungroup()
# 
# pal <-  c(RColorBrewer::brewer.pal(n = length(unique(all_landings$gear))-2, "Paired"),"grey60", "grey30")[c(2, 1, 4, 3, 5, 6)]
# 
# pdf("catches.pdf", width = 16, height = 10)
# filter(all_landings) %>% 
#   ggplot(aes(year, value, fill = gear)) +
#   geom_col() +
#   facet_wrap(~species_common_name, scales = "free_y") +
#   ggsidekick::theme_sleek() +
#   scale_fill_manual(values = pal)
# 
# # filter(cm, variable %in% c("landed_pcs", "discarded_pcs")) %>% 
# #   ggplot(aes(year, value)) +
# #   geom_col(aes(fill = variable)) +
# #   facet_wrap(~species_common_name, scales = "free_y") +
# #   ggsidekick::theme_sleek() +
# #   scale_fill_manual(values = c("grey50", "grey10"))
# dev.off()
# 
# ## ----------------

plot_catch <- function(dat, xlim = c(1955, 2017)) {
  pal <-  c(RColorBrewer::brewer.pal(n = length(unique(all_landings$gear))-2, 
    "Paired"),"grey60", "grey30")[c(2, 1, 4, 3, 5, 6)]
  
  # if (unique(dat$species_common_name) == "copper rockfish") browser()
  
  scale_val <- 1000000
  ylab <- "Landings (1000 tons)"
  
  if (max(dat$value) < 100000) {
    scale_val <- 1000
    ylab <- "Landings (tons)"
  }
  
  if (max(dat$value) < 1000) {
    scale_val <- 1
    ylab <- "Landings (kg)"
  }
  
  g <- dat %>% 
    ggplot(aes(year, value/scale_val, fill = gear)) +
    geom_col() +
    ggsidekick::theme_sleek() +
    scale_fill_manual(values = pal)
  gd <- ggplot_build(g) # extract data from ggplot
  gd1 <- gd$data[[1]]
  
  par(mfrow = c(1, 1), oma = c(1.5, 3.2, 3.2, .5), mar = c(0,0,0,0), cex = 0.8,
    mgp = c(2, 0.35, 0), tcl = -0.2)
  
  plot(1, 1, xlim = xlim, ylim = range(gd1$y) * c(1, 1.03), 
    type = "n", axes = FALSE,
    ann = FALSE, yaxs = "i", xaxs = "i")
  plyr::d_ply(gd1, "group", function(x) {
    rect(xleft = x$x-0.5, xright = x$x+0.5, ybottom = x$ymin, ytop = x$ymax,
      col = x$fill, border = x$fill)
  })
  box(col = "grey40")
  axis(1, at = seq(1900, 2040, 10), cex.axis = 0.8, col = "grey40", col.axis = "grey40",
    padj = -0.4)
  axis(2, las = 1, cex.axis = 0.8, col = "grey40", col.axis = "grey40")
  mtext(ylab, side = 2, line = 2, col = "grey40", cex = 0.75)

  par(xpd = NA)
  legend(1955, max(gd1$y) * 1.38, legend = levels(all_landings$gear)[1:3],
    col =  pal[1:3], pch = 21, pt.bg = pal[1:3], bty = "n", cex = 0.75,
    text.col = "grey40", ncol = 1)
  
  N <- length(levels(all_landings$gear))
  legend(1977, max(gd1$y) * 1.38, legend = levels(all_landings$gear)[4:N],
    col =  pal[4:N], pch = 21, pt.bg = pal[4:N], bty = "n", cex = 0.75,
    text.col = "grey40", ncol = 1)
}

dir.create("catches", showWarnings = FALSE)

## run

cm <- reshape2::melt(filter(catches,
  !species_common_name %in% "unknown fish"), 
  id.vars = c("year", "species_common_name", "gear"))
landings <- filter(cm, variable %in% c("landed_kg"))
discards <- filter(cm, variable %in% c("discarded_kg"))

landings$gear <- as.character(landings$gear)
discards$gear <- as.character(discards$gear)

discards$gear <- "Discarded"

all_landings <- bind_rows(landings, discards)

all_landings <- mutate(all_landings, gear = forcats::fct_relevel(gear,
  "Bottom trawl",
  "Midwater trawl",
  "Hook and line",
  "Trap",
  "Unknown/trawl",
  "Discarded"))

all_landings <- group_by(all_landings, year, species_common_name, gear) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup()

source("R/make-spp-list.R")
torun <- get_spp_names()$species_common_name

stopifnot(all(torun %in% all_landings$species_common_name))

for (i in seq_along(torun)) {
  xx <- filter(all_landings, species_common_name == torun[i])
  message(torun[i])
  pdf(paste0("catches/", gsub("/", "-", gsub(" ", "-",
    unique(xx$species_common_name))), ".pdf"),
    width = 3, height = 2.1)
  plot_catch(xx)
  dev.off()
}

#' 
#' pal <- RColorBrewer::brewer.pal(5, "Greys")
#' 
#' #' @param xfrac The fraction over from the left side.
#' #' @param yfrac The fraction down from the top.
#' #' @param label The text to label with.
#' #' @param pos Position to pass to text()
#' #' @param ... Anything extra to pass to text(), e.g. cex, col.
#' add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
#'   u <- par("usr")
#'   x <- u[1] + xfrac * (u[2] - u[1])
#'   y <- u[4] - yfrac * (u[4] - u[3])
#'   text(x, y, label, pos = pos, ...)
#' }
#' 
#' plot_catch <- function(a, b, lab) {
#'   plot(1, 1, type = "n", xlim = c(1960, 2017) + c(-0.5, 0.5), ylim = c(0, max(a + b)*1.03), 
#'     axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
#'   abline(v = seq(1900, 2020, 10), col = "grey90")
#'   # polygon(c(x$year, rev(x$year)), c(rep(0, nrow(x)), rev(x$landed_kg + x$discarded_kg)), col = "red")
#'   # polygon(c(x$year, rev(x$year)), c(rep(0, nrow(x)), rev(x$discarded_kg)), col = "blue")
#'   rect(xleft = x$year - 0.5, xright = x$year + 0.5, ybottom = rep(0, nrow(x)), ytop = a + b,
#'     col = pal[[2]], border = "#00000090", lwd = 0.5)
#'   rect(xleft = x$year - 0.5, xright = x$year + 0.5, ybottom = rep(0, nrow(x)), ytop = b,
#'     col = pal[[3]], border = "#00000090", lwd = 0.5)
#'   box(col = "grey50")
#'   axis(2, col = "grey60", col.ticks = "grey70", col.axis = "grey30", las = 1)
#'   
#'   add_label(-0.008, 0.09, lab, col = "grey30", cex = 1)
#'   
#' }
#' 
#' for(i in unique(cm$species_common_name)) {
#'   pdf(paste0("catch/", gsub("/", "-", gsub(" ", "-", i)), ".pdf"), width = 3, height = 2.8)
#'   x <- filter(catches, species_common_name == i)
#'   par(mfrow = c(2, 1), oma = c(2, 1, .5, .5), cex = 0.7, mar = c(0, 2.5, 0, 0), mgp = c(2, 0.4, 0),
#'     tcl = -0.3)
#'   plot_catch(x$landed_kg/100000, x$discarded_kg/100000, "Trawl (10000 kg)")
#'   plot_catch(x$landed_pcs/1000, x$discarded_pcs/1000, "Longline (1000 fish)")
#'   axis(1, col = "grey60", col.ticks = "grey70", col.axis = "grey30")
#'   dev.off()
#' }



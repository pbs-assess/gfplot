plot_catch <- function(dat, xlim = c(1955, 2017)) {

  library(dplyr)
  library(ggplot2)

  pal <-  c(RColorBrewer::brewer.pal(n = length(unique(all_landings$gear))-2,
    "Paired"),"grey60", "grey30")[c(2, 1, 4, 3, 5, 6)]
  names(pal) <- levels(all_landings$gear)

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

  # more reliable 1996+
  rect(xleft = 1950, xright = 1996 - 0.5, ybottom = 0, ytop = max(gd1$y) * 1.1,
    border = NA, col = "#00000020")

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

d <- readRDS("data-cache/all-catches.rds")

library(dplyr)
library(ggplot2)

catches <- mutate(d, gear = dplyr::recode(gear,
  UNKNOWN = "Unknown/trawl",
  `BOTTOM TRAWL` = "Bottom trawl",
  `HOOK AND LINE` = "Hook and line",
  `MIDWATER TRAWL` = "Midwater trawl",
  `TRAP` = "Trap",
  `UNKNOWN TRAWL` = "Unknown/trawl"))


cm <- reshape2::melt(filter(catches,
  !species_common_name %in% "unknown fish"),
  id.vars = c("year", "species_common_name", "gear"))
landings <- filter(cm, variable %in% c("landed_kg"))
discards <- filter(cm, variable %in% c("discarded_kg"))

landings$gear <- as.character(landings$gear)
discards$gear <- as.character(discards$gear)
discards$gear <- "Discarded"

all_landings <- bind_rows(landings, discards)
all_landings <- mutate(all_landings,
  gear = forcats::fct_relevel(gear,
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


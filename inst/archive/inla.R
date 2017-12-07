library(INLA)
# library(rgdal)
library(tidyverse)
library(mapdata)
library(lubridate)
# library(splancs)
# library(sp)

####
d <- readRDS("../../Dropbox/dfo/data/all-survey-catches.rds")
names(d) <- tolower(names(d))
d$species_common_name <- tolower(d$species_common_name)
d$species_science_name <- tolower(d$species_science_name)
d$year <- lubridate::year(d$trip_start_date)

sp <- filter(d, species_common_name %in% "pacific ocean perch") %>% 
  filter(!is.na(catch_weight)) %>% 
  filter(year %in% seq(2004, 2012, 2))

dat <- sp
dat <- filter(dat, start_lon > -128, start_lon < 125, start_lat > 48, start_lat < 50.3)
nrow(dat)
dat <- filter(dat, fe_bottom_water_temperature > 1, fe_bottom_water_temperature < 12,
  !is.na(fe_bottom_water_temp_depth), !is.na(fe_bottom_water_temperature))
nrow(dat)
dat <- select(dat, year, start_lon, start_lat, catch_weight, fe_bottom_water_temp_depth) %>% 
  rename(X = start_lon, Y = start_lat)
attr(dat, "projection") <- "LL"
dat <- PBSmapping::convUL(dat)
dat <- as.data.frame(na.omit(dat))

subcoords = cbind(dat$X, dat$Y)

bnd = inla.nonconvex.hull(subcoords, convex = 30)
mesh1 = inla.mesh.2d(
  boundary = bnd,
  max.edge = c(15, 40),
  cutoff = 5,
  offset = 1
)
plot(mesh1)
points(dat$X, dat$Y, col = "red")
summary(mesh1)



##########
A.data <- inla.spde.make.A(mesh1, loc = cbind(dat$X, dat$Y))

# Make SPDE based on mesh
spde = inla.spde2.matern(mesh1, alpha = 3 / 2)

n = nrow(dat)
YEARS <- unique(dat$year)
k = length(unique(dat$year))

# Make a design matrix where the first year is the intercept
# ...
dm <- matrix(nrow = nrow(dat), ncol = 2)
dm[,1] <- 1
dm[,2] <- dat$fe_bottom_water_temp_depth

iset <- inla.spde.make.index("i2D", n.spde = mesh1$n, n.group = k)

X.1 = dm
Covar.names <- c("intercept", "depth")
XX.list <- as.list(X.1)
effect.list <- list()
#   effect.list[[1]] <- c(iset, list(Intercept=1))
effect.list[[1]] <- c(iset)
for (Z in 1:ncol(X.1))
  effect.list[[Z + 1]] <- XX.list[[Z]]
names(effect.list) <- c("1", Covar.names)

### Make data stack.
A <-
  inla.spde.make.A(
    mesh = mesh1,
    loc = cbind(dat$X, dat$Y),
    group = dat$year
  )
A.list = list()
A.list[[1]] = A
for (Z in 1:ncol(X.1))
  A.list[[Z + 1]] <- 1

### Make projection points stack.
Ntrials <- rep(1, length(dat$Y))

sdat <-
  inla.stack(
    tag = 'stdata',
    data = list(
      y = dat$Y,
      link = 1,
      Ntrials = Ntrials
    ),
    A = A.list,
    effects = effect.list
  )

formula = as.formula(
  paste0(
    "y ~ -1 +",
    paste(Covar.names, collapse = "+"),
    "+ f(i2D, model=spde, group = i2D.group, control.group = list(model='ar1'))"
  )
)		# field evolves with AR1 by year

inlaModel <-
  inla(
    formula,
    family = "gamma",
    data = inla.stack.data(sdat),
    control.predictor = list(compute = TRUE, A = inla.stack.A(sdat)),
    verbose = TRUE,
    debug = TRUE,
    keep = FALSE,
    control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE),
    control.fixed = list(correlation.matrix = TRUE),
    control.inla = list(lincomb.derived.correlation.matrix = TRUE)
  )
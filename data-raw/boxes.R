boxes <- readRDS("generated-data/boxes.rds")

library(dplyr)
surv <- names(boxes)
boxes <- lapply(boxes, function(x)
  tibble(xmin = x[[1]][1], xmax = x[[1]][2], ymin = x[[2]][[1]], ymax = x[[2]][2])) %>% bind_rows()

boxes$survey <- surv
# boxes2 <- lapply(boxes, function(x)
#   data.frame(x = c(x[[1]][1], x[[1]][1], x[[1]][2], x[[1]][2]),
#     y = c(x[[2]][1], x[[2]][1], x[[2]][2], x[[2]][2]))) %>%
#   bind_rows()

# boxes2
# boxes2$survey <- rep(surv, each = 4L)

# boxes <- boxes2
boxes$xmin <- boxes$xmin * 10
boxes$ymin <- boxes$ymin * 10
boxes$xmax <- boxes$xmax * 10
boxes$ymax <- boxes$ymax * 10

# load("R/sysdata.rda")
usethis::use_data(boxes, internal = TRUE, overwrite = TRUE)

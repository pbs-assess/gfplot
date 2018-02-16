d <- data.frame(x = 1:100, y = rnorm(100))
d$z <- gl(4, 25)
library(tidyverse)
g <- ggplot(d, aes(x, y)) + geom_point()
ggsave("R/a.pdf", width = 4, height = 4)
ggsave("R/b.pdf", width = 4, height = 8)

g <- ggplot(d, aes(x, y)) + geom_point() +
  facet_wrap(~z)
ggsave("R/c.pdf", width = 4, height = 4)

# system("pdflatex R/test.tex")

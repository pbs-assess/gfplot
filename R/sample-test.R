true_ages <- rpois()
age_dist <- function(N, bias = 0) {
  rpois(N, lambda = 5)
}
# hist(age_dist(1000))

# set.seed(1)
nfeid <- 200
frac_sampled <- rbeta(nfeid, shape1 = 0.5, shape2 = 0.5)
# hist(frac_sampled)
catch <- rlnorm(nfeid, meanlog = log(1500), sdlog = log(1.6))
# hist(catch)
avg_weight <- 5

d <- data.frame(
  feid = seq_len(nfeid),
  tid = as.numeric(gl(50, 2)),
  catch = catch,
  frac_sampled = frac_sampled,
  avg_weight = avg_weight) %>% as_tibble()

d <- mutate(d, sampled_weight = catch * frac_sampled,
  sampled_fish = round(sampled_weight / avg_weight))

ages <- group_by(d, feid) %>%
  do({data.frame(ages = age_dist(.$sampled_fish[[1]]))})

d <- inner_join(d, ages, by = "feid")

dfreq <- group_by(d, feid, ages) %>%
  summarise(freq = n()) %>%
  ungroup()

dfreq <- inner_join(dfreq, unique(select(d, -ages)), by = "feid")

g1 <- mutate(dfreq, all_counted = sum(freq)) %>%
  # re-weight:
  mutate(age_freq_scaled = freq * catch / sampled_weight,
    sum_age_freq_scaled = sum(age_freq_scaled)) %>%
  group_by(ages) %>%
  summarise(age_prop = sum(age_freq_scaled) / sum_age_freq_scaled[1]) %>%
  ungroup() %>%
  ggplot(aes(ages, age_prop)) + geom_col() + ggtitle("Sampled") +
  xlim(0, 25)

age_dist_true <- data.frame(ages = age_dist(2e6))
g2 <- mutate(age_dist_true, all_counted = nrow(age_dist_true)) %>%
  group_by(ages) %>%
  summarise(age_prop = n()/all_counted[1]) %>%
  ungroup() %>%
  ggplot(aes(ages, age_prop)) + geom_col() + ggtitle("True") +
  xlim(0, 25)

gridExtra::grid.arrange(g1, g2, ncol = 1)

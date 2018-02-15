data {
  int<lower=1> N;
  vector[N] length;
  vector[N] age;
  real<lower=0> linf_upper_sd;
}
parameters {
  real<lower=0> k;
  real<lower=0> linf;
  real<lower=0> sigma;
  real t0;
}
model {
  k ~ normal(0, 2);
  linf ~ normal(0, linf_upper_sd);
  sigma ~ student_t(3, 0, 2);
  t0 ~ normal(0, 20);
  length ~ lognormal(log(linf * (1 - exp(-k * (age - t0)))), sigma);
}

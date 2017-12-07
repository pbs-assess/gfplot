data {
  int<lower=1> N;
  vector[N] length;
  vector[N] age;
}
parameters {
  real<lower=0> k;
  real<lower=0> linf;
  real<lower=0> sigma;
  real t0;
}
model {
  // k ~ normal(0, 5);
  // linf ~ normal(0, 300);
  // sigma ~ normal(0, 2);
  // t0 ~ normal(0, 50);
  length ~ lognormal(log(linf * (1 - exp(-k * (age - t0)))), sigma);
}

data {
  int<lower=0> N;
  int<lower=0> J;
  matrix[N,J] X;
  int y[N];
}
parameters {
  vector[J] beta;
}
model {
  vector[N] mu;
  mu = X * beta;
  beta ~ normal(0, 1);
  y ~ bernoulli_logit(mu);
}

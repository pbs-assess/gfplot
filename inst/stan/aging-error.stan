data {
  int<lower=1> N;
  vector[N] age1;
  vector[N] se1;
  vector[N] age2;
  vector[N] se2;
}
parameters {
  real<lower=0> sigma;
  real age1_true[N];
  real age2_true[N];
}
model {
  sigma ~ normal(0, 5);

  age1 ~ normal(age1_true, se1);
  age2 ~ normal(age2_true, se2);
  age2_true ~ normal(age1_true, sigma);
}

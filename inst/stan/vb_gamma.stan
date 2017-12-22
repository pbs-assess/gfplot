data {
  int<lower=1> N;
  vector[N] length;
  vector[N] age;
  int<lower=1> N_pred;
  vector<lower=0>[N_pred] age_pred;
  // real<lower=0> cv_age;
}
parameters {
  real<lower=0> k;
  real<lower=0> linf;
  real<lower=0> cv_length;
  real t0;
  // vector[N] age_true;
}
transformed parameters {
  // real<lower=0> alpha_age;
  real<lower=0> alpha_length;
  // alpha_age = inv(pow(cv_age, 2.0));
  alpha_length = inv(pow(cv_length, 2.0));
}
model {
  // priors:
  k ~ normal(0, 5);
  linf ~ normal(0, 300);
  t0 ~ normal(0, 50);
  cv_length ~ student_t(3, 0, 2);

  // aging measurement error:
  // age ~ gamma(alpha_age, alpha_age ./ exp(age_true));

  // likelihood:
  length ~ gamma(alpha_length, alpha_length ./ (linf * (1 - exp(-k * (age - t0)))));
}
generated quantities {
  vector[N_pred] length_pred;
  for (i in 1:N_pred) {
    length_pred[i] = linf * (1 - exp(-k * (age_pred[i] - t0)));
  }
}

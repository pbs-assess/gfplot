functions {

real get_linf(real k, data real a1, data real a2, real L1, real L2, real p) {
  real linf_num;
  real linf_den;
  real linf;

  linf_num = exp(k * a2) * pow(L2, p);
  linf_num -= exp(k * a1) * pow(L1, p);

  linf_den = exp(k * a2) - exp(k * a1);
  linf = pow(linf_num/linf_den, 1/p);
  return linf;
}

real get_t0(real k, data real a1, data real a2, real L1, real L2, real p) {
  real linf_num;
  real t0_den;
  real t0;

  linf_num = exp(k * a2) * pow(L2, p);
  linf_num -= exp(k * a1) * pow(L1, p);

  t0_den = pow(L2, p) - pow(L1, p);
  t0 = -log(linf_num/t0_den)/k;
  t0 += a1 + a2;
  return t0;
}

vector get_eta(data int N, data vector age, real k, data real a1, data real a2, real L1, real L2, real p) {

  vector[N] eta;
  vector[N] tmp;

  real tmp1 = pow(L1, p);
  real tmp2 = pow(L2, p) - tmp1;
  tmp2 /= 1 - exp(-k * (a2 - a1));

  tmp = 1 - exp(-k * (age - a1));
  tmp *= tmp2;
  tmp += tmp1;

  for (i in 1:N) eta[i] = pow(tmp[i], 1/p);

  return eta;
}

}
data {
  int<lower=1> N;
  vector[N] length;
  vector[N] age;
  real<lower=0> len_upper_sd;
  real a1;
  real a2;
}
parameters {
  real<lower=0> k;
  real<lower=0> L1;
  real<lower=0> L2;
  real<lower=0> sigma;
  real p;
}
model {
  k ~ normal(0, 2);
  L1 ~ normal(0, len_upper_sd);
  L2 ~ normal(0, len_upper_sd);
  sigma ~ student_t(3, 0, 2);
  p ~ normal(0, 5);
  length ~ lognormal(log(get_eta(N, age, k, a1, a2, L1, L2, p)), sigma);
}
generated quantities {
  real linf = get_linf(k, a1, a2, L1, L2, p);
  real t0 = get_t0(k, a1, a2, L1, L2, p);
}

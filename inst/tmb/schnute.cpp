#include <TMB.hpp>

template<class Type>
Type dlnorm(Type x, Type meanlog, Type sdlog, int give_log=0){
  Type logres;
  logres = dnorm( log(x), meanlog, sdlog, true) - log(x);
  if(give_log)return logres; else return exp(logres);
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(len);
  DATA_VECTOR(age);
  DATA_SCALAR(a1);
  DATA_SCALAR(a2);

  PARAMETER(k);
  PARAMETER(L1);
  PARAMETER(L2);
  PARAMETER(log_sigma);
  PARAMETER(p); // vB: p = 1; Richards: 0 < p < 1 with p = ratio of length of inflection to Linf

  int n = len.size();
  vector<Type> eta(n);
  Type nll = 0.0;

  Type tmp1 = pow(L1, p);
  Type tmp2 = pow(L2, p) - tmp1;
  tmp2 /= 1 - exp(-k * (a2 - a1));
  Type sigma = exp(log_sigma);

  for(int i = 0; i < n; i++){
    Type tmp = 1 - exp(-k * (age(i) - a1));
    tmp *= tmp2;
    tmp += tmp1;
    eta(i) = pow(tmp, 1/p);
    nll -= dlnorm(len(i), log(eta(i)) - 0.5 * sigma * sigma, sigma, true);
  }

  Type linf_num = exp(k * a2) * pow(L2, p);
  linf_num -= exp(k * a1) * pow(L1, p);

  Type linf_den = exp(k * a2) - exp(k * a1);
  Type linf = pow(linf_num/linf_den, 1/p);

  Type t0_den = pow(L2, p) - pow(L1, p);
  Type t0 = -log(linf_num/t0_den)/k;
  t0 += a1 + a2;

  ADREPORT(linf);
  ADREPORT(t0);
  REPORT(linf);
  REPORT(t0);
  REPORT(eta);

  return nll;
}

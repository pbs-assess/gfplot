#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(len);
  DATA_VECTOR(weight);
  DATA_SCALAR(df);

  PARAMETER(log_a);
  PARAMETER(b);
  PARAMETER(log_sigma);

  int n = len.size();
  vector<Type> eta(n);
  Type nll = 0.0;

  for(int i = 0; i < n; i++){
    eta(i) = log_a + b * len(i);
    // nll -= log(Type(1.0) / exp(log_sigma) * dt((weight(i) - eta(i)) / exp(log_sigma), df, false));
    // from metRology::dt.scaled
    // stats::dt((x - mean)/sd, df, ncp = ncp, log = TRUE) - log(sd)
    nll -= dt((weight(i) - eta(i))/exp(log_sigma), df, true) - log_sigma;
  }

  return nll;
}

// Simple linear regression.
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  DATA_VECTOR(x);
  PARAMETER(a);
  PARAMETER(b);

  int n = y.size(); // get number of data points to loop over
  Type nll = 0.0; // initialize negative log likelihood
  Type size = 1.0;

  for(int i = 0; i < n; i++){
    Type linear_predictor = a + b * x(i);

    nll -= dbinom_robust(y(i), size, linear_predictor, true);
    // nll -= dnorm(y(i), linear_predictor, 1.0, true);
  }

  return nll;
}

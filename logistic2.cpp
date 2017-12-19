// Simple linear regression.
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y_i);
  DATA_MATRIX(X_ij)
  PARAMETER_VECTOR(b_j);

  int n = y_i.size(); // get number of data points to loop over
  Type jnll = 0.0; // initialize joint negative log likelihood

  // Linear predictor
  vector<Type> linear_predictor_i(n);
  linear_predictor_i = X_ij * b_j;

  for(int i = 0; i < n; i++){
    Type size = 1.0;
    jnll -= dbinom_robust(y_i(i), size, linear_predictor_i(i), true);
  }

  return jnll;
}

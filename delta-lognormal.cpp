#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y1_i);
  DATA_VECTOR(y2_i);
  DATA_MATRIX(X1_ij)
  DATA_MATRIX(X2_ij)
  PARAMETER_VECTOR(b1_j);
  PARAMETER_VECTOR(b2_j);
  PARAMETER(log_sigma);

  int n1 = y1_i.size(); // get number of data points to loop over
  Type jnll = 0.0; // initialize joint negative log likelihood

  // Linear predictor
  vector<Type> linear_predictor1_i(n);
  vector<Type> linear_predictor2_i(n);
  linear_predictor1_i = X1_ij * b1_j;
  linear_predictor2_i = X2_ij * b2_j;

  Type size = 1.0;
  for(int i = 0; i < n1; i++){
    jnll -= dbinom_robust(y1_i(i), size, linear_predictor1_i(i), true);
  }
  for(int i = 0; i < n2; i++){
    jnll -= dnorm(y2_i(i), linear_predictor2_i(i), exp(log_sigma), true);
  }

  return jnll;
}

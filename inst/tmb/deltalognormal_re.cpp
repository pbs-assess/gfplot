#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y1_i);
  DATA_VECTOR(y2_i);
  DATA_MATRIX(X1_ij);
  DATA_MATRIX(X2_ij);

  DATA_IVECTOR(factor1_i);
  DATA_IVECTOR(factor2_i);
  DATA_INTEGER(nk1);
  DATA_INTEGER(nk2);

  DATA_MATRIX(X1_pred_ij);
  DATA_MATRIX(X2_pred_ij);

  PARAMETER_VECTOR(b1_j);
  PARAMETER_VECTOR(b2_j);

  PARAMETER_VECTOR(z1_k);
  PARAMETER_VECTOR(z2_k);

  PARAMETER(log_sigma);
  PARAMETER(log_sigma_z1);
  PARAMETER(log_sigma_z2);

  int n1 = y1_i.size();
  int n2 = y2_i.size();

  Type jnll = 0.0; // initialize joint negative log likelihood

  // Linear predictor
  vector<Type> linear_predictor1_i(n1);
  vector<Type> linear_predictor2_i(n2);
  linear_predictor1_i = X1_ij * b1_j;
  linear_predictor2_i = X2_ij * b2_j;

  for(int i = 0; i < n1; i++){
    jnll -= dbinom_robust(y1_i(i), Type(1.0),
      linear_predictor1_i(i) + z1_k(factor1_i(i)), true);
  }
  for(int i = 0; i < n2; i++){
    jnll -= dnorm(y2_i(i),
      linear_predictor2_i(i) + z2_k(factor2_i(i)), exp(log_sigma), true);
  }

  // Probability of random coefficients
  for(int k = 0; k<nk1; k++){
    jnll -= dnorm(z1_k(k), Type(0.0), exp(log_sigma_z1), true);
  }
  for(int k = 0; k<nk2; k++){
    jnll -= dnorm(z2_k(k), Type(0.0), exp(log_sigma_z2), true);
  }

  Type sigma = exp(log_sigma);
  REPORT(sigma);

  vector<Type> linear_prediction1_i(n1);
  vector<Type> linear_prediction2_i(n2);
  linear_prediction1_i = X1_pred_ij * b1_j;
  linear_prediction2_i = X2_pred_ij * b2_j;

  vector<Type> prediction =
    exp(linear_prediction1_i) / (exp(linear_prediction1_i) + 1) *
    exp(linear_prediction2_i);

  vector<Type> log_prediction = log(prediction);
  REPORT(prediction);
  ADREPORT(prediction);
  REPORT(log_prediction);
  ADREPORT(log_prediction);

  return jnll;
}

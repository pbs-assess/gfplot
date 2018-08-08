#include <TMB.hpp>

template<class Type>
Type invlogit_p1(Type x){
  return 1.0 / (1.0 + exp(-x)) + 1.0;
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y1_i);
  DATA_MATRIX(X1_ij);

  DATA_IVECTOR(factor1k_i);
  DATA_INTEGER(nk1);

  PARAMETER_VECTOR(b1_j);
  PARAMETER(log_phi);
  PARAMETER(logit_p);

  PARAMETER_VECTOR(z1_k);
  PARAMETER(log_sigma_zk1);

  DATA_MATRIX(X1_pred_ij);

  int n1 = y1_i.size();

  Type jnll = 0.0; // initialize joint negative log likelihood

  // Linear predictor
  vector<Type> linear_predictor1_i(n1);
  linear_predictor1_i = X1_ij * b1_j;

  for(int i = 0; i < n1; i++){
    jnll -= dtweedie(y1_i(i),
        exp(linear_predictor1_i(i) + z1_k(factor1k_i(i))),
        exp(log_phi), invlogit_p1(logit_p), true);
  }

  // Probability of random coefficients
  for(int k = 0; k<nk1; k++){
    jnll -= dnorm(z1_k(k), Type(0.0), exp(log_sigma_zk1), true);
  }

  vector<Type> log_prediction(X1_pred_ij.rows());
  log_prediction = X1_pred_ij * b1_j;

  REPORT(log_prediction);
  ADREPORT(log_prediction);

  return jnll;
}

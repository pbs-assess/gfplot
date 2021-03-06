#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y1_i);
  DATA_VECTOR(y2_i);
  DATA_MATRIX(X1_ij);
  DATA_MATRIX(X2_ij);
  PARAMETER_VECTOR(b1_j);
  PARAMETER_VECTOR(b2_j);
  PARAMETER(log_shape);

  DATA_MATRIX(X1_pred_ij);
  DATA_MATRIX(X2_pred_ij);

  int n1 = y1_i.size(); // get number of data points to loop over
  int n2 = y2_i.size(); // get number of data points to loop over
  Type jnll = 0.0; // initialize joint negative log likelihood

  // Linear predictor
  vector<Type> linear_predictor1_i(n1);
  vector<Type> linear_predictor2_i(n2);
  linear_predictor1_i = X1_ij * b1_j;
  linear_predictor2_i = X2_ij * b2_j;

  Type size = 1.0;
  for(int i = 0; i < n1; i++){
    jnll -= dbinom_robust(y1_i(i), size, linear_predictor1_i(i), true);
  }
  Type shape = exp(log_shape);
  for(int i = 0; i < n2; i++){
    jnll -= dgamma(y2_i(i), shape, exp(linear_predictor2_i(i)) / shape, true);
  }

  vector<Type> linear_prediction1_i(n1);
  vector<Type> linear_prediction2_i(n2);
  linear_prediction1_i = X1_pred_ij * b1_j;
  linear_prediction2_i = X2_pred_ij * b2_j;

  vector<Type> prediction =
    exp(linear_prediction1_i) / (exp(linear_prediction1_i) + 1) *
    exp(linear_prediction2_i);
  vector<Type> log_prediction = log(prediction);

  REPORT(log_prediction);
  ADREPORT(log_prediction);

  REPORT(linear_prediction1_i);
  ADREPORT(linear_prediction1_i);

  REPORT(linear_prediction2_i);
  ADREPORT(linear_prediction2_i);

  Type cv = sqrt(1.0 / shape);
  REPORT(cv);
  ADREPORT(cv);

  return jnll;
}

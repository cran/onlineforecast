// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
using namespace Rcpp;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

//
// via the exports attribute we tell Rcpp to make this function
// available from R
//

//' Low pass filtering of a vector.
//' 
//' This function returns a vector which is x through a unity gain first-order low-pass filter.
//'
//' @name lp_vector_cpp
//' @param x A numeric vector
//' @param a1 the first order low-pass filter coefficient

// [[Rcpp::export]]

NumericVector lp_vector_cpp(NumericVector x, double a1) {
  int n = x.size();
  NumericVector y(n);
  double oma1 = (1-a1);

  // First value in x is the init value of y
  y[0] = x[0];

  for(int i = 1; i < n; ++i) {
    if(NumericVector::is_na(y[i-1])){
      y[i] = x[i];
    }else{
      y[i] = a1*y[i-1] + oma1*x[i];
    }
  }
  // Return (afterwards the init value (i.e. first value in y[0]), must be handled)
  return y;
}



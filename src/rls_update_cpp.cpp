// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
using namespace Rcpp;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

//' Calculating k-step recursive least squares estimates
//' 
//' This function applies the k-step recursive least squares scheme to estimate
//' parameters in a linear regression model.
//'
//' @name rls_update_cpp
//' @param y Vector of observation
//' @param X Matrix of input variables (design matrix)
//' @param theta Vector of parameters (initial value)
//' @param P Covariance matrix (initial value)
//' @param lambda Forgetting factor
//' @param k Forecast horizon
//' @param n Length of the input
//' @param np Dimension of P (np x np)
//' @param istart Start index
//' @param kmax Keep only the last kmax rows for next time

// [[Rcpp::export]]

Rcpp::List rls_update_cpp(arma::vec y,
                          arma::mat X, // 
			  arma::vec theta, // 
                          arma::mat P, // 
                          double lambda, // 
                          unsigned int k, //
                          unsigned int n, // length of input
                          unsigned int np, // dimension of P (npxnp)
                          unsigned int istart,
                          unsigned int kmax
) {

  // -9999 will be treated as NA in R
  arma::mat Theta(n, np);  Theta.fill(NA_REAL); // The parameter matrix
  NumericVector yhat(n);  yhat.fill(NA_REAL); // The predictions k steps ahead
  // Not used: NumericVector yhat_lagged(n); yhat_lagged.fill(NA_REAL); // The predictions synced with y, e.g. y-yhat_lagged is the prediction error
  // arma::vec SigmaPredLocal(n);  SigmaPredLocal.fill(-9999); // The local variance of prediction
  // arma::vec SigmaPredGlobal(n);  SigmaPredGlobal.fill(-9999); // The global variance of prediction
  // arma::vec SigmaUpLocal(n); SigmaUpLocal.fill(-9999); // The local variance of update
  // arma::vec SigmaUpGlobal(n); SigmaUpGlobal.fill(-9999); // The global variance of update
  // arma::vec CUp(n); CUp.fill(-9999);
  // arma::vec CPred(n); CPred.fill(-9999);
  // arma::vec DegreesUp(n); DegreesUp.fill(-9999); // The degrees of freedom used for update
  // arma::vec DegreesPred(n); DegreesPred.fill(-9999); // The degrees of freedom used for prediction
  // arma::vec Lambda(n); Lambda.fill(-9999); // The forgetting factor used
  // arma::vec TraceP(n); TraceP.fill(-9999); // The trace of P after update

  arma::vec K(np);
  arma::vec x(np);
  double sigsca;
  double cup;//, cpred;
  // double sigmaupglobal;
  // double sigmauplocal;
  // double sigmapredglobal;
  // double sigmapredlocal;
  double err;
  // double err2;
  // double cerr2 = 0;
  // int degrees = 0;

  // Return
  Rcpp::List fit;
  Rcpp::List result;

  // Iterate through
  for(unsigned int i = istart-1 ; i < n; i++) {

    // Take the forecast k steps back to match it with y[i]
    // The regressor vector, take the forecasts k steps back from X
    x = arma::trans(X.row(i-k));

    if(!NumericVector::is_na(y[i]) && !x.has_nan()){
      // Update
      cup = arma::as_scalar(x.t() * P * x);
      sigsca = arma::as_scalar(lambda + cup);

      K = (P * x) / sigsca; //gain
      err = arma::as_scalar(y[i] - x.t() * theta); //residual
      // err2 = err*err; // squared residual
      // cerr2 += err2; // cumulated squared residual
      // degrees +=1;
      // sigmauplocal = sqrt(cerr2/degrees);
      // sigmaupglobal = sqrt(cerr2/(degrees*sigsca));

      // Ordinary RLS P  and theta update
      P = (1/lambda) * P - (K * x.t() * P);
      theta += K * err;

      // Save results
      //      TraceP(i) = trace(P);
      //Lambda(i) = lambda;
      Theta.row(i) = theta.t();
      //SigmaUpLocal(i) = sigmauplocal;
      //SigmaUpGlobal(i) = sigmaupglobal;
      //CUp(i) = cup;
      //DegreesUp(i) = degrees;
    }

    // Make the prediction taking the forecasts at the step i
    x = arma::trans(X.row(i));
//    if(!x.has_nan()){
      yhat[i] = arma::as_scalar(x.t() * theta);
//    }
  }

  // Keep the fit
  // First only last part of yhat
  arma::vec tmp(kmax);
  for(unsigned int i = 0; i < kmax; i++) {
    tmp[i] = yhat[n-kmax+i];
  }
  fit = List::create(Named("k") = k,
                           Named("theta") = theta,
                           Named("P") = P,
                           Named("yhat") = tmp);

  // Keep the result
  // // First lag yhat k steps
  // for(unsigned int i = k; i < n; i++) {
  //   yhat_lagged[i] = yhat[i-k];
  // }
  // 
  // Maybe give names to the matrices (its just missing innames as an argument)
  //Theta.attr("dimnames") = Rcpp::List::create(NULL, innames)
  //
  result = List::create(Named("yhat") = yhat,
                        Named("Theta") = Theta);
  // Return Theta and the result
  return List::create(Named("fit") = fit,
                      Named("result") = result);
}

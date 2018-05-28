#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector calculate_principal(NumericVector rates,
                                  double loan_amount,
                                  double monthly_payment) {
  int n = rates.size();
  NumericVector principal(n);
  
  for(int i = 0; i < n; i++){
    if(i == 0){
      principal[i] = loan_amount;
    } else {
      principal[i] = principal[i - 1] * rates[i - 1] - monthly_payment;
    }
  }
  return principal;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


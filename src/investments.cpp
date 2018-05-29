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
List simulate(NumericMatrix contributions,
              NumericVector initial,
              double mean_ror,
              double sd_ror,
              double n_sims = 100){
  
  List simulations(n_sims);
  
  for(int n_sim = 0; n_sim < n_sims; n_sim++){
    int n_periods = contributions.nrow();
    int n_buckets = contributions.ncol();
    
    NumericMatrix result(n_periods, n_buckets + 2);
    NumericVector rates = rnorm(n_periods, mean_ror, sd_ror);
      
    for(int period = 0; period < n_periods; period++){
      for(int bucket = 0; bucket < n_buckets; bucket++){
        if(period == 0){
          result(period, bucket) = initial[bucket] + contributions(period, bucket);
        } else{
          result(period, bucket) = result(period - 1, bucket) * rates[period - 1] + 
            contributions(period, bucket);
        }
      }
      result(period, n_buckets) = period + 1;
      result(period, n_buckets + 1) = n_sim + 1;
    }
    
    simulations[n_sim] = result;
  }
  
  return simulations;
}




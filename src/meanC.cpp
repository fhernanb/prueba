#include <Rcpp.h>
using namespace Rcpp;

//' Media de un vector usando C++.
//' @param x numeric vector
//' @export
//' @return la media.
// [[Rcpp::export]]
 double meanC(NumericVector x) {
   int n = x.size();
   double total = 0;
   
   for(int i = 0; i < n; ++i) {
     total += x[i];
   }
   return total / n;
 }

//' Varianza de un vector usando C++.
//' @param x numeric vector
//' @export
//' @return la varianza.
// [[Rcpp::export]]
 double varC(NumericVector x) {
   int n = x.size();
   double total = 0;
   
   double media;
   media = meanC(x);
   
   for(int i = 0; i < n; ++i) {
     total += pow(x[i] - media, 2);
   }
   return total / (n-1);
 }

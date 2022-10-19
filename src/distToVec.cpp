#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector distSqrToVec(NumericMatrix target, NumericVector query) {
  int n = target.nrow(), d = target.ncol();
  NumericVector out(n);
  double dst;
  double v;
  for (int i = 0; i < n; ++i) {
    dst = 0;
    for (int j = 0; j < d; ++j) {
      v = target(i, j) - query(j);
      dst += v*v;
    }
    out[i] = dst;
  }
  return out;
}

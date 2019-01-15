#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector cumVEtoInsFast(
	Rcpp::NumericVector ve_c,
	Rcpp::NumericVector rate_p
){
	int ve_c_N = ve_c.size();
	Rcpp::NumericVector ve_i(ve_c_N);
	
	int rate_N = rate_p.size();
	long double diff = 0.0;
	long double ratediff = 0.0;
	
	int t = 0;
	int z = 0;
	
	for( t = 0; t < ve_c_N; t++ ){
		if(t == 0){
			ve_i[t] = ve_c[t];
		} else {
			diff = 0.0;
			for( z = 0; z < t; z++ ){
				if(rate_N == 1){
					ratediff = 1.0;
					diff += (ve_c[t] - ve_i[z]);
				} else {
					ratediff = rate_p[z]/rate_p[t];
					diff += (ve_c[t] - ve_i[z])*ratediff;
				}
			}
			ve_i[t] = ve_c[t] + diff;
		}
	}
	return(ve_i);
}
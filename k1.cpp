#include <Rcpp.h>
#include<iostream>
#include<limits>
using namespace Rcpp;
//[[Rcpp::export]]
double my_knn_C(NumericMatrix X,NumericVector X0,NumericVector y) {
  int nrows=X.nrow();
  int ncols=X.ncol();
  double closest_distance=99999999;
  double closest_output=-1;
  double closest_neighbor=-1;
  int i;
  int j;
  for(i=0;i<nrows;++i){
    double dist=0;
    for(j=0;j<ncols;++j){
      double difference=X(i,j)-X0[j];
      dist=dist+difference*difference;
    }
    double distance=sqrt(dist);
    if(distance<closest_distance){
      closest_distance=distance;
      closest_output=y[i];
      closest_neighbor=i;
    }
  }
  return(closest_output); 
}
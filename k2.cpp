#include <Rcpp.h>
#include<iostream>
#include<limits>
using namespace Rcpp;
//[[Rcpp::export]]
double my_knn2_C(NumericMatrix X,NumericVector X0,NumericVector y) {
  int nrows=X.nrow();
  int ncols=X.ncol();
  double fclosest_distance=99999999;
  double fclosest_output=-1;
  double fclosest_neighbor=-1;
  double closest_distance=99999999;
  double closest_output=-1;
  double closest_neighbor=-1;
  double sum;
  double output;
  int i;
  int j;
  for(i=0;i<nrows;++i){
    double dist=0;
    for(j=0;j<ncols;++j){
      double difference=X(i,j)-X0[j];
      dist=dist+difference*difference;
    }
    double distance=sqrt(dist);
    if(distance<fclosest_distance){
      fclosest_distance=distance;
      fclosest_output=y[i];
      fclosest_neighbor=i;
    }
  }
  for(i=0;i<nrows;++i){
    double dist=0;
    for(j=0;j<ncols;++j){
      double difference=X(i,j)-X0[j];
      dist=dist+difference*difference;
    }
    double distance=sqrt(dist);
    if((distance<closest_distance)&(i!=fclosest_neighbor)){
      closest_distance=distance;
      closest_output=y[i];
      closest_neighbor=i;
    }
    }
  sum=fclosest_output+closest_output;
  output=sum/2;
  return(output); 
}
  
  

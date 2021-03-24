#include <Rcpp.h>
#include<iostream>
#include<limits>
using namespace Rcpp;
//[[Rcpp::export]]
double my_knn_inverse_C(NumericMatrix X,NumericVector X0,NumericVector y, int k) {
  int nrows=X.nrow();
  int ncols=X.ncol();
  double fclosest_distance=99999999;
  double fclosest_output=-1;
  double fclosest_neighbor=-1;
  double closest_distance=99999999;
  double closest_output=-1;
  double closest_neighbor=-1;
  double sum,mean;
  double output1,output2,output3;
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
  if(k==1){return(fclosest_output);}
  if(k==2){
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
   output1=fclosest_output/fclosest_distance;
  output2=closest_output/closest_distance;
 output3=(output1+output2)/((1/fclosest_distance)+(1/closest_distance));
 if(k==2){return(output3);}
  }
}
  
  

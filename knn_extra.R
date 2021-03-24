###############################################################################
# Code for KNN starts here. the prediction is knn for weighted inverse of distance
my_knn_inverse_R= function(X, X0, y,k){
  # k is the number of closest neighbor
  nrows = nrow(X)
  ncols = ncol(X)
  fclosest_distance = 99999999
  fclosest_output = -1
  fclosest_neighbor =-1
  closest_distance = 99999999
  closest_output = -1
  closest_neighbor =-1
  # to find the first closest neighbor and the prediction as output
  for(i in 1:nrows){
    distance = 0
    for(j in 1:ncols){
      difference = X[i,j]-X0[j]
      distance = distance + difference * difference
    }
    distance = sqrt(distance)
    if(distance < fclosest_distance){
      fclosest_distance = distance
      fclosest_output = y[i]
      fclosest_neighbor = i
    }
  }
  if(k==1){ return(fclosest_output)}
  # compare the distnces to the point except the first closest neighbor,
  #the point with minmum distance would be the second closest neighbor
  if(k==2){
   for(i in 1:nrows){
    distance = 0
    for(j in 1:ncols){
      difference = X[i,j]-X0[j]
      distance = distance + difference * difference
    }
    distance = sqrt(distance)
    if((distance < closest_distance)&&(i!= fclosest_neighbor)){
      closest_distance = distance
      closest_output = y[i]
      closest_neighbor = i
    }
  }
  output1=fclosest_output/ fclosest_distance
  output2=closest_output/ closest_distance
  output3=(output1+output2)/((1/fclosest_distance)+(1/closest_distance))
  return(output3)
  }

}  
# Code for KNN ends here. 
##############################################################################
## Here, we test the function we just programmed ###
#data
library(mlbench)
data("BostonHousing")
X <- BostonHousing[,c(-4, -14)]
y <- BostonHousing[,14]
X <- as.matrix(X)
X0 <- c(0.80,  0,  8.24, 0.6, 5.00, 80.0, 4.5,   4, 307, 21, 385, 13)
##################################################################################
my_knn_inverse_R(X, X0, y,1)
# fclosest_distance =3.129570,fclosest_output =17.5
#k=1,the prediction=closest_output
#[1] 17.5
my_knn_inverse_R(X, X0, y,2)
#[1] 17.70935
# closest_distance =7.334901,closest_output =18.2
#output1=fclosest_output/ fclosest_distance=5.591823
#output2=closest_output/ closest_distance=2.481288
#output3=(output1+output2)/((1/fclosest_distance)+(1/closest_distance))=17.096(by hand)
#k=2,the prediction is weighted inverse of distance equal to the formula "output3"
#17.70935 the result of the function my_knn_inverse_R(X, X0, y,2)
#load source of the function of "my_knn_inverse_C(X, X0, y,k)"
Rcpp::sourceCpp('knn_extra.cpp')
my_knn_inverse_C(X, X0, y,1)
#[1] 17.5
my_knn_inverse_C(X, X0, y,2)
#[1] 17.70935
#compare the time of these two version R and c++
library(microbenchmark)
microbenchmark(my_knn_inverse_R(X, X0, y,1),my_knn_inverse_C(X, X0, y,1))

#Unit: microseconds
#expr                              min       lq       mean   median       uq       max  neval
#my_knn_inverse_R(X, X0, y, 1) 7284.100 7626.100 8968.43460 8010.849 8479.072 61711.026   100
#my_knn_inverse_C(X, X0, y, 1)   15.398   17.424   42.26793   51.665   57.135   111.028   100
 
microbenchmark(my_knn_inverse_R(X, X0, y,2),my_knn_inverse_C(X, X0, y,2))
#
#Unit: microseconds
#expr                                 min       lq       mean    median        uq        max neval
#my_knn_inverse_R(X, X0, y, 2) 14104.637 15156.57 17518.0495 15696.311 17024.193 132386.973   100
#my_knn_inverse_C(X, X0, y, 2)    25.934    27.96    51.8107    29.783    70.507    222.462   100









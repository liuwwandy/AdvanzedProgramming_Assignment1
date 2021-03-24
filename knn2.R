###############################################################################
# Code for KNN starts here. k=2 the prediction is knn for regression 
my_knn2_R = function(X, X0, y){
# X data matrix with input attributes
# y response variable values of instances in X  
# X0 vector of input attributes for prediction
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
  # compare the distnces to the point except the first closest neighbor,
  #the point with minmum distance would be the second closest neighbor
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
  mean=(closest_output+fclosest_output)/2
  return(mean)
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
#k=2 in R
my_knn2_R(X, X0, y)
#[1] 17.85
#k=2 in C++ load the source file
Rcpp::sourceCpp('k2.cpp')
my_knn2_C(X, X0, y)
#[1] 17.85
#k=2 in FNN::knn.reg()

install.packages("FNN")
library(FNN)
FNN::knn.reg(X, matrix(X0, nrow = 1), y, k=2)
#Prediction:
# [1] 17.85
#the result are the same
##################################################################################
#compare the time used in R,c++ and FNN::knn.reg()
library(microbenchmark)
microbenchmark(my_knn2_R(X, X0, y))
microbenchmark(my_knn2_C(X, X0, y))
microbenchmark(FNN::knn.reg(X, matrix(X0, nrow = 1), y, k=2))
microbenchmark(FNN::knn.reg(X, matrix(X0, nrow = 1), y, k=2),my_knn2_R(X, X0, y),my_knn2_C(X, X0, y))
#Unit: microseconds
#expr                                                min         lq        mean     median         uq        max neval
#FNN::knn.reg(X, matrix(X0, nrow = 1), y, k = 2)   589.990   605.9955   800.01121   834.1305   884.9845   2056.454   100
#my_knn2_R(X, X0, y)                             14598.591 15334.4560 18392.47769 16395.1005 18617.2855 138391.412   100
#my_knn2_C(X, X0, y)                                25.934    28.1630    49.19712    44.5730    65.2400    233.403   100

# the c++ version is fastest of the three.




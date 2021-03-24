###############################################################################
# Code for KNN starts here. You have to translate this code into C++ / Rcpp
my_knn_R = function(X, X0, y){
# X data matrix with input attributes
# y response variable values of instances in X  
# X0 vector of input attributes for prediction
  nrows = nrow(X)
  ncols = ncol(X)
  # One of the instances is going to be the closest one:
  #   closest_distance: it is the distance , min_output
  closest_distance = 99999999
  closest_output = -1
  closest_neighbor = -1
  
  for(i in 1:nrows){
  
    distance = 0
    for(j in 1:ncols){
      difference = X[i,j]-X0[j]
      distance = distance + difference * difference
    }
    
    distance = sqrt(distance)
    
    if(distance < closest_distance){
      closest_distance = distance
      closest_output = y[i]
      closest_neighbor = i
    }
  }
 return(closest_output)
}  

# Code for KNN ends here. 
##############################################################################
## Here, we test the function we just programmed ###
#data
library(mlbench)
data("BostonHousing")
# X contains the input attributes, but attribute number 4 is removed because it is not numeric
X <- BostonHousing[,c(-4, -14)]
# y contains the response variable (named medv, a numeric value)
y <- BostonHousing[,14]
# From dataframe to matrix
X <- as.matrix(X)
# This is the point we want to predict
X0 <- c(0.80,  0,  8.24, 0.6, 5.00, 80.0, 4.5,   4, 307, 21, 385, 13)
####################################
# Using my_knn and FNN:knn.reg to predict point X0
# Using the same number of neighbors, it should be similar (k=1)
my_knn_R(X, X0, y)
#[1] 17.5
#the c++ code of knn (k=1) with sourceCpp "k1.cpp"
Rcpp::sourceCpp('k1.cpp')#different computer the position different
my_knn_C(X, X0, y)
#[1] 17.5
#FNN:knn.reg()
install.packages("FNN")
library(FNN)
FNN::knn.reg(X, matrix(X0, nrow = 1), y, k=1)
#the predictions results are the same.
#FNN::knn.reg(X, matrix(X0, nrow = 1), y, k=1)
#Prediction:
#  [1] 17.5
#compare the time used in these 3 ways in R ,c++ and FNN::knn.reg()
library(microbenchmark)
microbenchmark(my_knn_R(X, X0, y))
microbenchmark(my_knn_C(X, X0, y))
microbenchmark(FNN::knn.reg(X, matrix(X0, nrow = 1), y, k=1))
microbenchmark(my_knn_R(X, X0, y),my_knn_C(X, X0, y),FNN::knn.reg(X, matrix(X0, nrow = 1), y, k=1))

#microbenchmark(my_knn_R(X, X0, y),my_knn_C(X, X0, y),FNN::knn.reg(X, matrix(X0, nrow = 1), y, k=1))
#Unit: microseconds
#expr                                                 min        lq       mean    median         uq       max neval
#my_knn_R(X, X0, y)                               7196.979 7702.4815 9729.33851 8401.6755 10539.5770 22144.863   100
#my_knn_C(X, X0, y)                                 14.183   17.4245   39.77997   33.2280    53.4885   166.542   100
#FNN::knn.reg(X, matrix(X0, nrow = 1), y, k = 1)   592.016  635.5760  842.15738  837.5745   912.5390  2259.465   100
#the result is that c++ version is fastest of the three,



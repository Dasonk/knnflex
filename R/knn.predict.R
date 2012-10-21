#'KNN prediction routine using pre-calculated distances
#'
#'K-Nearest Neighbor prediction method which uses the distances calculated by
#'\code{\link{knn.dist}}.
#'
#'Predictions are calculated for each test case by aggregating the responses of
#'the k-nearest neighbors among the training cases. \code{k} may be specified
#'to be any positive integer less than the number of training cases, but is
#'generally between 1 and 10.
#'
#'The indexes for the training and test cases are in reference to the order of
#'the entire data set as it was passed to \code{\link{knn.dist}}.
#'
#'Only responses for the training cases are used.  The responses provided in y
#'may be those for the entire data set (test and training cases), or just for
#'the training cases.
#'
#'The aggregation may be any named function.  By default, classification
#'(factored responses) will use the "majority" class function and non-factored
#'responses will use "mean".  Other options to consider include "min", "max"
#'and "median".
#'
#'The ties are handled using the \code{\link{rank}} function.  Further
#'information may be found by examining the \code{ties.method} there.
#'
#'@param train indexes which specify the rows of \emph{x} provided to
#'\code{\link{knn.dist}} to be used in making the predictions
#'@param test indexes which specify the rows of \emph{x} provided to
#'\code{\link{knn.dist}} to make predictions for
#'@param y responses, see details below
#'@param dist.matrix the output from a call to \code{\link{knn.dist}}
#'@param k the number of nearest neighbors to consider
#'@param agg.meth method to combine responses of the nearest neighbors,
#'defaults to "majority" for classification and "mean" for continuous responses
#'@param ties.meth method to handle ties for the kth neighbor, the default is
#'"min" which uses all ties, alternatives include "max" which uses none if
#'there are ties for the k-th nearest neighbor, "random" which selects among
#'the ties randomly and "first" which uses the ties in their order in the data
#'@return a vector of predictions whose length is the number of test cases.
#'@note For the traditional scenario, classification using the Euclidean
#'distance on a fixed set of training cases and a fixed set of test cases, the
#'method \code{\link[class]{knn}} is ideal. The functions
#'\code{\link{knn.dist}} and \code{\link{knn.predict}} are intend to be used
#'when something beyond the traditional case is desired.  For example,
#'prediction on a continuous y (non-classification), cross-validation for the
#'selection of k, or the use of an alternate distance method are well handled.
#'@author Atina Dunlap Brooks
#'@seealso \code{\link{knn.dist}}, \code{\link{dist}}, \code{\link[class]{knn}}
#'@keywords methods
#'@examples
#'
#'# a quick classification example
#'x1 <- c(rnorm(20,mean=1),rnorm(20,mean=5))
#'x2 <- c(rnorm(20,mean=5),rnorm(20,mean=1))
#'x  <- cbind(x1,x2)
#'y <- c(rep(1,20),rep(0,20))
#'train <- sample(1:40,30)
#'# plot the training cases
#'plot(x1[train],x2[train],col=y[train]+1,xlab="x1",ylab="x2")
#'# predict the other cases
#'test <- (1:40)[-train]
#'kdist <- knn.dist(x)
#'preds <- knn.predict(train,test,y,kdist,k=3,agg.meth="majority")
#'# add the predictions to the plot
#'points(x1[test],x2[test],col=as.integer(preds)+1,pch="+")
#'# display the confusion matrix
#'table(y[test],preds)
#'
#'
#'# the iris example used by knn(class)
#'library(class)
#'data(iris3)
#'train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
#'test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
#'cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#'# how to get predictions from knn(class)
#'pred<-knn(train, test, cl, k = 3)
#'# display the confusion matrix
#'table(pred,cl)
#'
#'# how to get predictions with knn.dist and knn.predict
#'x <- rbind(train,test)
#'kdist <- knn.dist(x)
#'pred <- knn.predict(1:75, 76:150, cl, kdist, k=3)
#'# display the confusion matrix
#'table(pred,cl)
#'
#'# note any small differences are a result of both methods
#'# breaking ties in majority class randomly
#'
#'
#'# 5-fold cross-validation to select k for above example
#'fold <- sample(1:5,75,replace=TRUE)
#'cvpred <- matrix(NA,nrow=75,ncol=10)
#'for (k in 1:10)
#'  for (i in 1:5)
#'    cvpred[which(fold==i),k] <- knn.predict(train=which(fold!=i),test=which(fold==i),cl,kdist,k=k)
#'# display misclassification rates for k=1:10
#'apply(cvpred,2,function(x) sum(cl!=x))
#'
`knn.predict` <-
function(train, test, y, dist.matrix, k=1,
    agg.meth=if (is.factor(y)) "majority" else "mean",
    ties.meth="min") {

#number of predictions to make
n<-length(test)

#sort the indexes for the training and test sets
if (is.unsorted(train)) train<-sort(train)
if (is.unsorted(test)) test<-sort(test)

#only need the rows for the test data and columns
#for the training data
d<-dist.matrix[test,train]

#only need the responses for the training data
if (length(y)>length(train)) y<-y[train]

#calculate closest neighbors and
#return aggregate response for the k closest neighbors
if (n==1) {
  d <- rank(d, ties.method = ties.meth)
  x <- apply(data.frame(y[d <= k]), 2, agg.meth)
  names(x) <- test
  return(x)
  }
else {
  d<-t(apply(d,1,function(x) rank(x,ties.method=ties.meth)))
  apply(d,1,function(x) apply(data.frame(y[x<=k]),2,agg.meth))
  }
}

#'Calculates the distances to be used for KNN predictions
#'
#'The distances to be used for K-Nearest Neighbor (KNN) predictions are
#'calculated and returned as a symmetric matrix.  Distances are calculated by
#'\code{\link{dist}}.
#'
#'This function calculates the distances to be used by
#'\code{\link{knn.predict}}. Distances are calculated between all cases. In the
#'traditional scenario (a fixed n training cases, m disjoint test cases) this
#'method will calculate more distances than required for prediction. For
#'example, distances between training cases are not needed, but are calculated
#'anyway. However, performance testing has shown that in most cases it is still
#'faster to simply calculate all distances, even when many will not be used.
#'
#'The advantage to calculating distances in a separate step prior to
#'prediction, is that these calculations only need to be performed once. So,
#'for example, cross-validation to select k can be performed on many values of
#'k, with different cross-validation splits, all using a single run of
#'knn.dist.
#'
#'The default method for calculating distances is the "euclidean" distance,
#'which is the method used by the \code{\link[class]{knn}} function from the
#'\code{class} package. Alternative methods may be used here.  Any method valid
#'for the the function \code{\link{dist}} is valid here.  The parameter p may
#'be specified with the Minkowski distance to use the \emph{p} norm as the
#'distance method.
#'
#'@param x the entire dataset, the rows (cases) to be used for training and
#'testing.
#'@param dist.meth the distance to be used in calculating the neighbors.  Any
#'method valid in function \code{\link{dist}} is valid.
#'@param p the power of the Minkowski distance.
#'@return a square symmetric matrix whose dimensions are the number of rows in
#'the original data.  The diagonal contains zeros, the off diagonal entries
#'will be >= 0.
#'@note For the traditional scenario, classification using the Euclidean
#'distance on a fixed set of training cases and a fixed set of test cases, the
#'method \code{\link[class]{knn}} is ideal. The functions
#'\code{\link{knn.dist}} and \code{\link{knn.predict}} are intend to be used
#'when something beyond the traditional case is desired.  For example,
#'prediction on a continuous y (non-classification), cross-validation for the
#'selection of k, or the use of an alternate distance method are all possible
#'with this package.
#'@author Atina Dunlap Brooks
#'@seealso \code{\link{knn.predict}}, \code{\link{dist}},
#'\code{\link[class]{knn}}
#'@keywords methods
#'@export
#'@examples
#'
#'#a quick classification example
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
`knn.dist` <- function(x, dist.meth="euclidean", p=2) {
    #create a distance matrix using all values in the data
    d<-as.matrix(dist(x,dist.meth,p))
    #fix for some small high persision errors
    round(d,digits=15)
}


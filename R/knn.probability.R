#'KNN prediction probability routine using pre-calculated distances
#'
#'K-Nearest Neighbor prediction probability method which uses the distances
#'calculated by \code{\link{knn.dist}}.  For predictions (not probabilities)
#'see \code{\link{knn.predict}}.
#'
#'Prediction probabilities are calculated for each test case by aggregating the
#'responses of the k-nearest neighbors among the training cases and using the
#'\code{\link{classprob}}. \code{k} may be specified to be any positive integer
#'less than the number of training cases, but is generally between 1 and 10.
#'
#'The indexes for the training and test cases are in reference to the order of
#'the entire data set as it was passed to \code{\link{knn.dist}}.
#'
#'Only responses for the training cases are used.  The responses provided in y
#'may be those for the entire data set (test and training cases), or just for
#'the training cases.
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
#'@param ties.meth method to handle ties for the kth neighbor, the default is
#'"min" which uses all ties, alternatives include "max" which uses none if
#'there are ties for the k-th nearest neighbor, "random" which selects among
#'the ties randomly and "first" which uses the ties in their order in the data
#'@return a matirx of prediction probabilities whose number of columns is the
#'number of test cases and the number of rows is the number of levels in the
#'responses.
#'@note For the traditional scenario, classification using the Euclidean
#'distance on a fixed set of training cases and a fixed set of test cases, the
#'method \code{\link[class]{knn}} is ideal. The functions
#'\code{\link{knn.dist}} and \code{\link{knn.predict}} are intend to be used
#'when something beyond the traditional case is desired.  For example,
#'prediction on a continuous y (non-classification), cross-validation for the
#'selection of k, or the use of an alternate distance method are well handled.
#'@author Atina Dunlap Brooks
#'@seealso \code{\link{knn.dist}}, \code{\link{knn.predict}},
#'\code{\link[class]{knn}}
#'@keywords methods
#'@export
#'@examples
#'
#'# the iris example used by knn(class)
#'library(class)
#'data(iris3)
#'train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
#'test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
#'cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#'# how to get predictions from knn(class)
#'pred <- knn(train, test, cl, k = 3, prob=TRUE)
#'# display the confusion matrix
#'table(pred,cl)
#'# view probabilities (only the highest probability is returned)
#'attr(pred,"prob")
#'
#'# how to get predictions with knn.dist and knn.predict
#'x <- rbind(train,test)
#'kdist <- knn.dist(x)
#'pred <- knn.predict(1:75, 76:150, cl, kdist, k=3)
#'# display the confusion matrix
#'table(pred,cl)
#'# view probabilities (all class probabilities are returned)
#'knn.probability(1:75, 76:150, cl, kdist, k=3)
#'
#'# to compare probabilites, rounding done for display purposes
#'p1 <- knn(train, test, cl, k = 3, prob=TRUE)
#'p2 <- round(knn.probability(1:75, 76:150, cl, kdist, k=3), digits=2)
#'table( round(attr(p1,"prob"), digits=2), apply(p2,2,max) )
#'
#'# note any small differences in predictions are a result of
#'# both methods breaking ties in majority class randomly
#'
#'
`knn.probability` <- function(train, test, y, dist.matrix, k=1, ties.meth="min") {
    
    #number of predictions to make
    n<-length(test)
    
    #sort the indexes for the training and test sets
    if (is.unsorted(train)) train<-sort(train)
    if (is.unsorted(test)) test<-sort(test)
    
    #only need the rows for the test data and columns
    #for the training data
    d<-dist.matrix[test,train]
    
    #ensure y is a factor
    y<-as.factor(y)
    
    #only need the responses for the training data
    if (length(y)>length(train)) y<-y[train]
    
    #calculate closest neighbors and
    #return aggregate response for the k closest neighbors
    if (n==1) {
        d<-rank(d, ties.method = ties.meth)
        x<-classprob(y[d <= k])
        x<-data.frame(x)
        names(x)<-test
        row.names(x)<-levels(y)
        return(x)
    } else {
        d<-t(apply(d,1,function(x) rank(x,ties.method=ties.meth)))
        x<-apply(d,1,function(x) classprob(y[x<=k]))
        row.names(x)<-levels(y)
        return(x)
    }
}

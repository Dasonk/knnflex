#'Determines majority class
#'
#'A function which determines the majority class of a vector (treated as
#'factor).
#'
#'This function treats the input vector as a factor and determines which level
#'(class) of the factor is present most often.  If two or more levels tie for
#'majority then a random selection is made among the ties.
#'
#'@param x a one dimensional vector
#'@return The factor level which occurs most often in x.
#'@author Atina Dunlap Brooks
#'@seealso \code{\link{factor}}
#'@keywords utilities
#'@export
#'@examples
#'
#'x <- sample( c("a","b","c","d","e"), 10, replace=TRUE )
#'majority(x)
#'
`majority` <- function(x){
    x <- as.factor(x)
    n <- nlevels(x)
    votes <- rep(0,n)
    for (i in 1:length(x)) votes[as.integer(x[i])] <- votes[as.integer(x[i])]+1
    levels(x)[order(votes,decreasing=TRUE,sample(1:n,n))[1]]
}


\name{classprob}
\alias{classprob}
\title{Determines the prevalence of each class}
\usage{
  classprob(x)
}
\arguments{
  \item{x}{a one dimensional vector}
}
\value{
  A vector whose length is equal to the number of levels in
  the input. The order is numerically or alphabetically
  increasing.  Note the factors may have levels which are
  not present in the vector, see examples for details.
}
\description{
  A function which determines the prevalence or probability
  for each class of a vector (treated as factor).
}
\details{
  This function treats the input vector as a factor and
  determines the probability for each level (class) of the
  factor.  The order of the returned probabilities is the
  order of the \code{\link{levels}} command, which defaults
  to numeric or alphabetic order.
}
\examples{
#calculate probbilities
x <- sample( c("a","b","c","d","e"), 10, replace=TRUE )
classprob(x)
#label the probabilities
levels(as.factor(x))

#to see levels which aren't represnted in the vector
x<-as.factor(c('a','a','a','b','b','c'))
levels(x)
#now remove the 'c'
x<-x[1:5]
#but 'c' is still a level
levels(x)
#and the probability is calculated for it
classprob(x)
}
\author{
  Atina Dunlap Brooks
}
\seealso{
  \code{\link{majority}} \code{\link{factor}}
}
\keyword{utilities}


\name{majority}
\alias{majority}
\title{Determines majority class}
\usage{
  majority(x)
}
\arguments{
  \item{x}{a one dimensional vector}
}
\value{
  The factor level which occurs most often in x.
}
\description{
  A function which determines the majority class of a
  vector (treated as factor).
}
\details{
  This function treats the input vector as a factor and
  determines which level (class) of the factor is present
  most often.  If two or more levels tie for majority then
  a random selection is made among the ties.
}
\examples{
x <- sample( c("a","b","c","d","e"), 10, replace=TRUE )
majority(x)
}
\author{
  Atina Dunlap Brooks
}
\seealso{
  \code{\link{factor}}
}
\keyword{utilities}


\name{stiff}
\alias{stiff}
\docType{data}
\title{
The Board Stiffness Dataset
}
\description{
 Four measures of stiffness of 30 boards are available. The first
     measure of stiffness is obtained by sending a shock wave down the
     board, the second measure is obtained by vibrating the board, and
     remaining are obtained from static tests.
}
\usage{data(stiff)}
\format{
   A data frame with 30 observations on the following 4 variables.
\describe{
    \item{\code{x1}}{first measure of stiffness is obtained by sending a shock
          wave down the board}
    \item{\code{x2}}{second measure is obtained by vibrating the board}
    \item{\code{x3}}{third measure is obtained by a static test}
    \item{\code{x4}}{fourth measure is obtained by a static test}
    }
  }
 
\references{
 Johnson, R.A., and Wichern, D.W. (1982-2007). Applied Multivariate
     Statistical Analysis, 6e. Pearson Education.
  Tattar, et al. (2016). A Course in Statistics with R. J. Wiley. 
}
\examples{
data(stiff)
summary(stiff)
}
\keyword{datasets}

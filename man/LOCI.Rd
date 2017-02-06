\name{LOCI}
\alias{LOCI}
\title{
Local Correlation Integral
}
\description{
We provide an R implementation of the Local Correlation Integral method for detecting outliers as developed by Breunig, et al. (2000), and we follow its description given in Papadimitriou, et al. (2002). 
}
\usage{
LOCI(data, alpha)
}

\arguments{
  \item{data}{
Any R data.frame which consists of numeric values only
}
  \item{alpha}{
a number in the unit interval for the fractional circle search
}
}
\details{
A simple implementation is provided here. The core function is the distance function. For each observation, a search is made for nearest neighbors within r distance of it, and then for each of these neighbors, we find the number of observations in the fractional circle. Calculations based on multi-granularity deviation factor, MDEF, help in determining the outlier. 
}
\references{
M.M. Breunig, H.P. Kriegel, R.T. Ng, and J. Sander. Lof: Identifying density-based
local outliers. In Proc. SIGMOD Conf., pages 93-104, 2000.
Papadimitriou, S., Kitagawa, H., Gibbons, P.B. and Faloutsos, C., 2003, March. Loci: Fast outlier detection using the local correlation integral. In Data Engineering, 2003. Proceedings. 19th International Conference on (pp. 315-326). IEEE.
}
\author{
Siddharth Jain and Prabhanjan Tattar
}

\examples{
data(stiff)
OM <- LOCI(stiff,0.5)
OM
}
\keyword{LOCI}
\keyword{multi granularity deviation factor}
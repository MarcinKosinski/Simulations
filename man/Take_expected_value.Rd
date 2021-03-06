% Generated by roxygen2 (4.0.1): do not edit by hand
\name{Take_expected_value}
\alias{Take_expected_value}
\title{Calculates the expected value - \eqn{(H_{\lambda}(S))}}
\usage{
Take_expected_value(interval_end = 1, points = 100,
  number_of_trajectories = 1000)
}
\arguments{
\item{interval_end}{Describes the end of interval on which expression(H_{\lambda}(S))
should be calculated. Default is 1.}

\item{points}{Tells on how many points should the maximum be estimated. Points are
spread evenly.}

\item{number_of_trajectories}{Determines the number of trajectory realizations on which
the expected values is estimated by taking a mean of those realizations.}
}
\description{
This function calculates the expected value of supremum for Wiener process,
which is also known as \deqn{H_{\lambda}(S))=\mathbb{E}\exp(\sup(\sqrt{2}W_{t}-t))}
}
\details{
Marcin ma mala palke
}
\examples{
Take_expected_value(10,100,1000)

# Parallel computing
library(parallel)
cl <- makeCluster(detectCores())
clusterEvalQ(cl, library(Rpickands))
system.time({
  m <- parLapply(cl, 1:100,Take_expected_value,
                copy_number=100000,points=1000)
})
stopCluster(cl)

#####HUGE SIMULATION#####
m1 <- vector("list", 100)
library(parallel)
cl <- makeCluster(detectCores())
clusterEvalQ(cl, library(Rpickands))
system.time({
   m1 <- parLapply(cl, 1:100,Take_expected_value, copy_number=100000,points=10000)
})
stopCluster(cl)
}
\author{
Marcin Kosi?ski, \email{m.p.kosinski@gmail.com}
}
\seealso{
Other Rpickands: \code{\link{Rpickands-package}}
}


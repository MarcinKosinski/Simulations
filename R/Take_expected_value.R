##    Rpickands package for R
##    Copyright (C) 2014 Kosinski Brothers
##



#' @title Calculates the expected value - \eqn{(H_{\lambda}(S))}
#'
#' @description
#' This function calculates the expected value of supremum for Wiener process, 
#' which is also known as \deqn{H_{\lambda}(S))=\mathbb{E}\exp(\sup(\sqrt{2}W_{t}-t))}
#' 
#' 
#' @param interval_end Describes the end of interval on which expression(H_{\lambda}(S))
#' should be calculated. Default is 1.
#' @param points Tells on how many points should the maximum be estimated. Points are
#' spread evenly.
#' @param copy_number Determines the number of random variable realizations on which
#' the expected values is estimated by taking a mean of those realizations.
#' 
#' @details
#' Blah blah
#' 
#' 
#' @examples
#' Take_expected_value(10,100,1000)
#' 
#' # Parallel computing
#' library(parallel)
#' cl <- makeCluster(detectCores())
#' clusterEvalQ(cl, library(Rpickands))
#' system.time({
#'   m <- parLapply(cl, 1:100,Take_expected_value, 
#'                 copy_number=100000,points=1000)
#' })
#' stopCluster(cl)
#' 
#' #####HUGE SIMULATION#####
#' m1 <- vector("list", 100)
#' library(parallel)
#' cl <- makeCluster(detectCores())
#' clusterEvalQ(cl, library(Rpickands))
#' system.time({
#'    m1 <- parLapply(cl, 1:100,Take_expected_value, copy_number=100000,points=10000)
#' })
#' stopCluster(cl)
#' 
#' @author Marcin Kosiñski, \email{m.p.kosinski@@gmail.com}
#' 
#' @family Rpickands
#' @export
#' @rdname Take_expected_value
Take_expected_value <- function(interval_end=1, points=100,copy_number=1000){
   return(
      mean(
         exp(
            replicate(
               n=copy_number,
               expr=Max_from_Wiener_on_interval(interval_end,points)
               )
         )
      )
   ) # This function just replicates max_from_... function, then put values
      # to exp function, and calculates mean of all replications.
   
}


# this function shall not be exported
.Max_from_Wiener_on_interval <- function(interval_end=1,points=100){
   Delta <- interval_end/points
   # time increment

   # Wiener process
   W <- c( 0, cumsum( sqrt(Delta) * rnorm( points+1 ) ))
   # time moments
   time <- seq( 0, interval_end, length = points+1)
   # return max of "Wiener * sqrt(2) - time moment"
   return(
      max(
         sqrt(2) * W - time
         )
      )
}






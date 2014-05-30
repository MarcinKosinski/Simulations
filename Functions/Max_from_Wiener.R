Max_from_Wiener_on_interval <- function(interval_end=1,points=100){
   Delta <- interval_end/points
   # time increment
   W <- numeric(points+1)
   # initialization of the vector W approximating 
   # Wiener process
   t <- seq(0,interval_end, length=points+1)
   return(max(c(0, cumsum( sqrt(Delta) * rnorm(points+1)))))
}
Max_from_Wiener_on_interval(1,100)


Take_expected_value <- function(copy_number=1000, interval_end=1, points=100){
return(
   mean(
      exp(
         replicate(copy_number,Max_from_Wiener_on_interval(interval_end,points))
         )
      )
   )

}

Take_expected_value(10000,1,1000)

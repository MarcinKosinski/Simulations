Max_from_Wiener_on_interval <- function(interval_end=1,points=100){
   Delta <- interval_end/points
   # time increment
   W <- numeric(points+1)
   # initialization of the vector W approximating 
   # Wiener process
   t <- seq(0,interval_end, length=points+1)
   return(max(c(0, cumsum( sqrt(Delta) * rnorm(points+1)))))
}

Plot_Wiener_on_interval <- function(interval_end=1,points=100,...){
      Delta <- interval_end/points

      W <- numeric(points+1)


      t <- seq(0,interval_end, length=points+1)
      plot(c(0, cumsum( sqrt(Delta) * rnorm(points+1))), type="l",...)
}

Plot_exp_from_Wiener_on_interval <- function(interval_end=1,points=100,...){
   Delta <- interval_end/points
   
   W <- numeric(points+1)
   
   
   t <- seq(0,interval_end, length=points+1)
   plot(exp(c(0, cumsum( sqrt(Delta) * rnorm(points+1)))), type="l",...)
}

Max_from_Wiener_on_interval(1,100)
Plot_Wiener_on_interval(100,1000)
par(mfrow=c(3,3))
Plot_exp_from_Wiener_on_interval(100,1000, ylab="exp(max(Wiener))")
exp(Max_from_Wiener_on_interval(100,100))

Take_expected_value <- function(interval_end=1, points=100,copy_number=1000){
return(
   mean(
      exp(
         replicate(copy_number,Max_from_Wiener_on_interval(interval_end,points))
         )
      )
   )

}

Take_expected_value(10000,1,1000)

system.time({
   Take_expected_value(100000,1,1000)
})
#użytkownik     system   upłynęło 
#13.17       0.00      13.35

cl <- makeCluster(detectCores())
clusterEvalQ(cl, # sending Ma_from's definition to all nodes
             Max_from_Wiener_on_interval <- function(interval_end=1,points=100){
                                             Delta <- interval_end/points
                                             W <- numeric(points+1)
                                             t <- seq(0,interval_end, 
                                                      length=points+1)
                        return(max(c(0, cumsum( sqrt(Delta) * rnorm(points+1)))))
})

system.time({
m <- parLapply(cl, 1:100,Take_expected_value, copy_number=100000,points=1000)
})
#użytkownik     system   upłynęło x=1
#0.00       0.00       4.04


#użytkownik     system   upłynęło x=1:100
#0.02       0.04     394.95 

system.time({
   s <- lapply(1:100,Take_expected_value, copy_number=1000,points=100)
})
unlist(s)



k <- numeric(100)
for (i in 1:100){
k[i] <- Take_expected_value(1000,i,100)
}

plot(1:100,k-unlist(s), ylim=c(-500000,500000))

stopCluster(cl)


Max_from_Wiener_on_interval <- function(interval_end=1,points=100){
   Delta <- interval_end/points
   # time increment
   W <- numeric(points+1)
   # initialization of the vector W approximating 
   # Wiener process
   t <- seq(0,interval_end, length=points+1)
   return(max(c(0, cumsum( sqrt(Delta) * rnorm(points+1)))-c(0, Delta)))
}

Plot_Wiener_on_interval <- function(interval_end=1,points=100,...){
      Delta <- interval_end/points

      W <- numeric(points+1)


      t <- seq(0,interval_end, length=points+1)
      plot(c(0, cumsum( sqrt(Delta) * rnorm(points+1)))-c(0,Delta), type="l",...)
}

Plot_exp_from_Wiener_on_interval <- function(interval_end=1,points=100,...){
   Delta <- interval_end/points
   
   W <- numeric(points+1)
   
   
   t <- seq(0,interval_end, length=points+1)
   plot(exp(c(0, cumsum( sqrt(Delta) * rnorm(points+1)))-c(0,Delta)), type="l",...)
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

Take_expected_value(10,100,100000)

system.time({
   Take_expected_value(1,1000,100000)
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
                        return(max(c(0, cumsum( sqrt(Delta) * rnorm(points+1)))
                                   -c(0, Delta)))
})

system.time({
m <- parLapply(cl, 1:100,Take_expected_value, copy_number=100000,points=1000)
})
#użytkownik     system   upłynęło x=1
#0.00       0.00       4.04


#użytkownik     system   upłynęło x=1:100
#0.02       0.04     394.95 

system.time({
   s <- parLapply(cl,1:100,Take_expected_value, copy_number=10000,points=1000)
})
unlist(s)
#użytkownik     system   upłynęło 
#0.01       0.00      37.50 

stopCluster(cl)

# saving into .txt file
first_100_H_lamba_S <- as.character(unlist(s))
first_100_H_lamba_S <- stri_flatten(first_100_H_lamba_S, collapse= "\n ")
write.table(first_100_H_lamba_S,
            "first_100_H_lamba_S.txt", sep="\t", quote=FALSE )

par(mfrow=c(1,1))
plot(1:100,unlist(s)/1:100, main="First 100 H_lambda_s for Wiener",ylab="some values are above", ylim=c(0, 1e+12))

#####HUGE SIMULATION#####
m1 <- vector("list", 100)
system.time({
   m1 <- parLapply(cl, 1:100,Take_expected_value, copy_number=100000,points=10000)
})
# for 10^6
# Timing stopped at: 0.14 0.1 9755.45 
# Does not compute on my computer !

# for 10^5
#użytkownik     system   upłynęło 
#0.04       0.01    3193.76

stopCluster(cl)


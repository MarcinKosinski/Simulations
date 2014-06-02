?rwiener
## calculate density for reactiontime 1.45 and some parameters
dwiener(1.45, 2,0.3,0.5,0)

## calculate CDF for reactiontime 1.45 and some parameters
pwiener(1.45, 2,0.3,0.5,0)

## calculate quantile for CDF value of 0.5 and some parameters
qwiener(0.5, 2,0.3,0.5,0)

## generate one random value
rwiener(1, 2,0.3,0.5,0)

wiener_plot(rwiener(100, 2,0.3,0.5,0))


###############################################
################ Withouth any built in packages
###############################################

N <- 100
# number of end-points of the grid including T
T <- 1
# length of the interval [0, T] in time units
Delta <- T/N
# time increment
W <- numeric(N+1)
# initialization of the vector W approximating 
# Wiener process
t <- seq(0,T, length=N+1)
W <- c(0, cumsum( sqrt(Delta) * rnorm(N)))
plot( t, W, type="l", main="Wiener process", ylim=c(-1,1))


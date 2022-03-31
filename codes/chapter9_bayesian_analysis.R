source("jhp.R")

MarkovChainSampler <- function(mcmc=1000, burnin=1000){
  iter <- mcmc + burnin
  storage <- rep(NA, iter)
  # The initial state is set as State 1
  storage[1] <- 1
  for (g in 2:iter) {
    u <- runif(1)
    if(storage[g-1] == 1) {
      ## state t-1 = 1
      storage[g] <- ifelse(u<0.65, 1, 2)
    } else {
      ## state t-1 = 2
      storage[g] <- ifelse(u<0.25, 1, 2)
    }
    if(g>burnin & g%%1000 == 0){
      cat("iteration at ", g,
          table(storage[(burnin+1):g])/(g-(burnin+1)), "\n")
    }
  }
  return(storage[(burnin+1):iter])
}

set.seed(1975)
out <- MarkovChainSampler()
table(out)/5000

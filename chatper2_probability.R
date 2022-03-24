birthday_problem_test <- function(k) {
  ## k: 그룹안의 사람 수
  n <- 365
  numerator <- factorial(365)
  denominator <- n^k*(factorial(365-k))
  out <- 1 - numerator/denominator
  return(out)
}

birthday_problem <- function(k) {
  n <- 365
  log.numerator <- lgamma(365+1)
  log.denominator <- log(n^k) + lgamma(365-k+1)
  out <- 1 - exp(log.numerator - log.denominator)
  return(out)
}

start_time <- Sys.time()

birthday_problem(k=20)
Sys.time() - start_time

birthday.simulator <- function(n, total.sim = 1000000){
  probs <- c(rep(1/365.2425, 365), (97/400)/365.2425)
  anyduplicated <- function(ignored)
    any(duplicated(sample(1:366, n, prob=probs, replace=TRUE)))
  out <- sum(sapply(seq(total.sim), anyduplicated)) / total.sim
  return(out)
}

birthday.simulator(20)

birthday.simulator2 <- function(n, total.sim = 1000000) {
  probs <- c(rep(1/365, 365))
  anyduplicated <- function(ignored)
    any(duplicated(sample(1:365, n, prob=probs, replace=TRUE)))
  out <- sum(sapply(seq(total.sim), anyduplicated)) / total.sim
  return(out)
}

birthday.simulator2(20)

#hash
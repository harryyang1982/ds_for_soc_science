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

### 3.1 균등분포를 이용한 MH

BetaMH <- function(f.target, # 목포 분포의 밀도 
                   f.prop, # 제안 분포의 밀도
                   r.prop, # 제안분포의 표본
                   x0, #체인 시작값
                   mcmc = 1000, #MCMC 횟수
                   burnin = 1000) { # 처음 1,000번은 버리기
  iter <- mcmc + burnin
  mcmc.store <- rep(NA, mcmc)
  accepts <- 0
  x <- c(x0, rep(NA, iter-1))
  
  for(g in 2:iter) {
    candidate <- r.prop(x[g-1])
    
    ## 수용률 계산
    numerator <- f.target(candidate)*f.prop(x[g-1], candidate)
    denominator <- f.target(x[g-1])*f.prop(candidate, x[g-1])
    alpha <- min(1, numerator/denominator)
    
    ## 수용 결정
    if(runif(1) < alpha){
      x[g] <- candidate
      accepts <- accepts + 1
    } else {
      x[g] <- x[g-1]
    }
  if(g>burnin){
    mcmc.store[g-burnin] <- x[g]
  }
  if(g%%500 == 0)
    cat("acceptance rate = ", accepts/g, "\n")
  }
  return(mcmc.store)
}

set.seed(1973)
a <- 3; b <- 4
f.target <- function(x) dbeta(x, a, b)
r.prop <- function(x) runif(1, 0, 1)
f.prop <- function(x, y) 1
x0 <- runif(1, 0, 1)
beta.mh.post <- BetaMH(f.target, f.prop, r.prop, x0=runif(1, 0, 1))

mcmc <- 1000
par(mar=c(3, 3, 2, 1), mgp=c(2, .7, 0), tck=.02)
par(mfrow=c(1, 2), mar=c(2, 2, 1, 1))

hist(beta.mh.post, breaks=50, col="blue", cex.main=0.5,
     main="균등분포를 이용한 MH 추출", freq=FALSE)
curve(dbeta(x, a, b), col="sienna", lwd=2, add=TRUE)

hist(rbeta(mcmc, a, b), breaks=50, col="grey", cex.main=0.5,
     main="IID 추출", freq=FALSE)
curve(dbeta(x, a, b), col="sienna", lwd=2, add=TRUE)

library(bayesplot)
library(rstan)
posterior <- rstan:::as.data.frame.stanfit(data.frame("beta"=beta.mh.post))
color_scheme_set("red")
mcmc_trace(posterior)

iid <- rstan:::as.data.frame.stanfit(data.frame("beta"=rbeta(mcmc, a, b)))
mcmc_trace(iid)

mcmc_areas(posterior, prob = 0.95, point_est = "mean")
mcmc_intervals_data(posterior)

mcmc_intervals(posterior)

### 3.2 독립커널을 이용한 MH

BetaIndMH <- function(mcmc=1000, burnin=1000, a=3, b=4){ 
  iter <- mcmc + burnin
  mcmc.store <- rep(NA, mcmc)
  accepts <- 0
  x <- runif(1)
  
  for (g in 1:iter){
    u2 <- runif(1)
    f2 <- dbeta(u2, a, b)
    f1 <- dbeta(x, a, b)
    alpha <- min(f2/f1, 1)
    
    ## 수용확률 계산
    if (runif(1) < alpha){
      x <- u2
      if (g > burnin){ 
        accepts <- accepts + 1
      }
    }  
    if (g > burnin){ 
      mcmc.store[g-burnin] <- x
    }
  }
  cat("acceptance rate = ", accepts/mcmc, "\n")
  return(mcmc.store)
}


## ----mh2, fig.cap="독립커널을 이용한 메트로폴리스 해이스팅스 방법", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
set.seed(1973)
a<-3; b<-4; 
mcmc = 1000
burnin=1000
beta.mh.ind <- BetaIndMH(mcmc, burnin)

par(mfrow=c(1, 2), mar=c(2, 2, 1 ,1))
hist(beta.mh.ind, breaks=50, col="blue", cex.main=0.5,
     main="독립커널 MH 추출", freq=FALSE)
curve(dbeta(x, a, b), col="sienna", lwd=2, add=TRUE)

hist(rbeta(mcmc, a, b), breaks=50, col="grey", cex.main=0.5,
     main="IID 추출", freq=FALSE)
curve(dbeta(x, a, b), col="sienna", lwd=2, add=TRUE)

posterior <- rstan:::as.data.frame.stanfit(data.frame("beta" = beta.mh.ind))
color_scheme_set("red")
mcmc_trace(posterior)

mcmc_areas(posterior, prob=0.95, point_est = "mean")
mcmc_intervals(posterior)

### 3.3 임의 보행 커널을 이용한 MH

poisson.log.post <- function(beta, ...) {
  n <- nrow(X)
  k <- ncol(X)
  eta <- X%*%matrix(beta, k, 1)
  mu <- exp(eta)
  log.like <- sum(dpois(y, mu, log=TRUE))
  log.prior <- dmvnorm(beta, b0, B0, log=TRUE)
  return(log.like + log.prior)
}

poissonMH <- function(y, X,
                      mcmc=1000, burnin=1000, verbose=0,
                      beta.hat, V.hat, tune=1){
  n <- length(y)
  k <- ncol(X)
  mcmc.store <- matrix(NA, mcmc, k)
  tot.iter <- mcmc+ burnin
  accepts <- 0
  beta <- beta.hat
  ## Metropolis-Hastings 샘플링
  for (g in 1:tot.iter){
    ## candidate 추출
    beta.can <- beta + rmvnorm(1, matrix(0, k, 1), tune*V.hat)
    ## 수용률 계산
    log.ratio <- poisson.log.post(beta.can) - poisson.log.post(beta)
    alpha <- min(exp(log.ratio), 1)
    ## 수용여부 결정
    if (runif(1) < alpha){
      beta <- beta.can
      if (g > burnin) accepts <- accepts + 1
    }
    ## 저장
    if (g > burnin) {
      mcmc.store[g-burnin, ] <- beta
    }
    ## echo some results
    if (verbose!=0&g%%verbose==0){
      cat("iteration ", g, "beta ", beta, "\n")
    }
  }
  cat("acceptance rate = ", accepts/mcmc, "\n")
  return(mcmc.store)
}

require(mvtnorm)
set.seed(1973)

## 가상의 자료 생성
X <- cbind(1, rnorm(100), runif(100))
true.beta <- c(1, -1, 2)
y <- rpois(100, exp(X%*%true.beta))
mle <- glm(y~X-1, family=poisson())
## MCMC 입력 준비물
V.hat <- vcov(mle)
beta.hat <- coef(mle)
b0 <- rep(0, 3)
B0 <- diag(1000, 3)

## MH로 모형 추정
poisson.rw.post <- poissonMH(y, X,
                             mcmc=5000, burnin=1000, verbose=1000,
                             beta.hat=beta.hat, V.hat=V.hat, tune=1.5)

par(mar=c(3, 3, 2, 1), mgp = c(2, .7, 0), tck=.02)
require(coda)
out <- as.mcmc(poisson.rw.post)
plot(out)

summary(out)

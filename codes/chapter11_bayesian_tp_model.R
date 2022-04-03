source("jhp.R")

# 1. 베이지안 선형 회귀분석 전환점 모형

library(MCMCpack)
set.seed(1119)
n <- 200
x1 <- runif(n)
true.beta1 <- c(0, -2)
true.beta2 <- c(0, 2)
true.Sigma <- c(1, 1)
true.s <- rep(1:2, each=n/2)

mu1 <- cbind(1, x1[true.s==1])%*%true.beta1
mu2 <- cbind(1, x1[true.s==2])%*%true.beta2

y <- as.ts(c(rnorm(n/2, mu1, sd=sqrt(true.Sigma[1])),
             rnorm(n/2, mu2, sd=sqrt(true.Sigma[2]))))
formula <- y~x1

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

b0 <- 0
B0 <- 0.1 ## B0 is a precision (i.e. the inverse of variance)
sigma.mu <- var(y)
sigma.var <- sigma.mu^2
c0 <- 4 + 2*(sigma.mu^2/sigma.var)
c0
d0 <- 2*sigma.mu*(c0/2 - 1)
d0

sigma.mu <- sd(y)
sigma.var <- var(y)
curve(dbeta(x, 1, 1), lwd=5, xlim=c(0, 1), ylim=c(0, 3),
      ylab="f(y)", xlab="y", col='firebrick4')

library(tictoc)
tic("MCMCregressChange model check")
sim0 <- MCMCregressChange(formula, m=0, b0=b0, B0=B0, mcmc=1000, burnin=1000,
                          sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood = "Chib95")
sim1 <- MCMCregressChange(formula, m=1, b0=b0, B0=B0, mcmc=1000, burnin=1000,
                          sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood = "Chib95")
sim2 <- MCMCregressChange(formula, m=2, b0=b0, B0=B0, mcmc=1000, burnin=1000,
                          sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood = "Chib95")
toc()

BayesFactor(sim0, sim1, sim2)[3]

par(mfrow=c(1, 2), mai=c(0.4, 0.6, 0.3, 0.05), cex.main=0.5)
plotState(sim1, main="전환점 1개")
plotState(sim2, main="전환점 2개")

par(mar=c(3, 3, 2, 1), mgp=c(2,.7,0), tck=.02, cex.main=0.5)
plotChangepoint(sim1, verbose=TRUE)

print(summary(sim1), digits=2)

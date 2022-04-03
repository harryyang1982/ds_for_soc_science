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
G <- 10000
tic("MCMCregressChange model check")
sim0 <- MCMCregressChange(formula, m=0, b0=b0, B0=B0, mcmc=G, burnin=1000,
                          sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood = "Chib95")
sim1 <- MCMCregressChange(formula, m=1, b0=b0, B0=B0, mcmc=G, burnin=1000,
                          sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood = "Chib95")
sim2 <- MCMCregressChange(formula, m=2, b0=b0, B0=B0, mcmc=G, burnin=1000,
                          sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood = "Chib95")
toc()

BayesFactor(sim0, sim1, sim2)[3]

par(mfrow=c(1, 2), mai=c(0.4, 0.6, 0.3, 0.05), cex.main=0.5)
plotState(sim1, main="전환점 1개")
plotState(sim2, main="전환점 2개")

par(mar=c(3, 3, 2, 1), mgp=c(2,.7,0), tck=.02, cex.main=0.5)
plotChangepoint(sim1, verbose=TRUE)

print(summary(sim1), digits=2)


# 모음 없음의 경우
complete.pool.mcmc <- MCMCregress(formula, b0=b0, B0=B0,
                                  mcmc=G, burnin=1000,
                                  sigma.mu = sigma.mu,
                                  sigma.var=sigma.var)

data <- data.frame(y, x1)
no.pool.mcmc1 <- MCMCregress(formula,
                             data=data[rep(c(T,F), each=n/2), ],
                             b0=b0, B0=B0, mcmc=G, burnin=1000,
                             sigma.mu=sigma.mu, sigma.var=sigma.var)
no.pool.mcmc2 <- MCMCregress(formula,
                             data=data[rep(c(F, T), each=n/2), ],
                             b0=b0, B0=B0, mcmc=G, burnin=1000,
                             sigma.mu=sigma.mu, sigma.var=sigma.var)

density.compare <- function(no.pool, complete.pool, partial.pool,
                            true.beta=0,
                            title="", subtitle="",
                            caption=""){
  df.dens <- data.frame(no.pool, complete.pool, partial.pool)
  colnames(df.dens) <- c("no pooling", "complete pooling", "partial pooling")
  df.dens.long <- tidyr::gather(df.dens, type, value)
  g.dens <- ggplot(df.dens.long, aes(value, fill=type, color=type)) +
    geom_density(alpha = 0.1) +
    geom_vline(xintercept = true.beta, color="red", linetype="longdash") +
    theme_jhp() + xlab("value") + ylab("density") +
    labs(title = title, subtitle = subtitle,
         caption = caption, color=NULL, fill=NULL)
  return(g.dens)
}

## beta 1 compare
beta1pre.complete <- complete.pool.mcmc[, 1]
beta1post.complete <- complete.pool.mcmc[, 1]
beta1pre.no.pool <- no.pool.mcmc1[, 1]
beta1post.no.pool <- no.pool.mcmc2[, 1]
beta1pre.partial <- sim1[, 1]
beta1post.partial <- sim[, 3]

## beta 2 compare
beta2pre.complete <- complete.pool.mcmc[, 2]
beta2post.complete <- complete.pool.mcmc[, 2]
beta2pre.no.pool <- no.pool.mcmc1[, 2]
beta2post.no.pool <- no.pool.mcmc2[, 2]
beta2pre.partial <- sim1[, 2]
beta2post.partial <- sim1[, 4]

## draw plots
complete.pool <- beta2pre.complete
no.pool <- beta2pre.no.pool
partial.pool <- beta2pre.partial
density.compare(no.pool, complete.pool, partial.pool,
                true.beta = true.beta1[2],
                subtitle = "레짐 1의 기울기 추정치 비교: 세로선이 참값")

complete.pool <- beta2post.complete
no.pool <- beta2post.no.pool
partial.pool <- beta2post.partial
density.compare(no.pool, complete.pool, partial.pool,
                true.beta = true.beta2[2],
                subtitle="레짐 2의 기울기 추정치 비교: 세로선이 참값")

RMSE <- function(sample, true){sqrt(mean((sample-true)^2))}
no.pool <- beta2pre.no.pool
partial.pool <- beta2pre.partial
cat("Regime 1 no pooling RMSE: ", RMSE(no.pool, true=true.beta1[2]), "\n")
cat("Regime 1 partial pooling RMSE: ", RMSE(partial.pool, true=true.beta1[2]), "\n")

no.pool <- beta2post.no.pool
partial.pool <- beta2post.partial
cat("Regime 2 no pooling RMSE: ", RMSE(no.pool, true=true.beta2[2]), "\n")
cat("Regime 2 partial pooling RMSE: ", RMSE(partial.pool, true=true.beta2[2]), "\n")


par(mfrow=c(1, 2), mai=c(0.4, 0.6, 0.3, 0.05), cex.main=0.8)
plot(sim1, density=FALSE)


# 2절 프로빗 회귀분석 전환점 모형

library(MCMCpack)
set.seed(1973)
x1 <- rnorm(300, 0, 1)
true.beta <- c(-5, .2, 1)
true.alpha <- c(.1, -1., .2)
X <- cbind(1, x1)
X

## 두 개의 전환점 생성: 100 and 200
true.phi1 <- pnorm(true.alpha[1] + x1[1:100]*true.beta[1])
true.phi2 <- pnorm(true.alpha[2] + x1[101:200]*true.beta[2])
true.phi3 <- pnorm(true.alpha[3] + x1[201:300]*true.beta[3])

## 종속변수 생성
y1 <- rbinom(100, 1, true.phi1)
y2 <- rbinom(100, 1, true.phi2)
y3 <- rbinom(100, 1, true.phi3)
Y <- as.ts(c(y1, y2, y3))
Y

## 서로 다른 전환점 수를 가진 여러 개의 모형을 추정
out0 <- MCMCprobitChange(formula=Y~X-1, data=parent.frame(), m=0,
                         mcmc=1000, burnin=1000, b0 = 0, B0 = 0.1,   
                         marginal.likelihood = c("Chib95"))
out1 <- MCMCprobitChange(formula=Y~X-1, data=parent.frame(), m=1,
                         mcmc=1000, burnin=1000, b0 = 0, B0 = 0.1,   
                         marginal.likelihood = c("Chib95"))
out2 <- MCMCprobitChange(formula=Y~X-1, data=parent.frame(), m=2,
                         mcmc=1000, burnin=1000, b0 = 0, B0 = 0.1,   
                         marginal.likelihood = c("Chib95"))
out3 <- MCMCprobitChange(formula=Y~X-1, data=parent.frame(), m=3,
                         mcmc=1000, burnin=1000, b0 = 0, B0 = 0.1,   
                         marginal.likelihood = c("Chib95"))

## 모형 설명력 비교
BayesFactor(out0, out1, out2, out3)[3]


par(mfrow=c(1, 3), mai=c(0.4, 0.6, 0.3, 0.05))
plotState(out1, main="전환점 1개")
plotState(out2, main="전환점 2개")
plotState(out3, main="전환점 3개")

plotChangepoint(out2, verbose = TRUE, ylab="Density")

# 3절 서수형 프로빗 회귀분석 전환점 모형

set.seed(1909)
N <- 200
x1 <- rnorm(N, 1, .5)

## 전환점 1개를 100에 설정하고
## 레짐별 모수(1, 1), (1, -0.2)로 프로빗 은닉 변수 z 생성
z1 <- 1 + x1[1:100] + rnorm(100);
z2 <- 1 - 0.2*x1[101:200] + rnorm(100);
z <- c(z1, z2);
y <- z

## 이분종속변수 생성
y[z < 1 ] <- 1
y[z >= 1 & z < 2] <- 2
y[z >= 2] <- 3

formula <- y~x1
out1 <- MCMCoprobitChange(formula, m=1,
                          mcmc=100, burnin=100, thin=1, tune=c(.5, .5), 
                          b0=0, B0=0.1, marginal.likelihood = "Chib95")
out2 <- MCMCoprobitChange(formula, m=2,
                          mcmc=100, burnin=100, thin=1, tune=c(.5, .5, .5), 
                          b0=0, B0=0.1, marginal.likelihood = "Chib95")

BayesFactor(out1, out2)[3]

par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05))
plotState(out1, main="전환점 1개", legend.control=c(1, 0.6))
plotState(out2, main="전환점 2개", legend.control=c(1, 0.6))

par(mfrow=c(1, 2), mai=c(0.4, 0.6, 0.3, 0.05))
plotChangepoint(out1, verbose = TRUE, ylab="확률밀도")

# 4절 푸아송 회귀분석 전환점 모형

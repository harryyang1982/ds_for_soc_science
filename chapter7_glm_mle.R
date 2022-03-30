source("jhp.R")

set.seed(1999)
X <- cbind(1, runif(100))
theta.true <- c(1, -1, 1)
y <- X%*%theta.true[1:2] + rnorm(100)
mle <- glm(y ~ X-1, family=gaussian)

jhp_report(mle, title="glm을 이용한 최대 우도 추정", dep.var.labels = "$y$")
X

## 3.1 그리드 탐색을 통한 최대 우도 추정

poisson.loglike <- function(lambda, y) {
  n <- length(y)
  loglik <- sum(y) * log(lambda) - n*lambda
  return(loglik)
}


par(mar=c(3, 3, 2, 1), mgp=c(2, .7, 0), tck=.02)  
set.seed(1999)

## 자료생성 및 우도 계산
y <- rpois(10000, 3)
lambda.list <- seq(1, 6, length=100)
loglike.holder <- sapply(lambda.list, function(k) { poisson.loglike(lambda = k, y=y)})

## 우도 시각화
plot(lambda.list, loglike.holder, pch=19,
     col=addTrans("forestgreen", 80),
     xlab=expression(~lambda), ylab="log likelihood")
grid(col="lightgray")

## 최대 우도 추정
mle.est <- lambda.list[which.max(loglike.holder)]; print(mle.est)
abline(v=mle.est, col="firebrick2", lty=3)
abline(v=3, col="firebrick4", lty=1)
legend("topright", legend=c("true", "mle estimate"),
                            lty=c(1, 3), col=c("firebrick4", "firebrick2"), bty="n")

normal.like <- function(theta, y){
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- length(y)
  log1 <- -.5*n*log(2*pi)-.5*n*log(sigma2)-(1/(2*sigma2))*sum((y-mu)^2)
  return(log1)
}

par(mar=c(3, 3, 2, 1), mgp=c(2,.7,0), tck=.02)
## 자료생성 및 우도 계산
set.seed(1999)
y <- rnorm(10000)
mu.list <- seq(-3, 3, length=1000)
loglike.holder <- sapply(mu.list, function(k){
  normal.like(theta = c(k, 1), y=y)})

## 우도 시각화
plot(mu.list, loglike.holder, pch=19,
     col=addTrans("forestgreen", 50), cex=0.5,
     xlab=expression(~mu), ylab="log likelihood")
grid(col="lightgray")

## 최대 우도 추정
mle.est <- mu.list[which.max(loglike.holder)]; print(mle.est)

abline(v=mle.est, col="firebrick4", lty=3)
abline(v=0, col="firebrick2", lty=1)
legend('topright', legend=c("true", "mle estimate"),
       lty=c(1, 3), col=c("firebrick2", "firebrick4"), bty='n')


#3.2 뉴튼-랩슨 방법을 이용한 최대 우도 추정

### 3.2.1 optim()을 이용한 선형회귀분석 모형의 최대 우도 추정
lm.like <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma <- exp(theta[k + 1])
  loglike <- sum(log(dnorm(y, X %*% beta, sigma)))
  return(loglike)
}

set.seed(1999)
X <- cbind(1, runif(100))
theta.true <- c(1, -1, 1)
y <- X %*% theta.true[1:2] + rnorm(100)

mle <- optim(c(1, 1, 1), lm.like, method="BFGS", control = list(fnscale = -1),
             hessian=TRUE, y=y, X=X)
mle

K <- ncol(X)
psi.mu <- mle$par[K + 1]
psi.sd <- sqrt(solve(-mle$hessian)[K + 1, K + 1])
sigma.mle <- exp(psi.mu + 0.5*psi.sd)
sigma.mle

ols <- lm(y ~ X-1)
summary(ols)$sigma

### 3.2.2 optim()을 이용한 프로빗 모형의 최대 우도 추정

probit.like <- function(beta) {
  ## 선형함수
  eta <- X %*% beta
  ## 확률
  p <- pnorm(eta)
  ## 로그 우도
  return(sum((1-y)*log(1-p)+y*log(p)))
}

probit.gr <- function (beta) {
  ## 선형함수
  mu <- X %*% beta
  
  ## 확률
  p <- pnorm(mu)
  
  ## 체인규칙
  u <- dnorm(mu) * (y - p) / (p * (1 - p)) 
  
  return(crossprod(X, u))
}

suppressMessages(library(MCMCpack))
data(birthwt)
formula <- low~age+as.factor(race)+smoke
ols <- lm(formula, data=birthwt, y=TRUE, x=TRUE)
y <- ols$y
X <- ols$x


## ----probit, fig.cap="프로빗모형의 최대우도추정", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
fit <- optim(ols$coef, probit.like, gr = probit.gr, control = list(fnscale = -1), 
             method = "BFGS", hessian =  TRUE)
fit.glm <- glm(formula, data=birthwt, 
               family = binomial(link = "probit"))
plot(fit$par, coef(fit.glm), xlab="optim 추정치", ylab="glm 추정치")
abline(a=0, b=1, col="red", lty=3)

## 3.3 왈드검정과 우도비검정(likelihood ratio test)
set.seed(1999)
X <- cbind(1, runif(100))
theta.true <- c(1, -1, 1)
y <- X%*%theta.true[1:2] + rnorm(100)
K <- ncol(X)

mle <- optim(c(1, 1, 1), lm.like, method="BFGS",
             control=list(fnscale = -1),
             hessian=TRUE, y=y, X=X)

beta.mle <- mle$par[1:K]
beta.mle.var <- solve(-mle$hessian)[1:K, 1:K]
psi.mu <- mle$par[K+1]
psi.var <- sqrt(solve(-mle$hessian)[K + 1, K + 1])
sigma.mle <- exp(psi.mu + 0.5*sqrt(psi.var))
sigma.mle.var <- sigma.mle^2*exp(sqrt(psi.var) - 1)
coef.se <- c(sqrt(diag(beta.mle.var)), sqrt(sigma.mle.var))
coef.mle <- c(beta.mle, sigma.mle)

t.stat <- coef.mle/coef.se
pval <- 2*(1-pt(abs(t.stat), nrow(X)-ncol(X)))
results <- cbind(coef.mle, coef.se, t.stat, pval)
colnames(results) <- c("coef", "se", "t-stat", "p-value")
rownames(results) <- c("Constant", "slope", "sigma2")
print(results, digits=3)

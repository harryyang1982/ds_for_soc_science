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

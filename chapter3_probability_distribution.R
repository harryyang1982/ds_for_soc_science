source("jhp.R")
# 2.2 이항분포

# 동전 던지기 10번 하는 걸 200번 시뮬레이션.

set.seed(1999)
n <- 10
p <- 0.5

# 확률 p=0.5와 n=10를 가진 이항 확률변수를 200번 출력
df.binom <- data.frame(x = rbinom(200, n, p))
table(df.binom)

ggplot(df.binom, aes(x)) +
  geom_histogram(binwidth=0.3) +
  theme_jhp() + xlab("동전의 앞면이 나온 횟수") + ylab("빈도") + 
  scale_x_continuous(breaks=seq(1, 9, 1))

# 2.3 다항분포(Multinomial Distribution)

dmultinom(x=c(28, 28, 23), prob=c(1/3, 1/3, 1/3))

# 2.4 기하분포(Geometric Distribution)

p <- 0.5
x <- 0:5
dgeom(x, p)

df.geom <- data.frame(x = x, y = dgeom(x, p))
ggplot(df.geom, aes(x=x, y=y)) +
  geom_line() + geom_point(size=3, alpha=0.3) +
  theme_jhp() + xlab("Y") + ylab("밀도")

# 2.5 초기하분포(Hypergeometric Distribution)

fpcf <- function(N, n) {
  sqrt((N-n)/(N-1))
}

n <- 100
s = 0.05

## 보정 계수
fpcf(N=1000, n=100)

tvalue <- qt(0.975, df=(n-1))
# 보정 전의 신뢰 구간
ci.raw <- c(0.52 + (tvalue*s/sqrt(n)),
            0.52 - (tvalue*s/sqrt(n)))
ci.raw

ci.correct <- c(0.52 + (tvalue*s/sqrt(n))*fpcf(N=1000, n=100),
                0.52 - (tvalue*s/sqrt(n))*fpcf(N=1000, n=100))

ci.correct

# 보정 전후 신뢰구간의 그래프 그리기
par(mfrow=c(2,2))
par(mar=c(3, 3, 2, 1), mgp=c(2, .7, 0), tck=-0.01)
## plot 1
plot(x = ci.raw, y=c(2,2), ylim=c(0,3), type="l",
     ylab="", xlab="", lwd=3, axes=FALSE)

## plot 2
plot(x = ci.raw, y = c(2,2), ylim=c(0, 3), type="l",
     ylab="", xlab="", lwd=3, axes=FALSE)

## plot 3
plot(x = ci.raw, y = c(2, 2), ylim=c(0, 3), type="l",
     ylab="", xlab="", lwd=3, axes=FALSE)
axis(1); grid()
lines(x = ci.correct, y = c(1, 1), lwd=3, col="brown")

## plot 4
plot(x = ci.raw, y = c(2, 2), ylim=c(0, 3), type="l",
     ylab="", xlab="", lwd=3, axes=FALSE)
axis(1); grid()
lines(x = ci.correct, y = c(1, 1), lwd=3, col="brown")
text(mean(ci.raw), 1.75, "보정 전 신뢰구간", adj=0)
text(mean(ci.raw), 0.75, "보정 후 신뢰구간", adj=0)

dhyper(2, 20, 30, 4)

dhyper(3, 4, 4, 4) + dhyper(4, 4, 4, 4)

dhyper(4, 4, 4, 4)

# 2.6 푸아송분포(Poisson Distribution)
set.seed(1990)
lambda <- 18
n <- 1000
x <- rpois(n, lambda)

df.pois <- data.frame(x = x, y = dpois(x, lambda))
ggplot(df.pois, aes(x=x, y=y)) +
  geom_line() + geom_point(size=3, alpha=.1) +
  theme_jhp() + xlab("Y") + ylab("밀도")

# 2.7 음이항분포(Negative Binomial Distribution)
rnbinom(10, 5, 0.5)

x <- 1:50
y <- dnbinom(x, 5, 0.5)
df.nbinom <- data.frame(x =x , y = y)
ggplot(df.nbinom, aes(x=x, y=y)) +
  geom_line() + geom_point(size=3, alpha=.3) +
  theme_jhp() + xlab("Y") + ylab("밀도")

# 3절 연속확률분포

# 3.1 균등분포(Uniform Distribution)

set.seed(2000)

curve(dunif(x, 0, 1), lwd=2, xlim=c(0, 4), ylim=c(0, 1),
      ylab="밀도", xlab="Y", col=1)
grid()

curve(dunif(x, 0, 2), lwd=2, add=T, col=2)
curve(dunif(x, 0, 3), lwd=2, add=T, col=3)
curve(dunif(x, 0, 4), lwd=2, add=T, col=4)

legend("topright", lwd=2, bty="n",
       legend=c('Unif(0, 1)', 'Unif(0, 2)', 'Unif(0, 3)', 'Unif(0, 4)'),
       col=1:4)

# 3.2 베타분포(Beta Distribution)
set.seed(2000)

curve(dbeta(x, 2, 2), lwd=2, xlim=c(0, 1), ylim=c(0, 3),
      ylab="밀도", xlab="Y", col=1)
grid()
curve(dbeta(x, 3, 1), lwd=2, add=T, col=2)
curve(dbeta(x, 1, 3), lwd=2, add=T, col=3)
curve(dbeta(x, 1, 1), lwd=2, add=T, col=4)
legend("top", lwd=2, bty="n",
       legend=c('Beta(2, 2)', 'Beta(3, 1)', 'Beta(1, 3)', 'Beta(1, 1)'),
       col=1:4)

# 3.3 지수분포(Exponential Distribution)
pexp(300, rate=1/3000)

set.seed(2000)
curve(dexp(x, 7), lwd=1, xlim=c(0, 4), ylim=c(0,5),
      ylab="밀도", xlab="Y", col=addTrans('firebrick4', 50))
grid()
curve(dexp(x, 5), lwd=2, add=T, col=addTrans('firebrick4', 100))
curve(dexp(x, 3), lwd=3, add=T, col=addTrans('firebrick4', 150))
curve(dexp(x, 1), lwd=4, add=T, col=addTrans('firebrick4', 200))
legend('topright', lwd=1:5, bty='n',
       legend=c('Exp(7)', 'Exp(5)', 'Exp(3)', 'Exp(1)'),
       col=c(addTrans('firebrick4', 50), addTrans('firebrick4', 100),
             addTrans('firebrick4', 150), addTrans('firebrick4', 200)))

# 3.4 정규분포(normal distribution)
require(mlbench)
data("PimaIndiansDiabetes")
ggplot(PimaIndiansDiabetes, aes(x=mass)) +
  geom_density(fill="brown", alpha=0.3) +
  geom_vline(xintercept = mean(PimaIndiansDiabetes$mass, na.rm = TRUE),
             linetype="dashed", color="blue", size=1) +
  stat_function(fun=dnorm, color="red",
                args=list(mean=mean(PimaIndiansDiabetes$mass),
                               sd=sd(PimaIndiansDiabetes$mass))) +
  labs(caption = "자료출처: PimaIndiansDiabetes") +
  theme_jhp() + xlab("Y") + ylab("밀도")

curve(dnorm(x, 0, 0.5), lwd=2, xlim=c(-6, 6), ylim=c(0, .8),
      ylab="밀도", xlab="Y", col=1)
grid
curve(dnorm(x, 0, 1), lwd = 2, add=T, col=2)
curve(dnorm(x, 0, 2), lwd = 2, add=T, col=3)
curve(dnorm(x, 0, 3), lwd = 2, add=T, col=4)
legend("topleft", legend=c('N(0, 0.25)', 'N(0, 1)', 'N(0, 4)', 'N(0, 9)'),
       lwd=2, bty="n", col=1:4)

# 3.5 디리클레분포(Dirichlet Distribution)
if(!require(plot3D)) install.packages("plot3D")
if(!require(MCMCpack)) install.packages("MCMCpack")
if(!require(akima)) install.packages("akima")
if(!require(rgl)) install.packages("rgl")

library(plot3D)
library(MCMCpack)
library(akima)
library(rgl)

#디리클레 모수
alpha_params <- c(2, 2, 2)

# 심플렉스에 그릴 정규화된 그리드 변수 작성
granularity <- 20
draws <- matrix(ncol=3, nrow=(granularity*granularity*granularity)-1)
i <- 0
for (x in 1:granularity) {
  for (y in 1:granularity) {
    for (z in 1:granularity) {
      draws[i, ] <- c(x, y, z)
      draws[i, ] <- draws[i, ] / sum(draws[i, ])
      i <- i + 1
    }
  }
}
x <- draws[, 1]
y <- draws[, 2]
z <- draws[, 3]
density <- ddirichlet(draws, alpha_params)
# 유클리드 공간으로 심플렉스를 변환
x <- .5 * (2*x+y)
y <- .5 * y * sqrt(3)
# (100x100) grid
grid <- interp(x, y, density, duplicate="strip", linear=FALSE,
                   xo=seq(min(x), max(x), length=100),
                   yo=seq(min(y), max(y), length=100))

# heatmap
image2D(x=grid$x, y=grid$y, z=grid$z)

# 3.6 스튜던트 t분포(Student t Distribution)
curve(dt(x, df = 1), lwd=1, xlim=c(-6, 6), ylim=c(0, .45),
                                 ylab="밀도", xlab="y", col=1)

grid()
curve(dt(x, df = 5), lwd = 1, add=TRUE, col=2)
curve(dt(x, df = 10), lwd = 1, add=TRUE, col=3)
curve(dt(x, df=30), lwd=1, add=TRUE, col=4)
curve(dnorm(x, 0, 1), lwd=5, add=TRUE, col=addTrans(6, 50))
legend("topleft", 
       legend=c('t(0, 1, 1)', 't(0, 1, 5)', 't(0, 1, 10)', 't(0, 1, 30)', 'N(0, 1)'),
       lwd=c(1, 1, 1, 1, 5), bty="n", col=c(1:4, addTrans(6, 50)))

# 3.7 코시분포(Cauchy Distribution)
curve(dcauchy(x, scale = 1), lwd = 1, xlim=c(-8, 8), ylim=c(0, .35),
      ylab="밀도", xlab="Y", col=1)
grid()
curve(dcauchy(x, scale=2), lwd=1, add=T, col=2)
curve(dcauchy(x, scale=3), lwd=1, add=T, col=3)
curve(dcauchy(x, scale=4), lwd=1, add=T, col=4)
curve(dt(x, df=1), lwd=5, add=T, col=addTrans(6, 50))
legend("topleft",
       legend=c("Cauchy(0,1)", "Cauchy(0,2)", "Cauchy(0,3)", "Cauchy(0,4)", "t(0,1,1)"),
       lwd=c(1,1,1,1,5), bty="n", col=c(1:4, addTrans(6, 50)))

# 3.8 라플라스분포(Laplace Distribution)

require(rmutil)
curve(dlaplace(x, s=1), lwd=1, xlim=c(-8, 8), ylim=c(0, .55),
      ylab="밀도", xlab="Y", col=1)
grid()
curve(dlaplace(x, s=2), lwd=1, add=T, col=2)
curve(dlaplace(x, s=3), lwd=1, add=T, col=3)
curve(dlaplace(x, s=4), lwd=1, add=T, col=4)
legend("topleft", legend=c('Laplace(0, 1)', 'Laplace(0, 2)', 'Laplace(0, 3)', 'Laplace(0, 4)'),
                           lwd=1, bty='n', col=1:4)

# 4절 중심극한정리
# 1단계 확률 분포 만들기
my.samples <- function(dist, r, n, param1, param2=NULL){
  set.seed(123)
  switch(dist,
         "Exponential" = matrix(rexp(r*n, param1), r),
         "Normal" = matrix(rnorm(r*n, param1, param2), r),
         "Uniform" = matrix(runif(r*n, param1, param2), r),
         "Poisson" = matrix(rpois(r*n, param1), r),
         "Binomial" = matrix(rbinom(r*n, param1, param2), r),
         "Beta" = matrix(rbeta(r*n, param1, param2), r),
         "Gamma" = matrix(rgamma(r*n, param1, param2), r),
         "Chi-squared" = matrix(rchisq(r*n, param1), r),
         "Cauchy" = matrix(rcauchy(r*n, param1, param2), r)
         )
}

# 2단계 평균과 표준편차 계산
mu <- function(dist, param1, param2=NULL) {
  switch(dist,
         "Exponential" = param1^-1,
         "Normal"=param1,
         "Uniform" = (param1+param2)/2,
         "Poisson" = param1,
         "Binomial"=param1*param2,
         "Beta" = param1/(param1+param2),
         "Gamma" = param1 / param2,
         "Chi-squared" = param1,
         "Cauchy" = param1)
}

sigma <- function(dist, param1, param2=NULL) {
  switch(dist,
         "Exponential" = param1^-1,
         "Normal" = param2,
         "Uniform"= sqrt((param2-param1)^2/12),
         "Poisson" = sqrt(param1),
         "Binomial" = sqrt(param1*param2*(1-param2)),
         "Beta"=sqrt(param1*param2/((param1+param2)^2*(param1+param2+1))),
         "Gamma"=sqrt(param1/(param2)^2),
         "Chi-squared"=sqrt(2*param1),
         "Cauchy"=sqrt(param2))
}

# 3단계 CLT함수로 10,000번 표본 추출하는 시뮬레이션 진행 => 히스토그램 및 커브 시각화

CLT <- function(dist, param1, param2=NULL, r=10000) {
  ## dist = 확률밀도함수
  ## r = 반복추출횟수
  par(mfrow=c(3, 3), mgp=c(1.75, .75, 0),
      oma=c(2, 2, 2, 2), mar=c(3, 3, 2, 0), xpd=NA)
  for (n in c(1:6, 10, 50, 100)) {
    samples <- my.samples(dist, r, n, param1, param2)
    ##표본평균 계산
    sample.means <- apply(samples, 1, mean)
    ## 표본평균 히스토그램
    hist(sample.means, col=ifelse(n<=10, grey(.1*(11-n)), rgb(0, 0, n, max=110)),
         freq=FALSE, xlab="Sample Mean", main=paste("n=", n))
    
    ## CLT 정규분포 그리기 N(mean=mu, sd=sigma/sqrt(n))
    x <- seq(min(sample.means), max(sample.means), length=100)
    curve(dnorm(x, mean=mu(dist, param1, param2),
                sd=(sigma(dist, param1, param2))/sqrt(n)),
          col="red", lwd=2, add=TRUE)
  }
  ## 확률분포 이름 레이블
  mtext(paste(dist, " Distribution (",
              param1, ifelse(is.null(param2), "", ","),
              param2, ")", sep=""), outer=TRUE, cex=1)
}

# 4단계: 함수 실행
CLT("Exponential", 1)

CLT("Uniform", 1, 5)

CLT("Cauchy", 1, 1)

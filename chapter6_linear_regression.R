source("jhp.R")
set.seed(1999)

x <- seq(0, 1, length=5)
y <- x + rnorm(5, 0, .5)
df.reg <- data.frame(x = x, y = y)

fit <- lm(y ~ x, data=df.reg)

fit

plot(x, y, pch=19, cex=1.5, ylim=c(0, 1.5), xlim=c(0, 1.5),
     col=addTrans("blue", 100), asp=1)
abline(fit, col="brown", lwd=1)
segments(x, fit$fitted.values, x, y, col="red", lty=2)

## 직교선 거리 구하기
perp.segment <- function(x0, y0, lm.fit) {
  a <- coef(lm.fit)[1]
  b <- coef(lm.fit)[2]
  x1 <- (x0 + b * y0 - a*b) / (1+b^2)
  y1 <- a+b*x1
  
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}

ss <- perp.segment(x, y, fit)
segments(x0=ss$x0, x1=ss$x1, y0=ss$y0, y1=ss$y1, col="blue", lty=3, pty="s")
legend("bottomright", legend=c("직교거리", "최소제곱선에서의 거리"),
       lty=c(3,2), bty="n", col=c("blue", "red"))

asp <- 0.5
plot(x, y, pch=19, cex=1.5, ylim=c(0, 1.5), xlim=c(0, 1.2),
     col=addTrans("blue", 100), asp=asp);
abline(fit, col='brown', lwd=1)
segments(x, fit$fitted.values, x, y, col="red", lty=2)
text(x, y - fit$res/2, round(fit$res, 2), cex=0.8)
rect(xleft = x - abs(fit$res) / 2 * asp, ybottom = fit$fitted.values,
     xright = x + abs(fit$residuals) / 2 * asp, ytop = fit$fitted.values + fit$residuals,
     col = rgb(1, 0, 0, 0.3), border = NA)

# lm을 통한 최소 제곱 추정

set.seed(1999)
x <- seq(0, 1, length=10)
y <- x + rnorm(10, 0, 0.3)
df.reg <- data.frame(x = x, y = y)
df.reg

plot(x, y, pch=19, cex=1.5, col=addTrans("blue", 100), asp=0.5, main="")

fit <- lm(y ~ x, data=df.reg)
plot(x, y, pch=19, cex=1.5, col=addTrans("blue", 100), asp=asp)
abline(fit, col="brown", lwd=1)
segments(x, fit$fitted.values, x, y, col='red', lty=2)
text(x, y - fit$res/2, round(fit$residuals, 2), cex=0.8)

rect(xlef = x - abs(fit$residuals) / 2 * asp, ybottom = fit$fitted.values,
     xright = x + abs(fit$residuals) / 2 * asp, ytop = fit$fitted.values + fit$residuals,
     col = rgb(1, 0, 0, 0.3), border=NA)

## 자료 생성
set.seed(1973)
N <- 100
x <- runif(N, 6, 20)
D <- rbinom(N, 1, .5)
y <- 1 + 0.5*x - .4*D + rnorm(N)
df.lm <- data.frame(y = y, x = x, D = D)
df.lm$D <- factor(df.lm$D, labels = c("남성", "여성"))

## 회귀 분석
reg.parallel <- lm(y ~ x + D, data=df.lm)
jhp_report(reg.parallel, title="더미변수 회귀모형 추정결과", label="tab:D", 
           dep.var.labels="$y$")

par(mar=c(3, 3, 2, 1), mgp=c(2,.7,0), tck=.02)
plot(x, y, xlab="교육", ylab="소득", pch=19, col=addTrans("gray20", 40))
abline(a = reg.parallel$coef[1], b=reg.parallel$coefficients[2], lwd=4, col=addTrans("brown", 200))
abline(a = reg.parallel$coefficients[1], reg.parallel$coefficients[3],
       b = reg.parallel$coefficients[2], lwd=4, col=addTrans("brown", 100))       
legend("topleft", legend=c("남성", "여성"), bty="n", lwd=c(4, 4), lty=c(1, 1),
       col=c(addTrans("brown", 200), addTrans("brown", 100)))

reg.inter <- lm(y ~ x * D)
jhp_report(reg.inter, title="상호작용 회귀모형 추정결과", label="tab:xD",
           dep.var.labels = "$y$")

par(mar=c(3, 3, 2, 1), mgp=c(2,.7,0), tck=.02)
plot(x, y, xlab="교육", ylab="소득", pch=19, col=addTrans("gray20", 40))
abline(a = reg.inter$coefficients[1], b=reg.inter$coefficients[2],
       lwd=4, col=addTrans("brown", 200))
abline(a = reg.inter$coefficients[1] + reg.inter$coefficients[3],
       b = reg.inter$coefficients[2] + reg.inter$coefficients[4],
       lwd=4, col=addTrans("brown", 100))
legend("topleft", legend=c("남성", "여성"), bty="n", lwd=c(4, 4), lty=c(1, 1),
       col=c(addTrans("brown", 200), addTrans("brown", 100)))

set.seed(1973)
N <- 100
x <- runif(N, 6, 20)
D <- rbinom(N, 1, .5)
y <- 1 + 0.5*x - .4*D + 0.4*D*x + rnorm(N)
df.lm2 <- data.frame(y = y, x = x, D = D)
df.lm2$D <- factor(df.lm2$D, labels=c('여성', '남성'))

reg.parallel2 <- lm(y ~ x + D, data=df.lm2)
reg.inter2 <- lm(y ~ x * D, data=df.lm2)
jhp_report(reg.parallel2, reg.inter2,
           title="상호작용 회귀모형과 합산형 모형의 비교",
           label="tab:xDD", dep.var.labels = "$y$")

par(mfrow=c(1, 2), mar=c(3, 3, 2, 1), mgp=c(2, .7, 0), tck=.02, cex.main=0.5)
## 병렬분석
plot(df.lm2$x, df.lm2$y, main="병렬분석", pch=19, col=addTrans("gray20", 40),
     xlab = "교육", ylab = "소득")
abline(a = reg.parallel2$coefficients[1], b = reg.parallel2$coefficients[2],
       lwd=4, col=addTrans("brown", 100))
abline(a = reg.parallel2$coefficients[1] + reg.parallel2$coefficients[3],
       b = reg.parallel2$coefficients[2], lwd=4, col=addTrans("brown", 200))
legend("topleft", legend=c("남성", "여성"), bty="n", lwd=c(4, 4),
       lty=c(1, 1),
       col=c(addTrans("brown", 200), addTrans("brown", 100)))

## 상호작용분석
plot(df.lm2$x, df.lm2$y, main="상호작용분석", pch=19,
     col=addTrans("gray20", 40),
     xlab = "교육", ylab = "소득")
abline(a = reg.inter2$coefficients[1], b = reg.inter2$coefficients[2],
       lwd=4, col=addTrans("brown", 100))
abline(a = reg.inter2$coefficients[1] + reg.inter2$coefficients[3],
       b = reg.inter2$coefficients[2] + reg.inter2$coefficients[4],
       lwd=4, col=addTrans("brown", 200))
legend("topleft", legend=c("남성", "여성"), bty="n", lwd=c(4, 4),
       lty=c(1, 1),
       col=c(addTrans("brown", 200), addTrans("brown", 100)))


# 3.5 predict()를 이용한 신뢰구간 추정
methods("predict")

## 회귀분석 예측치
df.lm2$pred.parallel2 <- predict(reg.parallel2)
df.lm2$pred.inter2 <- predict(reg.inter2)

## 신뢰구간
conf.parallel2 <- predict(reg.parallel2, interval = "prediction")
conf.inter2 <- predict(reg.inter2, interval = "prediction")
head(conf.parallel2)
head(conf.inter2)

## 병렬분석
df.parallel <- cbind(df.lm2, conf.parallel2)
g.parallel <- ggplot(df.parallel, aes(x = x, y = y, color = D)) +
  geom_point() +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=D, color=NULL), alpha=.2) +
  geom_line(aes(y = fit), size = 1) +
  labs(subtitle = "병렬분석", fill = NULL, color = NULL) +
  theme_jhp() + xlab("교육") + ylab("소득")

## 상호작용분석
df.inter <- cbind(df.lm2, conf.inter2)
g.inter <- ggplot(df.inter, aes(x = x, y = y, color = D)) +
  geom_point() +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=D, color=NULL), alpha = .2) +
  geom_line(aes(y = fit), size = 1) +
  labs(subtitle = "상호작용분석", fill = NULL, color = NULL) +
  theme_jhp() + xlab("교육") + ylab("소득")

NetworkChange::multiplot(g.parallel, g.inter, cols=2)

#제 4절 회귀분석 모형의 진단
## 4.1 명성 자료를 이용한 회귀분석 진단

require(car)
data(Prestige)
Prestige <- Prestige %>% drop_na()
Prestige

par(mar=c(3, 3, 2, 1), mgp=c(2, .7, 0), tck=.02)
library(tidyverse)

Prestige %>% 
  dplyr::select(education, income, women, prestige) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free_x")

par(mar=c(3, 3, 2, 1), mgp=c(2, .7, 0), tck=.02)
library(GGally)
Prestige %>% 
  dplyr::select(education, income, women, prestige, type) %>% 
  ggpairs(aes(alpha = 0.4))


par(mar=c(3, 3, 2, 1), mgp=c(2, .7, 0), tck=.02)
ggplot(Prestige, aes(x = education, y = prestige, color=type)) +
  geom_point(size = 5, alpha = 0.5) +
  labs(color = NULL) +
  theme_jhp()

reg1 <- lm(prestige ~ education, data=Prestige)
jhp_report(reg1, title="직업별 명성과 교육수준의 관계",
           label="tab:prestige1",
           dep.var.labels = "prestige")

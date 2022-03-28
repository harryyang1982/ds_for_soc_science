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

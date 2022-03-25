source("jhp.R")
dt <- data.frame(row.names = c("D", "I", "R"),
                 female = c(279, 73, 225),
                 male = c(165, 47, 191))
dt

t(apply(dt, 1, prop.table))

apply(dt, 2, prop.table)

prop.table(dt)

library(scatterplot3d)

x <- c("D", "I", "R")
y <- c("Female", "Male")
z <- prop.table(dt)

mydat <- data.frame("정당일체감" = as.vector(row(dt)),
                    "성별" = as.vector(col(dt)),
                    "확률" = as.vector(unlist(z)))

mydat

scatterplot3d(mydat, type="h", lwd=2,
              x.ticklabs = c("민주당", "", "무당파", "", "공화당"),
              y.ticklabs = c("여성", "", "", "", "", "남성"),
              col.axis="blue", col.grid="lightblue", angle=60, pch=20,
              y.margin.add = 0.5, cex.symbols=3, color="brown", box=FALSE)

prop.table(apply(dt, 2, sum))

# 카이제곱 검정

par(mar=c(3, 3, 2, 1), mgp=c(2,.7,0), tck=.02)
curve(dchisq(x, 1), from = 0, to = 100, lwd=2, ylab = "f(y)", xlab="y",
      col=addTrans('firebrick4', 50))
curve(dchisq(x, 5), from = 0, to = 100, lwd = 2, add=TRUE,
      col=addTrans('firebrick4', 100))
curve(dchisq(x, 10), from = 0, to = 100, lwd = 2, add=TRUE,
      col=addTrans('firebrick', 150))
curve(dchisq(x, 50), from = 0, to = 100, lwd = 2, add=TRUE,
      col=addTrans('firebrick4', 200))
legend("topright", 
       legend = c('df = 1', 'df = 5', 'df = 10', 'df = 50'),
       lwd=2, bty="n",
       col=c(addTrans('firebrick4', 50), addTrans('firebrick4', 100),
             addTrans('firebrick4', 150), addTrans('firebrick4', 200)))

test0 <- chisq.test(dt)
test0$expected
test0

summary(test0)
test0$stdres

fisher.test(dt)

data(Titanic)
tab.class <- apply(Titanic, c(1, 4), sum)
tab.class

test.class <- chisq.test(tab.class[1:3, ])
test.class

test.class$stdres

# pearson correlation
y <- c(10, 9, 9.1, 7, 5)
x <- c(3.4, 2.9, 3.3, 3, 3.9)
r <- sum((x - mean(x))*(y - mean(y))) / 4 / (sd(x) *sd(y))
r
cor.test(x=x, y=y)

denom <- norm(as.matrix(x - mean(x)), "f") * norm(as.matrix(y - mean(y)), "f")
numer <- t(x - mean(x)) %*% ( y - mean(y))
numer / denom

cor.test(x, y, method="kendall")

cor.test(x, y, method="spearman")

# 2.2 앤스콤의 사중주

require(stats); require(graphics)
anscombe
apply(anscombe, 2, mean)

apply(anscombe, 2, sd)

rhos <- setNames(as.list(1:4), paste0("rho", 1:4))
for(i in 1:4) {
  rhos[[i]] <- with(anscombe,
                    eval(parse(text=paste0("cor(y", i, ", x", i, ")"))))
}
rhos

op <- par(mfrow = c(2, 2), mar = 0.1+c(4, 4, 1, 1),
          oma = c(0, 0, 2, 0))
ff <- y ~ x
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y", "x"), i), as.name)
  fit <- lm(ff, data = anscombe)
  plot(ff, data = anscombe, col = "brown",
       pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(fit, lwd = 2, col = addTrans('blue', 70))
  abline(a=0, b=rhos[[i]], lwd=2, col=addTrans("red", 90))
  legend("topleft", legend=c("correlation", "regression line"),
         bty="n", lty=1, lwd=2,
         col=c(addTrans("red", 90), addTrans("blue", 70)))
}

par(op)

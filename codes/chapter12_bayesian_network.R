source('jhp.R')

library(igraph)

n <- 15
g1 <- graph.ring(n)
g2 <- erdos.renyi.game(n, .2)
g3 <- watts.strogatz.game(1, n, 2, 0.05)
g4 <- barabasi.game(n)

require(randomNames)
vertex.names <- randomNames(n, which.names = "first")
par(mfrow=c(2,2), mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)

plot(g1, vertex.label.cex=0.8, vertex.size=20, vertex.label=vertex.names,
     layout=layout.circle, xlab="Ring Network")

plot(g2, vertex.label.cex=0.8, vertex.size=20, vertex.label=vertex.names,
     layout=layout.circle, xlab="Random Network")

plot(g3, vertex.label.cex=0.8, vertex.size=20, vertex.label=vertex.names,
     layout=layout.circle, xlab="Small-world Network")

plot(g4, vertex.label.cex=0.8, vertex.size=20, vertex.label=vertex.names,
     layout=layout.circle, xlab="Scale-free Network")

# 척도 없는 네트워크 생성
par(mfrow=c(1, 2), mai=c(0.4, 0.6, 0.3, 0.05))
require(igraph)
set.seed(1973)

g1 <- static.power.law.game(no.of.nodes = 200,
                            no.of.edges = 400,
                            exponent.out = 2.2,
                            exponent.in = -1, loops = FALSE,
                            multiple = TRUE,
                            finite.size.correction = TRUE)
g2 <- static.power.law.game(no.of.nodes = 500,
                            no.of.edges = 1000,
                            exponent.out = 2.2,
                            exponent.in = -1, loops = FALSE,
                            multiple = TRUE,
                            finite.size.correction = TRUE)

## 각 노드의 연결도 정보를 추출
deg1 <- igraph::degree(g1)
deg2 <- igraph::degree(g2)

## 네트워크 시각화
plot(g1, vertex.label=NA, edge.arrow.size=0.02,
     vertex.size = deg1/median(deg1), xlab = "", cex.main=0.5, main="n=200")
plot(g2, vertex.label=NA, edge.arrow.size=0.02,
     vertex.size = deg2/median(deg2), xlab = "", cex.main=0.5, main="n=500")

degreeGraph <- function(graph) {
  degrees <- igraph::degree(graph)
  node.nb <- length(degrees)
  degrees <- degrees[order(degrees, decreasing=TRUE)]
  ## 연결도 분포 계산
  degree.distrib <- table(degrees)
  ## 연결도 정보 데이터 프레임 저장
  degree.freq <- data.frame(degree=as.numeric(row.names(table(degrees))),
                            frequency=as.vector(table(degrees)))
  positive.locator <- degree.freq$degree > 0
  ## 로그 변환
  degree.freq$degree.log <- log(degree.freq$degree, base = 10);
  degree.freq$frequency.log <- log(degree.freq$frequency, base = 10);
  ## 푸아송분포 추정
  mean.degree <- mean(degrees)
  degree.freq$poisson.density <- dpois(x=degree.freq$degree, lambda=mean.degree)
  degree.freq$poisson.exp <- degree.freq$poisson.density * node.nb
  ## 멱법칙 분포 추정
  degree.freq$power.exp <- NA
  fit.domain <- positive.locator
  distrib.fitting <- lm(frequency.log ~ degree.log, data=degree.freq[fit.domain,])
  degree.freq[fit.domain, 'power.exp'] <- 10^(predict.lm(distrib.fitting))
  return(degree.freq)
}

plot.powerlaw <- function(degree.freq, main = "", xlab = '연결', ylab = '노드의 수'){
  plot(degree.freq[, 'degree'],
       degree.freq[, 'frequency'],
       col=NetworkChange:::addTrans('forestgreen', 100),
       type = 'p', pch = 19, cex=1,
       main=main, xlab=xlab, ylab=ylab, cex.main=0.5,
       panel.first=grid(col = 'grey80', equilog=FALSE)
       )
  lines(degree.freq$degree, degree.freq$poisson.exp, lwd=1, col='navy')
  lines(degree.freq$degree, degree.freq$power.exp, lwd=1, col = 'brown')
  legend('topright', col=c('navy', 'brown'),
         legend=c('푸아송', '멱법칙'), lwd=1, bg='white', bty='n')
}

## 연결도 분포 추출
degree.freq1 <- degreeGraph(g1)
degree.freq2 <- degreeGraph(g2)

## 그래프
par(mfrow=c(1, 2), mai=c(1, 1, 0.3, 0.05))
plot.powerlaw(degree.freq1, main="G(200, 0.5)")
plot.powerlaw(degree.freq2, main="G(500, 0.5)")

set.seed(1973)
g3 <- static.power.law.game(no.of.nodes = 10000,
                            no.of.edges = 20000,
                            exponent.out=2.2,
                            exponent.in = -1, loops = FALSE,
                            multiple = TRUE,
                            finite.size.correction = TRUE)
g4 <- static.power.law.game(no.of.nodes = 100000,
                            no.of.edges = 200000,
                            exponent.out = 2.2,
                            exponent.in = -1, loops = FALSE,
                            multiple = TRUE,
                            finite.size.correction = TRUE)

par(mfrow=c(1, 2), mai=c(1, 1, 0.3, 0.05))
degree.freq3 <- degreeGraph(g3)
degree.freq4 <- degreeGraph(g4)
plot.powerlaw(degree.freq3, main="G(10000, 0.5)")
plot.powerlaw(degree.freq4, main="G(100000, 0.5)")

# 2절 네트워크 시각화

require(NetworkChange)
data(PostwarAlly)
Y <- PostwarAlly
year <- dimnames(Y)[[3]]
g <- Y[,, year == "1980"]

plot.sociomatrix.jhp <- 
  function(x, labels = NULL, srt=45, pos=2, lab.col="brown",
           drawlab = TRUE, diaglab = TRUE, drawlines = TRUE, 
           xlab = NULL, ylab = NULL, cex.lab = 1, ...) {
    if ((!(class(x) %in% c("matrix", "array", "data.frame"))) || 
        (length(dim(x)) > 2)) 
      x <- as.sociomatrix.sna(x)
    if (is.list(x)) 
      x <- x[[1]]
    n <- dim(x)[1]
    o <- dim(x)[2]
    if (is.null(labels)) 
      labels <- list(NULL, NULL)
    if (is.null(labels[[1]])) {
      if (is.null(rownames(x))) 
        labels[[1]] <- 1:dim(x)[1]
      else labels[[1]] <- rownames(x)
    }
    if (is.null(labels[[2]])) {
      if (is.null(colnames(x))) 
        labels[[2]] <- 1:dim(x)[2]
      else labels[[2]] <- colnames(x)
    }
    d <- 1 - (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                           min(x, na.rm = TRUE))
    if (is.null(xlab)) 
      xlab <- ""
    if (is.null(ylab)) 
      ylab <- ""
    plot(1, 1, xlim = c(0, o + 1), ylim = c(n + 1, 0), type = "n", 
         axes = FALSE, xlab = xlab, ylab = ylab, ...)
    for (i in 1:n) for (j in 1:o) rect(j - 0.5, i + 0.5, j + 
                                         0.5, i - 0.5, col = gray(d[i, j]), xpd = TRUE, border = drawlines)
    rect(0.5, 0.5, o + 0.5, n + 0.5, col = NA, xpd = TRUE)
    if (drawlab) {
      ## y axis
      text(rep(0, n), 1:n, labels[[1]], cex = cex.lab, 
           col=lab.col, srt = srt, pos = pos)
      ## x axis
      text(1:o, rep(0, o), labels[[2]], cex = cex.lab, 
           col=lab.col, srt = srt, pos = pos)
    }
    if ((n == o) & (drawlab) & (diaglab)) 
      if (all(labels[[1]] == labels[[2]])) 
        text(1:o, 1:n, labels[[1]], cex = cex.lab, 
             col=lab.col, srt = srt, pos = pos)
  }


## ----post.socio, fig.cap="1980-1년 동맹 네트워크의 사회행렬", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1----
plot.sociomatrix.jhp(g, diaglab=FALSE, cex.lab=0.6, pos=3, lab.col="gray40")

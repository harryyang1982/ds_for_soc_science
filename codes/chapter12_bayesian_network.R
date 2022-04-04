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


## 블록 모형

plot.blockmodel.jhp <- 
  function(x, srt=45, pos=2, lab.col="brown",
           cex.lab=cex.lab, drawlab=TRUE, diaglab=TRUE, ...) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    n <- dim(x$blocked.data)[2]
    m <- stackcount(x$blocked.data)
    if (!is.null(x$plabels))
      plab <- x$plabels
    else plab <- (1:n)[x$order.vector]
    if (!is.null(x$glabels))
      lab <- x$labels
    else glab <- 1:m
    par(mfrow = c(floor(sqrt(m)), ceiling(m/floor(sqrt(m)))))
    if (m > 1)
      for(i in 1:m) {
        plot.sociomatrix.jhp(x$blocked.data[i, , ],
                             labels=list(plab, plab),
                             main = "",
                             srt=srt, pos=pos, lab.col=lab.col,
                             cex.lab = cex.lab,
                             drawlab = drawlab, diaglab = diaglab,
                             cex.main=0.5, drawlines = FALSE)
        for (j in 2:n) {
          if (x$block.membership[j] != x$block.membership[j - 1])
            abline(v = j - 0.5, h = j - 0.5, lty=3)
        }
      }
    else {
      plot.sociomatrix.jhp(x$blocked.data,
                           labels=list(plab, plab),
                           main="",
                           srt=srt, pos=pos, lab.col=lab.col,
                           cex.lab = cex.lab,
                           drawlab=drawlab, diaglab=diaglab,
                           cex.main=0.5, drawlines = FALSE)
      for (j in 2:n) {
        if (x$block.membership[j] != x$block.membership[j - 1])
          abline(v = j - 0.5, h = j - 0.5, lty = 3)
      }
    }
  }

library(sna)
eq <- sna::equiv.clust(g)
b <- sna::blockmodel(g, eq, h=10)
plot.blockmodel.jhp(b, main="",
                    diaglab=FALSE, cex.lab=0.6, pos=3, lab.col="gray40")

n <- network(g, directed = FALSE)
n %v% "family" <- as.character(b$block.membership)
n %v% "importance" <- sna::degree(n)

library(ggnetwork)
library(RColorBrewer)

set.seed(1973)
mycolors = c(brewer.pal(name="Dark2", n= 8),
             brewer.pal(name="Paired", n = 6))
g.1 <- ggplot(ggnetwork(n), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color="grey50", alpha=0.5) +
  geom_nodes(aes(x, y, color = family, size= 5.5 * importance),
             alpha=0.5,
             show.legend=FALSE) +
  geom_nodes(aes(x, y, color=family, size=1.5*importance),
             show.legend=FALSE) +
  guides(size = "none") +
  geom_nodelabel_repel(aes(label = vertex.names, color = family,
                           fill = factor(family)), alpha = 0.5,
                       box.padding=0.35, point.padding=0.5, fontface="bold",
                       family="AppleGothic", color="black", size = 4, show.legend = FALSE) +
  scale_color_manual(values = mycolors) +
  theme_blank()

g.1

g1950 <- graph_from_adjacency_matrix(Y[,,year=="1950"])
g1970 <- graph_from_adjacency_matrix(Y[,,year=="1970"])
g1990 <- graph_from_adjacency_matrix(Y[,,year=="1990"])
g2010 <- graph_from_adjacency_matrix(Y[,,year=="2010"])

degree.freq1 <- degreeGraph(g1950)
degree.freq2 <- degreeGraph(g1970)
degree.freq3 <- degreeGraph(g1990)
degree.freq4 <- degreeGraph(g2010)
pdf("figures/allianceNetworkdegree.pdf", width=10, height=10)   
par(mfrow=c(2, 2), mai=c(1, 1, 1, 0.05), cex.main=0.8, cex.axis=0.8)
plot.powerlaw(degree.freq1, xlab="연결도", main="1950년 동맹 네트워크", cex.main=0.8)
plot.powerlaw(degree.freq2, xlab="연결도", main="1970년 동맹 네트워크", cex.main=0.8)
plot.powerlaw(degree.freq3, xlab="연결도", main="1990년 동맹 네트워크", cex.main=0.8)
plot.powerlaw(degree.freq4, xlab="연결도", main="2010년 동맹 네트워크", cex.main=0.8)
dev.off()


# 3절 네트워크 중심성 분석

library(CINNA)
g.graph <- graph.adjacency(Y[,, which(year==1980)], mode="undirected")
## proper_centralities(g.graph)

head(igraph::degree(g.graph))

country.names <- dimnames(Y)[[1]]
## find a connected component of the graph
g.comp <- igraph::components(g.graph)
g.member <- lapply(seq_along(g.comp$csize)[g.comp$csize > 1],
                   function(x) V(g.graph)$name[g.comp$membership %in% x])
## the first group is chosen
locator.member <- is.element(country.names, g.member[[1]])
g.connected <- graph.adjacency(Y[locator.member, locator.member, which(year==1980)], mode="undirected")
pr_cent <- proper_centralities(g.connected)
calc_cent <- 
  calculate_centralities(g.connected, 
                         include  = pr_cent[c(1, 3, 5, 7, 9, 10, 15, 
                                              18, 19, 21, 26, 28, 29)])

par(cex = 0.7)
visualize_heatmap(calc_cent, scale = TRUE)


# 4절 네트워크 전환점



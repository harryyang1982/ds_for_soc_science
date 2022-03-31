# 1절 두개의 봉투 역설

source("jhp.R")

# 2g(x)와 g(x/2) 시각화

set.seed(2000)

par(mfrow=c(1, 2), mar=c(3, 3, 2, 1), mgp = c(2, .7, 0), tck=.02, cex = 0.5)

#graph1
curve(dexp(x, 1), lwd= 2, xlim=c(0, 4), ylim=c(0, 3),
      ylab="밀도", xlab="x", col=addTrans('forestgreen', 200))

curve(dexp(x, 1/2), lwd = 1, add=TRUE, col=addTrans('firebrick4', 100))
curve(2*dexp(x, 1), lwd = 1, add=TRUE, col=addTrans('firebrick4', 200))
legend("topright", lwd=c(2, 1, 1), bty="n",
       legend=c('g(x) = Exp(1)', 'g(x/2) = Exp(1/2)', '2g(x) = 2Exp(1)'),
       col=c(addTrans('forestgreen', 200),
             addTrans('firebrick4', 100),
             addTrans('firebrick4', 200)))

#graph2
curve(dexp(x, 2), lwd=2, xlim=c(0, 4), ylim=c(0,3),
      ylab="밀도", xlab="x", col=addTrans('forestgreen', 200))
curve(dexp(x, 1), lwd=1, add=TRUE, col=addTrans('firebrick4', 100))
curve(2*dexp(x, 2), lwd=1, add=TRUE, col=addTrans('firebrick4', 200))
legend("topright", lwd=c(2, 1, 1), bty="n",
       legend = c('g(x) = Exp(2)', 'g(x/2) = Exp(1)', 
                  '2g(x) = 2Exp(2)'),
       col=c(addTrans('forestgreen', 200),
             addTrans('firebrick4', 100),
             addTrans('firebrick4', 200)))

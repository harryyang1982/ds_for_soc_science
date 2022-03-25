source("jhp.r")

x <- seq(-3, 3, length=100)
ggplot(data.frame(x)) +
  geom_line(aes(x = x, 
                y = exp(x)),
            color = "brown") +
  theme_jhp() +
  geom_vline(xintercept = 0) +
  geom_line(aes(x = x,
                y = x+1)) +
  annotate("text", label="y = exp(x)", x = x[90],
           y = exp(x[90]) - 0.5) +
  annotate("text", label="y = x + 1", x = x[80],
           y = x[80] - 0.5)


library(UsingR)
data(galton)

ggplot(galton, aes(x = parent, y = child)) +
  geom_point(size = 4, alpha = 0.1, color="brown") +
  xlab("부모의 중간 신장") + ylab("자녀의 신장") +
  labs(caption = "자료출처: UsingR package") +
  theme_jhp()

center <- function(x) {
  out <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(out)
}

galton.cen <- data.frame(apply(galton, 2, center))
rho.test <- cor.test(galton.cen[, 1], galton.cen[, 2])
rho.test

rho <- rho.test$estimate
galton.lm <- lm(child ~ parent, data = galton.cen)
stargazer(galton.lm, header=FALSE, type='latex',
          title = "골튼의 신장유전 자료에 대한 회귀분석: 자녀의 신장 ~ 부모의 중간 신장",
          label='galton.reg')

ggplot(galton.lm, aes(x = parent, y = child)) +
  geom_smooth(method = 'lm', aes(fill = 'confidence'),
              show.legend= FALSE, alpha = 0.2, color = "navy") +
  geom_point(size = 4, alpha = 0.1, color = "brown") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "navy", linetype = "dashed") +
  xlim(-3, 3) + ylim(-3, 3) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  xlab("부모의 평균신장(표준화)") + ylab("자녀의 신장(표준화)") +
  labs(caption = "자료출처: UsingR package") +
  theme_jhp()

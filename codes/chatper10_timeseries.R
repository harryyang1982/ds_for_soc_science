# 2. 정상 시계열 모델

source("jhp.R")

library(readxl)
world_pop <- read_excel("datasets/pop1800_2100.xlsx", sheet = 2)
world_pop

sub <- subset(world_pop, year < 2020)
sub$log.pop <- log(sub$Population)
sub$log.pop <- ts(sub$log.pop, start=sub$year[1])

library(forecast)
d.arima <- auto.arima(sub$log.pop)
summary(d.arima)

library(ggfortify)
require(ggthemes)

autoplot(sub$log.pop, size=2) +
  xlab("연도") + ylab("세계인구 (로그)") +
  labs(caption = "원자료 출처: Gapminder") +
  theme_jhp()

d.forecast <- forecast(d.arima, level = c(95), h = 50)
autoplot(d.forecast, size=2) +
  xlab("연도") + ylab("세계인구 (로그)") +
  labs(title = "Forecasts from ARIMA (3, 2, 2)", subtitle = "검은 실선은 로그 변환된 인구이며 청색 실선과 회색 영역은 예측결과",
       caption = "원자료 출처: Gapminder") +
  theme_jhp()

y2 <- subset(sub$log.pop, start=151)
d.arima2 <- auto.arima(y2)
summary(d.arima2)

d.forecast2 <- forecast(d.arima2, level = c(95), h = 50)
autoplot(d.forecast2, size = 2) + 
  xlab("연도") + ylab("세계인구 (로그)") +
  labs(title = "Forecasts from ARIMA (1, 2, 2)", subtitle = "검은 실선은 로그 변환된 인구이며 붉은 선은 선형회귀선, 청색 실선과 회색 영역은 예측결과",
       caption = "원자료 출처: Gapminder") +
  theme_jhp()

autoplot(diff(sub$log.pop, 2), ts.colour = "brown", size=2,
         cpt.colour = "firebrick3", cpt.linetype = "dashed") +
  xlab("연도") + ylab("인구") +
  labs(caption = "원자료 출처: Gapminder") +
  theme_jhp()

# 3. Bayesian 전환점 모형

## 3.1 고전파 통계학의 전환점 분석법

library(strucchange)
y <- diff(sub$log.pop, 2)
lagy <- stats::lag(y, k = - 1)
df.pop <- cbind(y, lagy)
df.pop <- window(df.pop, start = 1803, end = 2019)
colnames(df.pop) <- c("y", "lagy")

pop.model <- y ~ lagy
fs <- Fstats(pop.model, data = df.pop)
plot(fs)

set.seed(1)
out <- breakpoints(diff(sub$log.pop, 2) ~ 1)
summary(out)

break.dates <- paste(summary(out)$breakdates[nrow(summary(out)$breakdates), ], collapse=" ")
autoplot(out, ts.colour = "brown", size=2,
         cpt.color = "firebrick3", cpt.linetype = "dashed") +
  xlab("연도") + ylab("인구") +
  labs(subtitle=paste0("세계인구 (로그) 예측: 전환점 = ", break.dates),
       caption = "원자료 출처: Gapminder") +
  theme_jhp()


## 4절 은닉 마르코프 모형

### 4.1 HMM에 대한 통계적 추론
set.seed(1)
N <- 200
K <- 2
mu.state <- c(1, -1)
P <- cbind(c(0.9, 0.1), c(0.1, 0.9))
sd <- 1

## 은닉 상태변수를 생성
Z <- c(1, rep(NA, N-1))
for(t in 2:N){
  Z[t] <- sample(K, size=1, prob=P[Z[t-1],])
}
table(Z)

X <- rnorm(N, mean=mu.state[Z], sd=sd)

df.hmm <- data.frame(x = 1:length(Z),
                     state = Z, y = X, mu=mu.state[Z])
df.hmm$state <- as.factor(df.hmm$state)

ggplot(df.hmm, aes(x = x, y = y, color=state)) +
  geom_point(size=5, alpha=0.5) + 
  labs(color="은닉 상태변수") +
  theme_jhp()

#s1에 대한 사전 분포를 가정
pi <- c(0.5, 0.5)
# Pr(y_t | s_t=k)
emit <- function(k, x){
  dnorm(x, mean=mu.state[k], sd=sd)
}
alpha <- matrix(nrow = N, ncol = K)
#alpha[1, ]를 초기화
for(k in 1:K) {
  alpha[1, k] <- pi[k] * emit(k, X[1])
}
## Foward algorithm
for(t in 1:(N-1)){
  m <- alpha[t,]%*% P
  for(k in 1:K){
    alpha[t+1, k] <- m[k]*emit(k, X[t+1])
  }
}
head(alpha)

## 후진확률 계산식(FBA)
## Backwards algorithm
beta <- matrix(nrow = N, ncol = K)
# beta 초기화
for(k in 1:K){
  beta[N, k] <-  1
}
for(t in (N-1):1){
  for(k in 1:K){
    beta[t, k] <- sum(beta[t+1, ]*P[k, ]*emit(1:K, X[t+1]))
  }
}
head(beta)

ab <- alpha*beta
prob <- ab/rowSums(ab)
head(prob)

df.hmm$prop1 <- prob[,1]
df.hmm$prop2 <- prob[,2]
df.hmm$post.state <- as.factor(ifelse(df.hmm$prop1>0.5, 1, 2))
table(df.hmm$post.state)

head(df.hmm)

ggplot(df.hmm, aes(x=x, y=y, color=post.state)) +
  geom_point(size=5, alpha=0.5) +
  geom_point(aes(x = x, y = y, color=state), size=2, alpha=1) +
  labs(color="은닉 상태변수") +
  theme_jhp()

mean(df.hmm$state != df.hmm$post.state)

# 5절 비균일 은닉 마르코프 모형을 이용한 역사 연구

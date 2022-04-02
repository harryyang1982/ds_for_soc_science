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
  labs(subtitle = "검은 실선은 로그 변환된 인구이며 청색 실선과 회색 영역은 예측결과",
       caption = "원자료 출처: Gapminder") +
  theme_jhp()

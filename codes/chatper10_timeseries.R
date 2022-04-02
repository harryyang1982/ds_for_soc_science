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

autoplot(diff(sub$log.pop, 2), ts.color = "brown", size=2,
         cpt.color = "firebrick3", cpt.linetype = "dashed") +
  xlab("연도") + ylab("인구") +
  labs(caption = "원자료 출처: Gapminder") +
  theme_jhp()

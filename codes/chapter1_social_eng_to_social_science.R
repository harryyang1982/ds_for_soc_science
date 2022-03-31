# verfying Malthus Trap
library(tidyverse)
library(readxl)
source("jhp.R")

# 다운로드
download.file("http://gapm.io/dl_pop", destfil = "pop1800_2100.xlsx")

# df로 불러오기
world_pop <- read_excel("pop1800_2100.xlsx", sheet=2)
theme_set(theme_gray(base_family = 'AppleGothic'))

world_pop
# 객체 클래스 확인
class(world_pop)

head(world_pop)

world_pop %>% 
  filter(year < 2020) %>% 
  ggplot(aes(x = year, y = Population)) +
  geom_line(size=2, alpha=0.6, color="forestgreen") +
  labs(caption = "원자료: Gapminder") +
  theme_jhp(base_size = 10, base_family = "AppleGothic") +
  xlab("연도") + ylab("인구")

sub <- subset(world_pop, year < 2020)
sub$log.pop <- log(sub$Population)

sub
ggplot(sub, aes(x = year, y = log.pop)) +
  geom_line(size=2, alpha=0.8, color="forestgreen") +
  geom_smooth(method="lm", color="firebrick2") +
  labs(caption = "원자료: Gapminder") +
  theme_jhp(base_family = "AppleGothic") + xlab("연도") + ylab("인구 (log)")

sub$period <- ifelse(sub$year < 1951, "Before-1950", "Post-1950")
sub

ggplot(sub, aes(x = year, y = log.pop, group = period, color=period)) +
  geom_line(size = 2, alpha = 0.8, color = "forestgreen") +
  geom_smooth(method="lm", color="firebrick2") +
  labs(caption = "원자료: Gapminder") +
  theme_jhp(base_family = "AppleGothic") + xlab("연도") + ylab("인구 (log")

# Malthus Trap 2

uk_crop <- read_csv("cereal-yields-uk.csv") %>% 
  rename(Wheat = `Wheat (UK yields)`,
         Barley = `Barley (UK yields)`,
         Oats = `Oats (UK yields)`)

sub_crop <- subset(uk_crop, Year > 1799 & Year < 2018)

ggplot(sub_crop, aes(x = Year, y = Wheat)) +
  geom_line(size = 0.5, color="forestgreen") +
  geom_point(size=1.5, alpha=0.2, color="forestgreen") +
  labs(caption = "자료출처: Our World In Data") +
  theme_jhp(base_family = "AppleGothic") +
  xlab("연도") + ylab("헥타르당 톤")


# 1950년 이전과 이후를 구분하는 더미변수 생성
sub_crop$period <- ifelse(sub_crop$Year < 1951, "Before-1950", "Post-1950")

# x축과 y축 지정, 그룹 정보와 색 정보도 지정
ggplot(sub_crop, aes(x = Year, y = Wheat, group = period, color = period)) +
  geom_line(size=0.5, color="forestgreen") +
  geom_point(size=1.5, alpha=0.2, color="forestgreen") +
  geom_smooth(method="lm", color="firebrick2") +
  labs(caption = "자료출처: Our World In Data") +
  theme_jhp(base_family = "AppleGothic") + xlab("연도") + ylab("헥타르당 톤")

## mirror: function name
## x: argument
## NULL: default value
## return: output object

mirror <- function(x=NULL){
  return(x)
}

## test
mirror("cat")

mirror(1)

## function(x=NULL)이 아니고 function(x)라면
## 아래 코드는 에러가 발생하나 여기서는 NULL을 출력함
mirror()

## 정규화 함수 작성
center <- function(x=NULL){
  out <- (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  return(out)
}

malthus <- sub %>% left_join(sub_crop, by = c("year" = "Year", "period" = "period"))

malthus

## 2. 인구와 밀생산 자료를 표준화
malthus$center.pop <- center(malthus$Population)
malthus$center.wheat <- center(malthus$Wheat)

malthus

## 3. ggplot을 이용한 시각화
ggplot(malthus, aes(x = year, y = center.pop)) +
  geom_line(size=2, alpha=0.6, color="firebrick2") +
  geom_line(aes(x = year, y=center.wheat), size=0.5, color = "forestgreen") +
  geom_point(size=1, alpha=0.6, color="firebrick2") +
  geom_point(aes(x=year, y=center.wheat), size=1.5, alpha=0.2, color="forestgreen") +
  labs(caption = "자료출처: Gapminder, Our World In Data") +
  theme_jhp(base_family = "AppleGothic") + xlab("연도") + ylab("표준화 지수")


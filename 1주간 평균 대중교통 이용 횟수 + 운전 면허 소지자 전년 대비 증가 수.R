
library(tidyverse)
library(corrr)

# 데이터 불러오기
license_incr <- read_csv("license_incr.csv", locale = locale("ko", encoding = "EUC-KR"))
license_incr
public_transport <- read_csv("public_transport.csv", locale = locale("ko", encoding = "EUC-KR"))
# 그래프 창 크기 조정
options(repr.plot.width = 8, repr.plot.height = 8)

# 이상값 탐지
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
boxplot(license_incr$`전년 대비 증감`)
boxplot(license_incr$증감률)
boxplot(public_transport$`1주간 평균 대중교통 이용횟수(1회~5회)`)
boxplot(public_transport$`1주간 평균 대중교통 이용횟수(5회~10회)`)
boxplot(public_transport$`1주간 평균 대중교통 이용횟수(10회~15회)`)
boxplot(public_transport$`1주간 평균 대중교통 이용횟수(15회~20회)`)
boxplot(public_transport$`1주간 평균 대중교통 이용횟수(20회이상)`)

# 이상값 제거
pt_5to10 <- public_transport$`1주간 평균 대중교통 이용횟수(5회~10회)`
pt_5to10_iqr <- IQR(pt_5to10)
pt_5to10 <- ifelse(pt_5to10 > summary(pt_5to10)[5] + pt_5to10_iqr*1.5, NA, pt_5to10)
public_transport$`1주간 평균 대중교통 이용횟수(5회~10회)`<-pt_5to10
table(is.na(public_transport))

public_transport <- na.omit(public_transport)
table(is.na(public_transport))

public_transport


# license_incr에서 2015년부터 2021년까지의 데이터만 추출
license_growth <- license_incr %>%
  filter(연도 >= 2015 & 연도 <= 2021) %>%
  select(`전년 대비 증감`)

# public_transport 데이터셋에서 필요한 열만 선택
public_transport_subset <- public_transport %>%
  filter(연도 >= 2015 & 연도 <= 2021) %>%
  select(`1주간 평균 대중교통 이용횟수(1회~5회)`, `1주간 평균 대중교통 이용횟수(5회~10회)`,
         `1주간 평균 대중교통 이용횟수(10회~15회)`, `1주간 평균 대중교통 이용횟수(15회~20회)`,
         `1주간 평균 대중교통 이용횟수(20회이상)`)

# 상관관계 계산
cor_matrix <- cor(public_transport_subset, license_growth)
cor_matrix

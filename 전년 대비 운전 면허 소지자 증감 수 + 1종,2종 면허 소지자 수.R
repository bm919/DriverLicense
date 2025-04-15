library(tidyverse)
library(corrr)
install.packages("forecast")
library(forecast)

license_spe <- read_csv("license_spe.csv", locale=locale("ko", encoding="EUC-KR"), col_names=TRUE, na="")
license_spe

license_incr <- read_csv("license_incr.csv", locale=locale("ko", encoding="EUC-KR"), col_names=TRUE, na="")
license_incr


# 이상값 분석
boxplot(license_spe)
boxplot(license_incr)

# 상관계수 계산
correlation1 <- cor(license_incr$`전년 대비 증감`, license_spe$`1종`, use = "complete.obs")
correlation1
correlation2 <- cor(license_incr$`전년 대비 증감`, license_spe$`2종`, use = "complete.obs")
correlation2

# 상관계수 시각화
# 데이터프레임 생성
combined_data <- data.frame(
  전년_대비_증감 = license_incr$`전년 대비 증감`,
  종1 = license_spe$`1종`,
  종2 = license_spe$`2종`
)

# 상관행렬 계산
correlation_matrix <- combined_data %>%
  correlate()

# 상관계수 네트워크 시각화
correlation_matrix %>%
  network_plot(min_cor = 0, colours = c("red", "yellow", "blue"))


# 선형 회귀 모델 적합
lm_model_spe <- lm(`1종` ~ 연도, data = license_spe)
summary(lm_model_spe)

# 예측할 연도 생성
future_year <- 2023

# 예측할 `1종` 면허 수 계산
Predicted_1종 <- predict(lm_model_spe, newdata = data.frame(연도 = future_year))

# 예측 결과 출력
Predicted_1종

# 시각화 - 예측값 점 그래프로 표시
ggplot() +
  geom_point(data = license_spe, aes(x = 연도, y = `1종`), color = "blue") +
  geom_point(data = data.frame(연도 = future_year, Predicted_1종), aes(x = 연도, y = Predicted_1종), color = "red", size = 3) +
  labs(title = "2023년 1종 면허 소지자 수 예측", x = "연도", y = "1종 면허 소지자 수") +
  theme_minimal()


lm_model <- lm(license_incr$`전년 대비 증감` ~ license_spe$`1종`)
summary(lm_model)
predict2023 <- predict(lm_model, newdata = data.frame(`1종` = 34671))
predict2023
average_p <- mean(predict2023)
average_p


# license_incr 데이터 프레임에서 연도와 전년 대비 증감 열을 선택하여 새로운 데이터 프레임 생성
new <- data.frame(연도 = license_incr$연도, `전년 대비 증감` = license_incr$`전년 대비 증감`)

# 새로운 데이터 프레임에 2023년의 평균 예측값을 추가
new <- rbind(new, data.frame(연도 = 2023, `전년 대비 증감` = average_p))
new

# ggplot을 사용하여 그래프 생성
library(ggplot2)
ggplot(new, aes(x = `연도`, y = `전년.대비.증감`)) +
  geom_point(color = "red", size = 3, na.rm = TRUE) +
  labs(title = "연도별 면허소지자 증감", x = "연도", y = "전년 대비 증감") +
  theme_minimal()


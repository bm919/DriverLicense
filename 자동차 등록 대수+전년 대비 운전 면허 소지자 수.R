library(tidyverse)
library(ggplot2)
library(corrr)

# 귀무가설 : 연도별 자동차 등록 증가 대수는 운전면허소지자 전년 대비 증감 수수에 영향을 미치지 않는다.
# 대립가설 : 연도별 자동차 등록 증가 대수는 운전면허소지자 전년 대비 증감 수에 영향을 미친다.

# 유의수준 : 0.05

license_incr <- read_csv("license_incr.csv", locale=locale("ko", encoding="EUC-KR"), col_names = TRUE, na="")
license_incr
carRegist <- read_csv("carRegist.csv", locale=locale("ko", encoding="EUC-KR"), col_names = TRUE, na="")
carRegist

# 이상값 분석
boxplot(license_incr$'전년 대비 증감')
boxplot(carRegist$'전년대비 증가대수(천대)')

merged_data <- merge(license_incr, carRegist, by = "연도")

# 상관계수 계산
correlation <- cor(merged_data$`전년 대비 증감`, merged_data$`전년대비 증가대수(천대)`, use = "complete.obs")
print(paste("상관계수: ", correlation))

cor_matrix <- merged_data %>%
  select(`전년 대비 증감`, `전년대비 증가대수(천대)`) %>%
  correlate()
cor_matrix %>%
  rplot() +
  ggtitle("상관계수 시각화") +
  theme_minimal()

# 상관계수에 대한 통계적 검정
cor_test <- cor.test(merged_data$`전년 대비 증감`, merged_data$`전년대비 증가대수(천대)`)
print(cor_test)

# 선형 회귀 모델 구축
model <- lm(`전년 대비 증감` ~ `전년대비 증가대수(천대)`, data = merged_data)

# 모델 요약
summary(model)

# 회귀선 시각화
ggplot(merged_data, aes(x = `전년대비 증가대수(천대)`, y = `전년 대비 증감`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "자동차 등록 대수 증가에 따른 운전면허 소지자 증가 예측",
       x = "전년대비 자동차 등록 증가 대수(천대)",
       y = "전년 대비 운전면허 소지자 증가율") +
  theme_minimal()

new_data <- data.frame(`전년대비 증가대수(천대)` = c(1000, 2000, 3000))  # 예측할 새로운 데이터
colnames(new_data) <- c("전년대비 증가대수(천대)")  # 열 이름 설정

predictions <- predict(model, newdata = new_data)
new_data$`전년 대비 증감` <- predictions
print(new_data)


# 전년 대비 운전면허 소지자 증감 수와 전년대비 자동차 등록 증가 대수는 관련이 있으며
# 자동차 등록 대수가 증가함에 따라 전년 대비 운전면허 소지자 수가 증가한다.
# 1000, 2000,3000대 증가할 경우 운전 면허 소지자는 879834, 1788002, 2696171명 증가할 것이라고 예측된다.


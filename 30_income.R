View(PS_data)

# 2022년과 2023년 기준 30대의 출생년도 범위를 계산합니다
start_year_30s_2022 <- 2022 - 39  # 30세에서 39세
end_year_30s_2022 <- 2022 - 30

start_year_30s_2023 <- 2023 - 39  # 30세에서 39세
end_year_30s_2023 <- 2023 - 30

# 2022년 기준 30대에 해당하는 데이터를 필터링합니다
data_30s_2022 <- subset(PS_data, X0002 >= start_year_30s_2022 & X0002 <= end_year_30s_2022)

# 2023년 기준 30대에 해당하는 데이터를 필터링합니다
data_30s_2023 <- subset(PS_data, X0002 >= start_year_30s_2023 & X0002 <= end_year_30s_2023)

View(data_30s_2022)
View(data_30s_2023)

library(dplyr)
Y_data_30s_2022 <- data_30s_2022 %>% select(starts_with("Y"))
Y_data_30s_2023 <- data_30s_2023 %>% select(starts_with("Y"))

View(Y_data_30s_2023)

# 가구원수가 1인 행들만 추출
filtered_data_single_member <- Y_data_30s_2023[Y_data_30s_2023$Y0001 == 1, ]

View(filtered_data_single_member)


# 필요한 패키지 로드
library(ggplot2)


# 소득 구간 목록
income_labels <- c("~1천", "1천~3천", "3천~5천", "5천~7천", "7천~1억", "1억~")

# 소득 구간별 카운트 계산
income_counts <- table(filtered_data_single_member$Y0008)

# 데이터프레임으로 변환
income_counts_df <- data.frame(
  income = factor(names(income_counts), levels = 1:6, labels = income_labels),
  count = as.numeric(income_counts)
)

# 소득 구간 분포 그래프 그리기
ggplot(income_counts_df, aes(x = income, y = count)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "30대 가구원수 1명인 가구의 연간 소득 분포",
       x = "연간 소득 구간", y = "응답 수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

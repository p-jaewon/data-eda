library(dplyr)

# CSV 파일 경로
file_paths <- c("C:/R/pay_data1.csv", "C:/R/pay_data2.csv", "C:/R/pay_data3.csv")


# 실제 공통된 열 이름으로 변경
common_colnames <- c("PANEL_ID", "SMS_REGISTRATION_DATE", "SMS_REGISTRATION_TIME",	
                     "APPROVAL_PRICE", "APPROVAL_STORE", "APPROVAL_TYPE", "CARD_PAYMENT_TYPE",
                     "BRANDNAME_N", "CATE_LEVEL3", "COMPANY_NAME", "APPROVAL_METHOD",	
                     "APPROVAL_UNIT", "APPROVAL_REAL_PRICE", "NUM")

# 각 파일을 읽고 열 이름을 맞춘 후 병합
Pay_data_list <- lapply(file_paths, function(file) {
  df <- read.csv(file)
  # 공통 열 이름으로 변경
  colnames(df) <- common_colnames
  return(df)
})

Pay_data <- do.call(rbind, Pay_data_list)

# 결과 데이터 프레임 확인
View(Pay_data)

# Ps데이터에서 다시 30대만 추출
start_year_30s_2023 <- 2023 - 39  # 30세에서 39세
end_year_30s_2023 <- 2023 - 30

data_2023_30 <- subset(PS_data, X0002 >= start_year_30s_2023 & X0002 <= end_year_30s_2023)
View(data_2023_30)

# 패널 ID가 일치하는 행만 Pay_data에서 추출
merged_data <- semi_join(Pay_data, data_2023_30, by = "PANEL_ID")

# 데이터 확인
View(merged_data)

# 2023으로 시작하는 SMS_REGISTRATION_DATE 값을 가진 행만 추출
pay_data_2023 <- merged_data %>% filter(grepl("^2023", SMS_REGISTRATION_DATE))

# 데이터 확인
View(pay_data_2023)


# CATE_LEVEL3 열에서 "NULL" 값을 가진 행 제거
pay_data_2023 <- filtered_data %>% filter(CATE_LEVEL3 != "NULL")

# 데이터 확인
View(pay_data_2023)

# CATE_LEVEL3 열에서 가장 빈도가 높은 상위 5개 항목 찾기
top5_cate_level3 <- pay_data_2023 %>%
  count(CATE_LEVEL3 != "NULL") %>%
  arrange(desc(n)) %>%
  head(5)

# COMPANY_NAME 열에서 가장 빈도가 높은 상위 5개 항목 찾기
top5_company_name <- pay_data_2023 %>%
  filter(COMPANY_NAME != "NULL") %>%  # "NULL" 값을 제외
  count(COMPANY_NAME) %>%
  arrange(desc(n)) %>%
  head(5)

# CATE_LEVEL3 시각화
ggplot(top5_cate_level3, aes(x = reorder(CATE_LEVEL3, n), y = n)) +
  geom_bar(stat = "identity", fill = "aquamarine") +
  labs(title = "30대 주요 결제 카테고리(상위 5개) ", x = "결제 카테고리", y = "빈도수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# COMPANY_NAME 시각화
ggplot(top5_company_name, aes(x = reorder(COMPANY_NAME, n), y = n)) +
  geom_bar(stat = "identity", fill = "gold") +
  labs(title = "30대 이용 카드사(상위 5개)", x = "카드사", y = "빈도수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



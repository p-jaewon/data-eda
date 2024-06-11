View(filtered_data_30s_2023)

# 필요한 라이브러리 로드
library(ggplot2)

# 필요한 칼럼 추출
ott_columns <- c("G0017", "G0018", "G0019", "G0020", "G0021", 
                 "G0022", "G0023", "G0024", "G0025", "G0026", "G0043")

ott_data <- filtered_data_30s_2023[, ott_columns]

# 각 칼럼에서 1의 개수 세기
yes_counts <- colSums(ott_data == 1, na.rm = TRUE)

# OTT 플랫폼 이름 목록
ott_labels <- c(
  "넷플릭스(Netflix)", "웨이브(Wavve)", "왓챠(Watcha)", "티빙(TVING)", 
  "네이버 시리즈(SERIES)", "유튜브 프리미엄(유튜브 레드)", "시즌(Seezn)", 
  "U+ 모바일 TV", "카카오TV", "디즈니플러스", "쿠팡플레이"
)

# 데이터프레임으로 변환환
yes_counts_df <- data.frame(
  platform = ott_labels,
  count = yes_counts
)
  

# 상위 5개 항목 선택
top5_ott <- yes_counts_df[order(-yes_counts_df$count), ][1:5, ]

# 그래프 그리기
ggplot(top5_ott, aes(x = reorder(platform, count), y = count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "30대 OTT 플랫폼 이용 분포 (상위 5개)",
       x = "OTT 플랫폼", y = "응답 수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

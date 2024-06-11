# 필요한 라이브러리 로드
library(ggplot2)

# 미디어 동영상 콘텐츠 항목 목록
media_content_labels <- c(
  "드라마/영화", "음악/댄스", "푸드/먹방/레시피", "뉴스/시사/정치", 
  "게임", "스포츠경기 중계/다시보기", "예능/코미디/웃긴 동영상", 
  "연예계 소식/연예인/가십", "여행", "뷰티(메이크업 등)", 
  "동물", "운동/헬스/요가/홈트레이닝", "학습/강의/업무/사용법/배우기", 
  "일상 및 토크(Vlog)"
)

media_content_data <- filtered_data_30s_2023[, paste0("G00", 29:42)]

View(filtered_data_30s_2023)

# 각 칼럼에서 1의 개수 세기
yes_counts <- colSums(media_content_data == 1, na.rm= TRUE)

# 데이터프레임으로 변환
yes_counts_df <- data.frame(
  content = media_content_labels,
  count = yes_counts
)

# 상위 5개 항목 선택
top5_yes_counts_df <- yes_counts_df[order(-yes_counts_df$count), ][1:5, ]

# 그래프 그리기
ggplot(top5_yes_counts_df, aes(x = reorder(content, count), y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "30대 미디어 동영상 콘텐츠 수요 (상위 5개)",
       x = "콘텐츠", y = "응답 수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


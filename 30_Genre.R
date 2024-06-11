# 필요한 라이브러리 로드
library(dplyr)
library(ggplot2)

# 데이터 로드
PS_data <- read.csv("C:/R/PS data.csv")

# 2023년 기준 30대의 출생년도 범위를 계산
start_year_30s_2023 <- 2023 - 39  # 30세에서 39세
end_year_30s_2023 <- 2023 - 30

# 2023년 기준 30대에 해당하는 데이터를 필터링
PS_30 <- subset(PS_data, X0002 >= start_year_30s_2023 & X0002 <= end_year_30s_2023)

PS_30 <- PS_30 %>% select(starts_with("G"))

# 확인하려는 칼럼의 0-"응답없음" 값을 제거
PS_30<- subset(PS_30, G0001 !=0 & G0004 != 0 & G0007 != 0)

View(PS_30)

# 영화 데이터의 장르 데이터 카운트
genre_counts <- table(PS_30$G0001)

# 영화 데이터의 상위 5개 장르 선택
top5_genres <- names(sort(genre_counts, decreasing = TRUE))[1:5]

# 영화 데이터의 데이터프레임으로 변환
top5_genre_counts <- data.frame(
  genre = as.numeric(top5_genres),
  count = as.numeric(genre_counts[top5_genres])
)

# 영화 데이터의 장르 코드를 실제 장르명으로 변환하는 함수
genre_code_to_name <- function(code) {
  genres <- c("액션", "코미디", "로맨스(멜로)", "스릴러", "공포(호러)", 
              "공상과학(SF)", "판타지", "드라마", "범죄", "다큐멘터리", 
              "음악/뮤지컬", "역사", "자연과학", "기타")
  return(genres[code])
}

# 영화 데이터의 장르 코드를 실제 장르명으로 변환
top5_genre_counts$genre <- sapply(top5_genre_counts$genre, genre_code_to_name)

# 영화 데이터의 그래프 그리기
ggplot(top5_genre_counts, aes(x = reorder(genre, count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "30대 영화 장르 선호도 분포 (상위 5개)",
       x = "장르", y = "응답 수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# TV 데이터의 장르 데이터 카운트
genre_counts2 <- table(PS_30$G0004)

# TV 데이터의 상위 5개 장르 선택
top5_genres2 <- names(sort(genre_counts2, decreasing = TRUE))[1:5]

# TV 데이터의 데이터프레임으로 변환
top5_genre_counts2 <- data.frame(
  genre = as.numeric(top5_genres2),
  count = as.numeric(genre_counts2[top5_genres2])
)

# TV 데이터의 장르 코드를 실제 장르명으로 변환하는 함수
genre_code_to_name2 <- function(code) {
  genres <- c("버라이어티/예능", "드라마", "뉴스", "스포츠", 
              "취미/레저", "음악", "교육", "시사/다큐", "교양/정보", 
              "홈쇼핑", "성인", "기타")
  return(genres[code])
}

# TV 데이터의 장르 코드를 실제 장르명으로 변환
top5_genre_counts2$genre <- sapply(top5_genre_counts2$genre, genre_code_to_name2)

# TV 데이터의 그래프 그리기
ggplot(top5_genre_counts2, aes(x = reorder(genre, count), y = count)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "30대 TV 프로그램 장르 선호도 분포 (상위 5개)",
       x = "장르", y = "응답 수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# G0007에서 0값 제거 (타 카테고리에 비해 0-'응답 없음' 상대적으로 많기 때문)
PS_30<- subset(PS_30)

# 라디오 장르 데이터 카운트
genre_counts3 <- table(PS_30$G0007)

# 라디오 데이터의 상위 5개 장르 선택
top5_genres3 <- names(sort(genre_counts3, decreasing = TRUE))[1:5]

# 상위 5개 장르의 카운트 값을 확인합니다
top5_counts3 <- as.numeric(genre_counts3[top5_genres3])

# 라디오 데이터의 데이터프레임으로 변환
top5_genre_counts3 <- data.frame(
  genre = as.numeric(top5_genres3),
  count = top5_counts3
)

# 라디오 장르 코드를 실제 장르명으로 변환하는 함수
genre_code_to_name3 <- function(code) {
  genres <- c("시사", "가요/POP", "뉴스", "생활정보", "교양", "클래식", 
              "성인가요", "청취자 사연", "기타")
  return(genres[code])
}

# 라디오 장르 코드를 실제 장르명으로 변환
top5_genre_counts3$genre <- sapply(top5_genre_counts3$genre, genre_code_to_name3)


# 라디오 데이터의 그래프 그리기
ggplot(top5_genre_counts3, aes(x = reorder(genre, count), y = count)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(title = "30대 라디오 장르 선호도 분포 (상위 5개)",
       x = "장르", y = "응답 수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

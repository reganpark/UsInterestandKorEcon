library(dplyr)
library(quantmod)
library(rugarch)
library(rmgarch)

# 데이터 불러오기
stock_market_data <- read.csv("C:/Users/Admin/Downloads/프로젝트데이터_외국인보유수_포함.csv",stringsAsFactors = FALSE)

# 날짜 형식 변환 (필요시)
stock_market_data$Date <- as.Date(stock_market_data$Date, format="%Y-%m-%d")

# 필요한 컬럼만 추출
financial_data <- stock_market_data[, c("Date", "kospi", "kosdaq", "TOTAL_foreign_own","KOSPI_foreign_own", 
                                        "KOSDAQ_foreign_own", "usdkrw")]

# 결측값 제거
financial_data <- na.omit(financial_data)

# 백분율 변화율 계산
financial_data <- financial_data %>%
  arrange(Date) %>%
  mutate(
    kospi_pct_change = (kospi - lag(kospi)) / lag(kospi) * 100,
    kosdaq_pct_change = (kosdaq - lag(kosdaq)) / lag(kosdaq) * 100,
    TOTAL_foreign_own_pct_change = (TOTAL_foreign_own - lag(TOTAL_foreign_own)) / lag(TOTAL_foreign_own) * 100,
    KOSPI_foreign_own_pct_change = (KOSPI_foreign_own - lag(KOSPI_foreign_own)) / lag(KOSPI_foreign_own) * 100,
    KOSDAQ_foreign_own_pct_change = (KOSDAQ_foreign_own - lag(KOSDAQ_foreign_own)) / lag(KOSDAQ_foreign_own) * 100,
    usdkrw_pct_change = (usdkrw - lag(usdkrw)) / lag(usdkrw) * 100
  )

# 백분율 변화율 데이터 확인
head(financial_data)

str(financial_data)

# lag로 인해 발생한 결측값 제거
financial_data <- na.omit(financial_data)


# 백분율 변화율만 추출
financial_pct_changes <- financial_data[, c("kospi_pct_change","kosdaq_pct_change","TOTAL_foreign_own_pct_change", 
                                          "KOSPI_foreign_own_pct_change", "KOSDAQ_foreign_own_pct_change", 
                                          "usdkrw_pct_change")]




#"kospi_pct_change", "kosdaq_pct_change", "TOTAL_foreign_own_pct_change", 
#"usdkrw_pct_change", "us_tb_3m_pct_change"






str(financial_pct_changes)
head(financial_pct_changes)

# 백분율 변화율 데이터를 매트릭스로 변환
financial_matrix <- as.matrix(financial_pct_changes)

str(financial_matrix)


str(financial_matrix)
sum(is.na(financial_matrix))

# 개별 GARCH(1,1) 모델 사양 정의
garch11.spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

# 각 시계열에 대해 GARCH 모델 피팅
garch_fits <- lapply(1:ncol(financial_matrix), function(i) {
  ugarchfit(spec = garch11.spec, data = financial_matrix[, i])
})

# 개별 GARCH 피팅 결과 확인
lapply(garch_fits, function(fit) print(fit@fit$coef))

# DCC-GARCH 모델 사양 정의
dcc.spec <- dccspec(
  uspec = multispec(replicate(ncol(financial_matrix), garch11.spec)),
  dccOrder = c(1, 1),
  distribution = "mvnorm"
)

# DCC-GARCH 모델 피팅
dcc.fit <- dccfit(dcc.spec, data = financial_matrix)

# 결과 출력
print(dcc.fit)






# 데이터 요약
summary(financial_matrix)

# 각 시계열의 분포 시각화
par(mfrow = c(3, 2))
for (i in 1:ncol(financial_matrix)) {
  hist(financial_matrix[, i], main = colnames(financial_matrix)[i], breaks = 50)
}



# GARCH(1,1) 모델 사양 정의
garch11.spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0))
)


# DCC-GARCH(1,1) 모델 사양 정의
dcc.spec <- dccspec(
  uspec = multispec(replicate(ncol(financial_matrix), garch11.spec)),
  dccOrder = c(1, 1),
  distribution = "mvnorm"
)

# 모델 피팅
dcc.fit <- dccfit(dcc.spec, data = financial_matrix)

# 피팅 결과 확인
print(dcc.fit)

# 조건부 상관 행렬 확인
rcor(dcc.fit)



# 필요한 패키지 설치 및 로드
# 필요한 패키지 설치 및 로드
install.packages("rmgarch")
install.packages("ggplot2")

library(rmgarch)
library(ggplot2)

# DCC-GARCH 모델 피팅 결과에서 조건부 상관 행렬 추출
rcor_matrix <- rcor(dcc.fit)

# 예시: KOSPI와 USD/KRW의 상관 관계 시계열 추출
kospi_usdkrw_corr <- rcor_matrix[1, 6, ]




str(financial_data$Date)

#financial_data의 날짜 열을 사용하여 데이터 프레임 생성
correlation_data <- data.frame(
  date = financial_data$Date,  # 변화율을 계산한 후 데이터가 줄어들었기 때문에 첫 번째 행을 제거
  correlation = kospi_usdkrw_corr
)

# ggplot2를 사용한 시각화
ggplot(correlation_data, aes(x = date, y = correlation)) +
  geom_line(color = "blue") +
  labs(title = "KOSPI와 USD/KRW 간의 동적 조건부 상관 관계",
       x = "날짜",
       y = "상관 관계") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x축 레이블 회전


rcor(dcc.fit)

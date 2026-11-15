library(tidyverse)
library(lubridate)
library(here)

# 1. NOAA teleconnection index 읽기
read_index <- function(path, value_name) {
  read.table(
    here(path),
    header = TRUE,
    stringsAsFactors = FALSE
  ) %>%
    rename(
      year = YEAR,
      month = MONTH,
      !!value_name := INDEX
    ) %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month)
    )
}

ao <- read_index("data/raw/ao.txt", "AO")
nao <- read_index("data/raw/nao.txt", "NAO")
wp <- read_index("data/raw/wp.txt", "WP")
eawr <- read_index("data/raw/eawr.txt", "EAWR")

# 확인
head(ao)
head(nao)
head(wp)
head(eawr)

# 2. NOAA 지수 하나로 합치기
tele <- ao %>%
  full_join(nao, by = c("year", "month")) %>%
  full_join(wp, by = c("year", "month")) %>%
  full_join(eawr, by = c("year", "month")) %>%
  arrange(year, month)

# 확인
head(tele)
tail(tele)
glimpse(tele)


# 3. NOAA 데이터 품질 확인

# 전체 행 수
nrow(tele)

# 결측치 개수
colSums(is.na(tele))

# 전체 기간 확인
tele %>%
  summarise(
    start_year = min(year, na.rm = TRUE),
    end_year = max(year, na.rm = TRUE),
    n_rows = n()
  )

# 각 지수별 유효 기간 확인
tele %>%
  summarise(
    AO_start = min(year[!is.na(AO)]),
    AO_end = max(year[!is.na(AO)]),
    
    NAO_start = min(year[!is.na(NAO)]),
    NAO_end = max(year[!is.na(NAO)]),
    
    WP_start = min(year[!is.na(WP)]),
    WP_end = max(year[!is.na(WP)]),
    
    EAWR_start = min(year[!is.na(EAWR)]),
    EAWR_end = max(year[!is.na(EAWR)])
  )

# 4. year-month 중복 확인

tele %>%
  count(year, month) %>%
  filter(n > 1)


write_csv(
  tele,
  here("data", "processed", "teleconnection_monthly.csv")
)

# NOAA 원격상관 지수에서 먼저 lag 생성 -----------------------------

tele_lagged <- tele %>%
  arrange(year, month) %>%
  mutate(
    AO_lag1   = lag(AO, 1),
    AO_lag2   = lag(AO, 2),
    AO_lag3   = lag(AO, 3),
    
    NAO_lag1  = lag(NAO, 1),
    NAO_lag2  = lag(NAO, 2),
    NAO_lag3  = lag(NAO, 3),
    
    WP_lag1   = lag(WP, 1),
    WP_lag2   = lag(WP, 2),
    WP_lag3   = lag(WP, 3),
    
    EAWR_lag1 = lag(EAWR, 1),
    EAWR_lag2 = lag(EAWR, 2),
    EAWR_lag3 = lag(EAWR, 3)
  )

#=========================
#KMA 서울 월자료 읽기

kma_raw <- read_csv(
  here("data", "raw", "kma_seoul_monthly.csv"),
  locale = locale(encoding = "EUC-KR"),
  show_col_types = FALSE
)

# 컬럼 확인
names(kma_raw)

# 앞부분 확인
head(kma_raw)

# 구조 확인
glimpse(kma_raw)

# KMA 파일의 빈 행 제거
kma_raw <- kma_raw %>%
  filter(!is.na(일시))

# 확인
nrow(kma_raw)
tail(kma_raw)


# 8. KMA 데이터 범위와 중복 확인 -----------------------------------

# 일시 값의 처음/마지막 확인
head(kma_raw$일시, 10)
tail(kma_raw$일시, 10)

# 지점이 서울(108)만 있는지 확인
unique(kma_raw$지점)
unique(kma_raw$지점명)

# 동일한 일시가 중복되어 있는지 확인
kma_raw %>%
  count(일시) %>%
  filter(n > 1)

# 9. KMA 날짜 파싱 --------------------------------------------------
month_lookup <- c(
  Jan = 1,
  Feb = 2,
  Mar = 3,
  Apr = 4,
  May = 5,
  Jun = 6,
  Jul = 7,
  Aug = 8,
  Sep = 9,
  Oct = 10,
  Nov = 11,
  Dec = 12
)

kma <- kma_raw %>%
  separate(
    일시,
    into = c("month_chr", "year_2digit"),
    sep = "-",
    remove = FALSE
  ) %>%
  mutate(
    year_2digit = as.integer(year_2digit),
    
    # 00~26 → 2000년대
    # 27~99 → 1900년대
    year = if_else(
      year_2digit <= 26,
      2000L + year_2digit,
      1900L + year_2digit
    ),
    
    month = unname(month_lookup[month_chr])
  ) %>%
  transmute(
    station = 지점,
    station_name = 지점명,
    year,
    month,
    temp_mean = `평균기온(°C)`,
    temp_max = `평균최고기온(°C)`,
    temp_min = `평균최저기온(°C)`
  ) %>%
  arrange(year, month)

head(kma)
tail(kma)
glimpse(kma)

# 10. KMA 정리 결과 검증 -------------------------------------------

kma %>%
  summarise(
    start_year = min(year, na.rm = TRUE),
    end_year   = max(year, na.rm = TRUE),
    n_rows     = n()
  )

sort(unique(kma$month))

colSums(is.na(kma))

kma %>%
  count(year, month) %>%
  filter(n > 1)


# 11. KMA + NOAA 월자료 병합 ---------------------------------------

monthly_master <- kma %>%
  left_join(
    tele_lagged,
    by = c("year", "month")
  ) %>%
  arrange(year, month)

glimpse(monthly_master)
head(monthly_master)
tail(monthly_master)

# 결측 확인
colSums(is.na(monthly_master))

write_csv(
  monthly_master,
  here("data", "processed", "monthly_master.csv")
)

# 14. 1월 분석용 데이터 생성 ---------------------------------------

january <- monthly_master %>%
  filter(month == 1) %>%
  transmute(
    year,
    
    jan_temp_raw = temp_mean,
    jan_temp_max = temp_max,
    jan_temp_min = temp_min,
    
    AO_lag0 = AO,
    AO_lag1,
    AO_lag2,
    AO_lag3,
    
    NAO_lag0 = NAO,
    NAO_lag1,
    NAO_lag2,
    NAO_lag3,
    
    WP_lag0 = WP,
    WP_lag1,
    WP_lag2,
    WP_lag3,
    
    EAWR_lag0 = EAWR,
    EAWR_lag1,
    EAWR_lag2,
    EAWR_lag3
  )

# 15. 서울 1월 평균기온 detrending ----------------------------------

trend_model <- lm(
  jan_temp_raw ~ year,
  data = january
)

summary(trend_model)

january <- january %>%
  mutate(
    jan_temp_trend = fitted(trend_model),
    jan_temp_detrended = resid(trend_model)
  )

# 16. 최종 분석 데이터 검증 ----------------------------------------

glimpse(january)

summary(january)

colSums(is.na(january))

head(january)
tail(january)
nrow(january)

# 17. 최종 분석 데이터 저장 ----------------------------------------

write_csv(
  january,
  here("data", "processed", "january_master.csv")
)

library(tidyverse)
library(here)

monthly <- read_csv(
  here("data", "processed", "monthly_master.csv"),
  show_col_types = FALSE
)

prelim <- monthly %>%
  filter(month %in% c(1, 7)) %>%
  mutate(
    season_month = if_else(month == 1, "January", "July")
  )

cor_results <- prelim %>%
  group_by(season_month) %>%
  summarise(
    AO = cor(temp_mean, AO, use = "complete.obs"),
    NAO = cor(temp_mean, NAO, use = "complete.obs"),
    WP = cor(temp_mean, WP, use = "complete.obs"),
    EAWR = cor(temp_mean, EAWR, use = "complete.obs")
  )

cor_results

cor_long <- cor_results %>%
  pivot_longer(
    cols = -season_month,
    names_to = "index",
    values_to = "correlation"
  )

write_csv(
  cor_long,
  here("results", "tables", "january_vs_july_correlations.csv")
)

# 1월 detrended temperature와 각 지수의 상관

january_detrended_cor <- january %>%
  summarise(
    AO = cor(jan_temp_detrended, AO_lag0, use = "complete.obs"),
    NAO = cor(jan_temp_detrended, NAO_lag0, use = "complete.obs"),
    WP = cor(jan_temp_detrended, WP_lag0, use = "complete.obs"),
    EAWR = cor(jan_temp_detrended, EAWR_lag0, use = "complete.obs")
  )

january_detrended_cor

# raw January / July correlation 결과를 long format으로 변환
cor_long <- cor_results %>%
  pivot_longer(
    cols = -season_month,
    names_to = "index",
    values_to = "correlation"
  ) %>%
  rename(
    analysis = season_month
  )

# detrended January 결과를 long format으로 변환
detrended_long <- january_detrended_cor %>%
  pivot_longer(
    cols = everything(),
    names_to = "index",
    values_to = "correlation"
  ) %>%
  mutate(
    analysis = "January_detrended"
  ) %>%
  select(
    analysis,
    index,
    correlation
  )

# 최종 예비분석 표
preliminary_cor_table <- bind_rows(
  cor_long,
  detrended_long
)

preliminary_cor_table

write_csv(
  preliminary_cor_table,
  here(
    "results",
    "tables",
    "preliminary_january_july_correlations.csv"
  )
)

preliminary_cor_wide <- preliminary_cor_table %>%
  pivot_wider(
    names_from = analysis,
    values_from = correlation
  ) %>%
  select(
    index,
    January,
    January_detrended,
    July
  )

write_csv(
  preliminary_cor_wide,
  here(
    "results",
    "tables",
    "preliminary_january_july_correlations_wide.csv"
  )
)

preliminary_cor_wide

# 예비분석에서 서울의 1월과 7월 평균기온을 주요 원격상관 지수와 비교한 결과, 1월에서 전반적으로 더 강한 상관관계가 나타났다. 또한 서울 1월 평균기온의 장기 선형추세를 제거한 이후에도 AO, NAO, WP, EA/WR과의 상관계수는 각각 0.505, 0.283, 0.327, 0.369로 양의 관계가 유지되었다. 이에 따라 본 연구에서는 원격상관 패턴과의 관계가 보다 뚜렷하게 나타나는 서울 1월 평균기온을 주요 분석 대상으로 설정하였다.

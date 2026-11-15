library(tidyverse)
library(here)
library(slider)

january <- read_csv(
  here("data", "processed", "january_master.csv"),
  show_col_types = FALSE
)

rolling_cor <- function(data, predictor, window = 30) {
  
  data %>%
    arrange(year) %>%
    mutate(
      rolling_cor = slide_dbl(
        .x = seq_len(n()),
        .f = ~ {
          idx <- .x
          start_idx <- max(1, idx - window + 1)
          
          if ((idx - start_idx + 1) < window) {
            return(NA_real_)
          }
          
          cor(
            data$jan_temp_detrended[start_idx:idx],
            data[[predictor]][start_idx:idx],
            use = "complete.obs"
          )
        },
        .before = 0
      )
    ) %>%
    transmute(
      year,
      predictor,
      rolling_cor,
      window
    )
}

primary_vars <- c(
  "AO_lag0",
  "WP_lag0",
  "EAWR_lag1"
)

rolling30 <- map_dfr(
  primary_vars,
  ~ rolling_cor(
    january,
    .x,
    window = 30
  )
)

rolling30

p_rolling30 <- rolling30 %>%
  filter(!is.na(rolling_cor)) %>%
  ggplot(
    aes(
      x = year,
      y = rolling_cor,
      linetype = predictor
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "30-Year Rolling Correlations",
    subtitle = "Detrended January mean temperature in Seoul",
    x = "Window end year",
    y = "Rolling correlation",
    linetype = "Predictor"
  ) +
  theme_minimal()

p_rolling30

ggsave(
  here(
    "results",
    "figures",
    "rolling_correlation_30year_primary.png"
  ),
  p_rolling30,
  width = 8,
  height = 5,
  dpi = 300
)

rolling20 <- map_dfr(
  primary_vars,
  ~ rolling_cor(
    january,
    .x,
    window = 20
  )
)

p_rolling20 <- rolling20 %>%
  filter(!is.na(rolling_cor)) %>%
  ggplot(
    aes(
      x = year,
      y = rolling_cor,
      linetype = predictor
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "20-Year Rolling Correlations",
    subtitle = "Sensitivity analysis",
    x = "Window end year",
    y = "Rolling correlation",
    linetype = "Predictor"
  ) +
  theme_minimal()

p_rolling20


ggsave(
  here(
    "results",
    "figures",
    "rolling_correlation_20year_primary.png"
  ),
  p_rolling20,
  width = 8,
  height = 5,
  dpi = 300
)

rolling_summary <- rolling30 %>%
  filter(!is.na(rolling_cor)) %>%
  group_by(predictor) %>%
  summarise(
    mean_rolling_cor = mean(rolling_cor),
    sd_rolling_cor = sd(rolling_cor),
    min_rolling_cor = min(rolling_cor),
    max_rolling_cor = max(rolling_cor),
    positive_ratio = mean(rolling_cor > 0),
    .groups = "drop"
  )

rolling_summary
write_csv(
  rolling_summary,
  here(
    "results",
    "tables",
    "rolling_correlation_summary_30year.csv"
  )
)


rolling_summary20 <- rolling20 %>%
  filter(!is.na(rolling_cor)) %>%
  group_by(predictor) %>%
  summarise(
    mean_rolling_cor = mean(rolling_cor),
    sd_rolling_cor = sd(rolling_cor),
    min_rolling_cor = min(rolling_cor),
    max_rolling_cor = max(rolling_cor),
    positive_ratio = mean(rolling_cor > 0),
    .groups = "drop"
  )

write_csv(
  rolling_summary20,
  here(
    "results",
    "tables",
    "rolling_correlation_summary_20year.csv"
  )
)
rolling_summary20


#30년 이동창 분석 결과, AO의 동시값, WP의 동시값, EA/WR의 1개월 선행값은 모든 분석구간에서 양의 상관을 유지하였다. 특히 EA/WR lag1은 평균 이동상관계수가 0.473으로 가장 높았으며, 표준편차도 0.043으로 세 변수 중 가장 작아 상대적으로 높은 시간적 안정성을 보였다. 20년 이동창에서도 세 변수 모두 양의 상관을 유지하여 이러한 결과는 이동창 길이 변화에도 대체로 일관되었다.
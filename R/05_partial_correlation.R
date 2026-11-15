library(tidyverse)
library(here)
library(ppcor)

january <- read_csv(
  here("data", "processed", "january_master.csv"),
  show_col_types = FALSE
)

# 각 lag별 partial correlation 계산 함수
get_partial_by_lag <- function(lag_num) {
  
  vars <- paste0(
    c("AO", "NAO", "WP", "EAWR"),
    "_lag",
    lag_num
  )
  
  dat <- january %>%
    dplyr::select(
      jan_temp_detrended,
      dplyr::all_of(vars)
    )
  
  pc <- ppcor::pcor(
    dat,
    method = "pearson"
  )
  
  tibble(
    index = c("AO", "NAO", "WP", "EAWR"),
    lag = lag_num,
    partial_correlation =
      pc$estimate[
        "jan_temp_detrended",
        vars
      ],
    p_value =
      pc$p.value[
        "jan_temp_detrended",
        vars
      ]
  )
}

partial_results <- purrr::map_dfr(
  0:3,
  get_partial_by_lag
)

partial_results

partial_results <- partial_results %>%
  mutate(
    p_adjusted = p.adjust(
      p_value,
      method = "BH"
    )
  )

partial_results

comparison <- cor_results %>%
  dplyr::select(
    index,
    lag,
    pearson = correlation,
    pearson_p = p_value,
    pearson_p_adjusted = p_adjusted
  ) %>%
  left_join(
    partial_results %>%
      dplyr::select(
        index,
        lag,
        partial = partial_correlation,
        partial_p = p_value,
        partial_p_adjusted = p_adjusted
      ),
    by = c("index", "lag")
  ) %>%
  arrange(
    lag,
    desc(abs(pearson))
  )

comparison

write_csv(
  comparison,
  here(
    "results",
    "tables",
    "pearson_vs_partial.csv"
  )
)

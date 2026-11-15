library(tidyverse)
library(here)

january <- read_csv(
  here("data", "processed", "january_master.csv"),
  show_col_types = FALSE
)

predictors <- c(
  "AO_lag0", "AO_lag1", "AO_lag2", "AO_lag3",
  "NAO_lag0", "NAO_lag1", "NAO_lag2", "NAO_lag3",
  "WP_lag0", "WP_lag1", "WP_lag2", "WP_lag3",
  "EAWR_lag0", "EAWR_lag1", "EAWR_lag2", "EAWR_lag3"
)

cor_results <- map_dfr(
  predictors,
  function(var) {
    
    test <- cor.test(
      january$jan_temp_detrended,
      january[[var]],
      method = "pearson"
    )
    
    tibble(
      variable = var,
      correlation = unname(test$estimate),
      p_value = test$p.value
    )
  }
)

cor_results

cor_results <- cor_results %>%
  separate(
    variable,
    into = c("index", "lag"),
    sep = "_lag",
    remove = FALSE
  ) %>%
  mutate(
    lag = as.integer(lag)
  )

cor_results

cor_ranked <- cor_results %>%
  mutate(
    abs_correlation = abs(correlation)
  ) %>%
  arrange(desc(abs_correlation))

cor_ranked

best_lag_by_index <- cor_results %>%
  group_by(index) %>%
  slice_max(
    order_by = abs(correlation),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

best_lag_by_index

write_csv(
  cor_results,
  here(
    "results",
    "tables",
    "lag_correlation_results.csv"
  )
)

write_csv(
  best_lag_by_index,
  here(
    "results",
    "tables",
    "best_lag_by_index.csv"
  )
)

p_cor_heatmap <- ggplot(
  cor_results,
  aes(
    x = lag,
    y = index,
    fill = correlation
  )
) +
  geom_tile() +
  geom_text(
    aes(
      label = sprintf("%.2f", correlation)
    ),
    size = 4
  ) +
  scale_x_continuous(
    breaks = 0:3
  ) +
  labs(
    title = "Lagged Correlations with January Temperature",
    subtitle = "Detrended January mean temperature in Seoul",
    x = "Lag (months)",
    y = "Teleconnection index",
    fill = "Correlation"
  ) +
  theme_minimal()

p_cor_heatmap

ggsave(
  here(
    "results",
    "figures",
    "lag_correlation_heatmap.png"
  ),
  p_cor_heatmap,
  width = 8,
  height = 5,
  dpi = 300
)

cor_results <- cor_results %>%
  mutate(
    p_adjusted = p.adjust(
      p_value,
      method = "BH"
    )
  )

write_csv(
  cor_results,
  here(
    "results",
    "tables",
    "lag_correlation_results.csv"
  )
)


cor_results <- cor_results %>%
  mutate(
    p_adjusted = p.adjust(
      p_value,
      method = "BH"
    )
  )

cor_ranked <- cor_results %>%
  mutate(
    abs_correlation = abs(correlation)
  ) %>%
  arrange(desc(abs_correlation))

cor_ranked

write_csv(
  cor_results,
  here(
    "results",
    "tables",
    "lag_correlation_results.csv"
  )
)

cor_results <- cor_results %>%
  mutate(
    sig = case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01  ~ "**",
      p_adjusted < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

p_cor_heatmap <- ggplot(
  cor_results,
  aes(
    x = lag,
    y = index,
    fill = correlation
  )
) +
  geom_tile() +
  geom_text(
    aes(
      label = paste0(
        sprintf("%.2f", correlation),
        sig
      )
    ),
    size = 4
  ) +
  scale_x_continuous(
    breaks = 0:3
  ) +
  labs(
    title = "Lagged Correlations with January Temperature",
    subtitle = "Detrended January mean temperature in Seoul",
    x = "Lag (months)",
    y = "Teleconnection index",
    fill = "Correlation"
  ) +
  theme_minimal()

p_cor_heatmap

ggsave(
  here(
    "results",
    "figures",
    "lag_correlation_heatmap.png"
  ),
  p_cor_heatmap,
  width = 8,
  height = 5,
  dpi = 300
)
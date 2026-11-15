library(tidyverse)
library(here)

january <- read_csv(
  here("data", "processed", "january_master.csv"),
  show_col_types = FALSE
)

test_years <- 1990:max(january$year)

oos_results <- map_dfr(
  test_years,
  function(test_year) {
    
    train <- january %>%
      filter(year < test_year)
    
    test <- january %>%
      filter(year == test_year)
    
    # Baseline: 과거 detrended January temperature 평균
    baseline_pred <- mean(
      train$jan_temp_detrended,
      na.rm = TRUE
    )
    
    # Model 1: strongest lag-1 predictor
    model1 <- lm(
      jan_temp_detrended ~ EAWR_lag1,
      data = train
    )
    
    pred1 <- predict(
      model1,
      newdata = test
    )
    
    # Model 2: all meaningful lag-1 predictors
    model2 <- lm(
      jan_temp_detrended ~ AO_lag1 + WP_lag1 + EAWR_lag1,
      data = train
    )
    
    pred2 <- predict(
      model2,
      newdata = test
    )
    
    tibble(
      year = test_year,
      actual = test$jan_temp_detrended,
      baseline = baseline_pred,
      model_EAWR = pred1,
      model_all_lag1 = pred2
    )
  }
)

oos_results


calc_metrics <- function(actual, predicted) {
  
  tibble(
    RMSE = sqrt(
      mean(
        (actual - predicted)^2
      )
    ),
    
    MAE = mean(
      abs(actual - predicted)
    )
  )
}

metrics <- bind_rows(
  calc_metrics(
    oos_results$actual,
    oos_results$baseline
  ) %>%
    mutate(model = "Baseline"),
  
  calc_metrics(
    oos_results$actual,
    oos_results$model_EAWR
  ) %>%
    mutate(model = "EAWR_lag1"),
  
  calc_metrics(
    oos_results$actual,
    oos_results$model_all_lag1
  ) %>%
    mutate(model = "AO_WP_EAWR_lag1")
) %>%
  dplyr::select(
    model,
    everything()
  )

metrics


baseline_sse <- sum(
  (oos_results$actual -
     oos_results$baseline)^2
)

oos_r2 <- tibble(
  model = c(
    "EAWR_lag1",
    "AO_WP_EAWR_lag1"
  ),
  
  OOS_R2 = c(
    
    1 -
      sum(
        (oos_results$actual -
           oos_results$model_EAWR)^2
      ) /
      baseline_sse,
    
    1 -
      sum(
        (oos_results$actual -
           oos_results$model_all_lag1)^2
      ) /
      baseline_sse
  )
)

oos_r2


write_csv(
  oos_results,
  here(
    "results",
    "tables",
    "oos_predictions.csv"
  )
)

write_csv(
  metrics,
  here(
    "results",
    "tables",
    "oos_metrics.csv"
  )
)

write_csv(
  oos_r2,
  here(
    "results",
    "tables",
    "oos_r2.csv"
  )
)

plot_df <- oos_results %>%
  dplyr::select(
    year,
    actual,
    model_EAWR,
    model_all_lag1
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "series",
    values_to = "temperature"
  )

p_oos <- ggplot(
  plot_df,
  aes(
    x = year,
    y = temperature,
    linetype = series
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  labs(
    title = "Out-of-Sample January Temperature Prediction",
    subtitle = "Expanding-window evaluation",
    x = "Year",
    y = "Detrended January temperature (°C)",
    linetype = NULL
  ) +
  theme_minimal()

p_oos

ggsave(
  here(
    "results",
    "figures",
    "oos_predictions.png"
  ),
  p_oos,
  width = 9,
  height = 5,
  dpi = 300
)

metrics
oos_r2

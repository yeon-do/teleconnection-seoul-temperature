library(tidyverse)
library(here)
library(car)
library(broom)

january <- read_csv(
  here("data", "processed", "january_master.csv"),
  show_col_types = FALSE
)

model_ols <- lm(
  jan_temp_detrended ~ AO_lag0 + WP_lag0 + EAWR_lag1,
  data = january
)

summary(model_ols)

ols_table <- broom::tidy(
  model_ols,
  conf.int = TRUE
)

ols_table

write_csv(
  ols_table,
  here(
    "results",
    "tables",
    "ols_main_results.csv"
  )
)

ols_glance <- broom::glance(model_ols)

ols_glance %>%
  dplyr::select(
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value,
    AIC,
    BIC
  )

write_csv(
  ols_glance,
  here(
    "results",
    "tables",
    "ols_model_fit.csv"
  )
)

vif_results <- car::vif(model_ols)

vif_results

vif_table <- tibble(
  variable = names(vif_results),
  VIF = as.numeric(vif_results)
)

write_csv(
  vif_table,
  here(
    "results",
    "tables",
    "ols_vif.csv"
  )
)

par(mfrow = c(2, 2))
plot(model_ols)
par(mfrow = c(1, 1))

# Durbin-Watson
car::durbinWatsonTest(model_ols)

# Breusch-Pagan
install.packages("lmtest")   # 처음 한 번만
library(lmtest)

lmtest::bptest(model_ols)

shapiro.test(residuals(model_ols))

residual_df <- tibble(
  residual = resid(model_ols),
  fitted = fitted(model_ols)
)

p_qq <- ggplot(
  residual_df,
  aes(sample = residual)
) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Q-Q Plot of OLS Residuals",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal()

p_qq

ggsave(
  here(
    "results",
    "figures",
    "ols_residual_qq.png"
  ),
  p_qq,
  width = 6,
  height = 5,
  dpi = 300
)

p_resid <- ggplot(
  residual_df,
  aes(
    x = fitted,
    y = residual
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_point() +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()

p_resid

ggsave(
  here(
    "results",
    "figures",
    "ols_residual_vs_fitted.png"
  ),
  p_resid,
  width = 6,
  height = 5,
  dpi = 300
)

end_years <- seq(
  1990,
  max(january$year),
  by = 5
)

coef_stability <- map_dfr(
  end_years,
  function(end_year) {
    
    train_df <- january %>%
      filter(year <= end_year)
    
    model <- lm(
      jan_temp_detrended ~ AO_lag0 + WP_lag0 + EAWR_lag1,
      data = train_df
    )
    
    broom::tidy(model) %>%
      filter(term != "(Intercept)") %>%
      transmute(
        end_year = end_year,
        variable = term,
        coefficient = estimate,
        std_error = std.error,
        p_value = p.value
      )
  }
)

coef_stability


p_coef <- ggplot(
  coef_stability,
  aes(
    x = end_year,
    y = coefficient,
    linetype = variable
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point() +
  labs(
    title = "Coefficient Stability Across Training Periods",
    x = "Training end year",
    y = "OLS coefficient",
    linetype = "Predictor"
  ) +
  theme_minimal()

p_coef


ggsave(
  here(
    "results",
    "figures",
    "ols_coefficient_stability.png"
  ),
  p_coef,
  width = 8,
  height = 5,
  dpi = 300
)

coef_stability_summary <- coef_stability %>%
  group_by(variable) %>%
  summarise(
    mean_coef = mean(coefficient),
    sd_coef = sd(coefficient),
    min_coef = min(coefficient),
    max_coef = max(coefficient),
    positive_ratio = mean(coefficient > 0),
    .groups = "drop"
  )

coef_stability_summary

write_csv(
  coef_stability_summary,
  here(
    "results",
    "tables",
    "ols_coefficient_stability_summary.csv"
  )
)


# Pearson에서 후보 신호 탐색
#→ Partial correlation에서 AO lag0, WP lag0, EA/WR lag1만 유지
#→ Rolling window에서도 세 관계 모두 일관된 양의 관계
#→ 다중회귀에 함께 넣어도 세 변수 모두 유의
#→ VIF 및 학습기간 변화에서도 계수가 안정적

#이게 딱 논문의 **“여러 검증조건에서도 유지되는 신호”**라는 주제에 맞아.

#다중회귀분석 결과 AO의 동시값, WP의 동시값, EA/WR의 1개월 선행값은 다른 변수들을 동시에 고려한 이후에도 모두 유의한 양의 계수를 나타냈다. 모형의 결정계수는 0.408이었다. 각 변수의 VIF는 1.03–1.12로 낮게 나타나 다중공선성 문제는 확인되지 않았다. 또한 학습기간을 순차적으로 확장하여 회귀계수를 반복 추정한 결과, 세 변수 모두 모든 분석기간에서 양의 계수를 유지하였으며 계수의 표준편차도 0.029–0.056으로 작게 나타났다. 이에 따라 본 자료에서는 별도의 정규화 회귀를 적용할 필요성이 크지 않은 것으로 판단하였다.


# Raw temperature sensitivity analysis -----------------------------

model_ols_raw <- lm(
  jan_temp_raw ~ AO_lag0 + WP_lag0 + EAWR_lag1,
  data = january
)

summary(model_ols_raw)

raw_ols_table <- broom::tidy(
  model_ols_raw,
  conf.int = TRUE
)

raw_ols_table

write_csv(
  raw_ols_table,
  here(
    "results",
    "tables",
    "ols_raw_temperature_sensitivity.csv"
  )
)
broom::glance(model_ols_raw) %>%
  dplyr::select(
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value
  )


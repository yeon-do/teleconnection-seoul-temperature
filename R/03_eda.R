library(tidyverse)
library(here)

january <- read_csv(
  here(
    "data",
    "processed",
    "january_master.csv"
  ),
  show_col_types = FALSE
)

eda_summary <- january %>%
  summarise(
    n = n(),
    
    mean_temp = mean(jan_temp_raw),
    sd_temp = sd(jan_temp_raw),
    min_temp = min(jan_temp_raw),
    max_temp = max(jan_temp_raw),
    
    mean_detrended = mean(jan_temp_detrended),
    sd_detrended = sd(jan_temp_detrended),
    
    mean_AO = mean(AO_lag0),
    sd_AO = sd(AO_lag0),
    
    mean_NAO = mean(NAO_lag0),
    sd_NAO = sd(NAO_lag0),
    
    mean_WP = mean(WP_lag0),
    sd_WP = sd(WP_lag0),
    
    mean_EAWR = mean(EAWR_lag0),
    sd_EAWR = sd(EAWR_lag0)
  )

eda_summary

write_csv(
  eda_summary,
  here(
    "results",
    "tables",
    "eda_summary.csv"
  )
)

p_raw <- ggplot(
  january,
  aes(
    x = year,
    y = jan_temp_raw
  )
) +
  geom_line() +
  geom_point(size = 1.5) +
  geom_line(
    aes(
      y = jan_temp_trend
    ),
    linewidth = 1
  ) +
  labs(
    title = "January Mean Temperature in Seoul",
    subtitle = "1954–2026",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_minimal()

p_raw

ggsave(
  here(
    "results",
    "figures",
    "january_temperature_raw.png"
  ),
  p_raw,
  width = 8,
  height = 5,
  dpi = 300
)

p_detrended <- ggplot(
  january,
  aes(
    x = year,
    y = jan_temp_detrended
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line() +
  geom_point(size = 1.5) +
  labs(
    title = "Detrended January Mean Temperature in Seoul",
    subtitle = "1954–2026",
    x = "Year",
    y = "Detrended temperature (°C)"
  ) +
  theme_minimal()

p_detrended

ggsave(
  here(
    "results",
    "figures",
    "january_temperature_detrended.png"
  ),
  p_detrended,
  width = 8,
  height = 5,
  dpi = 300
)

trend_model <- lm(
  jan_temp_raw ~ year,
  data = january
)

trend_summary <- broom::tidy(
  trend_model
) %>%
  filter(term == "year") %>%
  transmute(
    coefficient_per_year = estimate,
    coefficient_per_decade = estimate * 10,
    std_error = std.error,
    t_value = statistic,
    p_value = p.value
  )

trend_summary

library(broom)

write_csv(
  trend_summary,
  here(
    "results",
    "tables",
    "january_temperature_trend.csv"
  )
)


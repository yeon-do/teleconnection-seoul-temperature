# teleconnection-seoul-temperature

# Teleconnection and Seoul January Temperature

서울 1월 평균기온과 글로벌 원격상관(Teleconnection) 지수의 관계 및 안정성을 분석한 졸업논문 프로젝트입니다.

## Research Question

AO, NAO, WP, EA/WR 등의 원격상관 지수가 서울 1월 평균기온과 어떤 관계를 가지며, 이러한 관계가 시차와 분석기간이 달라져도 안정적으로 유지되는지를 분석합니다.

## Data

- Seoul monthly temperature
  - Korea Meteorological Administration (KMA) ASOS
  - Seoul station 108
- Teleconnection indices
  - NOAA Climate Prediction Center
  - AO, NAO, WP, EA/WR

Raw data are not included in this repository.  
Please download the source data from KMA and NOAA before running the preprocessing script.

## Analysis Pipeline

1. `01_build_dataset.R`
   - Raw data preprocessing
   - KMA and NOAA data integration
   - Lag variable construction
   - January dataset generation
   - Temperature detrending

2. `02_preliminary_analysis.R`
   - January vs. July correlation comparison

3. `03_eda.R`
   - Descriptive statistics
   - January temperature trend
   - Detrended temperature analysis

4. `04_correlation.R`
   - Lagged Pearson correlation
   - FDR correction
   - Correlation heatmap

5. `05_partial_correlation.R`
   - Partial correlation analysis

6. `06_rolling.R`
   - 20-year and 30-year rolling correlation analysis

7. `07_regression.R`
   - Multiple linear regression
   - VIF and residual diagnostics
   - Coefficient stability analysis

8. `08_validation.R`
   - Expanding-window out-of-sample validation

## Main Variables

- AO
- NAO
- WP
- EA/WR
- Seoul January mean temperature

## Environment

Analysis was conducted in R / RStudio.

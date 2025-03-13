# Recession Prediction
Co-authored university project completed in R to forecast recessions.

## Overview
This project implements and compares different machine learning methods to predict US recessions, with a particular focus on the 2008 Financial Crisis. The research analyzes how different forecasting horizons affect prediction accuracy and identifies which economic indicators serve as the most reliable signals for upcoming recessions.

## Methods
The project implements and compares three key methodologies:
- **Dynamic Probit** - Using lagged interest rate spread as a predictor
- **Autoregressive Dynamic Probit** - Enhancing the Dynamic Probit by adding lagged recession indicator
- **Random Forest Classifier** - Utilizing a comprehensive set of indicators from FRED-MD database

## Key Findings
- Autoregressive Dynamic Probit performs best for short-term forecasts (1-3 months ahead)
- Random Forest outperforms other methods for longer forecast horizons (6-12 months ahead)
- The lagged recession indicator proves to be a crucial predictor for short-term forecasts
- Interest rate spreads, particularly between longer-term bonds and Fed Funds rate, are important indicators for medium to long-term forecasts
- Employment metrics serve as significant predictors for detecting imminent recessions

## Data Sources
- **USREC**: US Recession indicator (binary variable; 1 = recession, 0 = no recession) from NBER
- **FRED-MD**: Federal Reserve Economic Data (comprehensive macroeconomic dataset)

## File Structure
- `recessionpredict.R`: Main R script containing the implementation of all forecasting models
- Supporting function files:
  - `modified-func-dy.R`: Functions for Dynamic Probit model
  - `modified-func-ar.R`: Functions for Autoregressive Dynamic Probit model
  - `modified-func-rf.R`: Functions for Random Forest model
  - `modified-func-rf-unknown.R`: Functions for Random Forest without lagged recession values

## Approach
1. **Data Preprocessing**:
   - Transformation of variables to achieve stationarity
   - Creation of interest rate spread variables
   - Removal of time periods with missing values

2. **Model Training**:
   - Use of rolling window methodology for time series forecasting
   - Prediction at 1, 3, 6, and 12-month horizons

3. **Evaluation Metrics**:
   - Root Mean Squared Error (RMSE) for probability forecasts
   - Misclassification percentage for binary predictions

4. **Extensions**:
   - Analysis of performance when recession status is unknown
   - Attempt to predict the COVID-19 recession

## Results
The research demonstrates that while short-term recession forecasting can be reliably performed with relatively simple models using lagged recession indicators, longer-term forecasting benefits from more complex ensemble methods like Random Forest that can incorporate a wider range of economic variables.

Interest rate spreads prove to be valuable leading indicators, particularly for medium to long-term forecasts, validating previous research while expanding on which specific spreads are most informative at different time horizons.

## Co-Authors
- Devika Rastogi 
- Manan Mittal 
- Maulik Jain

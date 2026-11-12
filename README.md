<div align="right">

[![Unit Tests](https://github.com/jasp-stats/jaspTimeSeries/actions/workflows/unittests.yml/badge.svg)](https://github.com/jasp-stats/jaspTimeSeries/actions/workflows/unittests.yml)
[![codecov](https://codecov.io/gh/jasp-stats/jaspTimeSeries/branch/master/graph/badge.svg)](https://codecov.io/gh/jasp-stats/jaspTimeSeries)
<br>
<b>Maintainer:</b> <a href="https://github.com/hrgodmann">Henrik Godmann</a>

</div>

# The Time Series Module

## Overview

The Time Series module for [JASP](https://jasp-stats.org) offers classical and Bayesian tools for exploring time series, assessing stationarity, modelling temporal patterns and forecasting.

## Analyses

The analyses are grouped as they appear in JASP:

### Descriptives

- **Time Series Descriptives:** Explore a series with descriptive statistics, time series and lag plots, and autocorrelation and partial autocorrelation plots.

### Classical

- **Stationarity:** Assess stationarity with Augmented Dickey–Fuller, Phillips–Perron and KPSS tests, apply transformations, and inspect the transformed series and its autocorrelations.
- **ARIMA:** Fit automatic or manually specified seasonal and nonseasonal ARIMA models, optionally with numeric covariates, and obtain forecasts and residual diagnostics.
- **Spectral Analysis:** Explore periodic patterns through spectral density estimates, with optional smoothing, detrending and reference noise curves.

### Bayesian

- **Gaussian State Space Models:** Fit Bayesian models with trend, seasonal, autoregressive and regression components, inspect posterior summaries and state estimates, and forecast series without predictors.

## Contributing

We welcome new analyses, improvements, bug fixes and documentation contributions!
Feel free to [contact Henrik Godmann](mailto:hrgodmann@gmail.com) to discuss an idea, or simply [submit a pull request](https://github.com/jasp-stats/jaspTimeSeries/pulls) and assign Henrik as reviewer.

All analyses use shared Time preparation: a complete numeric or unambiguous date/time variable orders observations when assigned, while empty Time uses spreadsheet row order and unusable or ambiguous Time triggers a warning and falls back to row order.
New analyses should reuse this preparation and its explanatory notes from `jaspDescriptives`, preserving missing-response positions and original-row mappings; current analyses treat successive observations as equally spaced steps without inserting rows for gaps in Time.

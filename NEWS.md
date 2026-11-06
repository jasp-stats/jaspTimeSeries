# jaspTimeSeries Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
>
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md).
> * **Adding New Changes (For Contributors):** Add new entries under `# jaspTimeSeries (development version)` and the appropriate category (`## Added`, `## Changed`, `## Fixed`, etc.).
> * **Issue References:** Reference the relevant GitHub issue when one exists.
> * **Format Categories:**
>   * **Added:** New analyses, features, options, or output.
>   * **Changed:** Updates to existing analyses, defaults, dependencies, or output.
>   * **Fixed:** Bug fixes in analyses, plots, tables, help, QML layouts, or module infrastructure.
>   * **Deprecated / Removed:** Outdated analyses, options, or legacy code.

---
# jaspTimeSeries (development version)

## Added
* Added missing-observation choices for autocorrelation plots, stationarity tests, spectral analysis, and ARIMA's Ljung-Box diagnostics: exclude cases listwise (default) or use the longest complete segment. Listwise exclusion adds an output note explaining that the remaining observations are treated as consecutive steps, ignoring internal gaps; model inputs and saved-row alignment are unchanged.
* Integrated the Bayesian State Space Models analysis from the standalone `jaspBsts` module, originally developed by Fridtjof Petersen.

## Changed
* Time series analyses, including Bayesian state space models, use Time to order whole observations, treating successive observations as equal steps without filling gaps. Unusable or ambiguous Time falls back to row order with a warning; ties retain spreadsheet order. Bayesian models retain missing-response steps and keep covariates and fixed factors aligned.
* With Time supplied, ARIMA forecasts use steps ahead in tables and CSV exports, and observation steps in plots. With Time empty, plots, tables, and CSV exports use the same continuing row index. Stationarity detrending uses observation steps.
* Bayesian state space forecast plots with Time supplied label their axis as observation steps; future dates or numeric Time values are not inferred.
* Renamed the Bayesian State Space Models analysis to Gaussian State Space Models; the R function and saved-analysis identifiers remain unchanged.
* Updated module metadata to use the project website and a consistent package title.

## Fixed
* Respect the selected differencing order in Stationarity while retaining missing positions and the original spreadsheet alignment of saved transformations.
* Initialize Bayesian local-level and semilocal-trend priors from the first observed response when the series starts with missing values, preserving every time step for estimation and residual plots.
* Load the Bayesian state space summary methods with the module so restored models can regenerate their tables in a fresh R session.
* Made the shared credible-interval setting accessible independently of the aggregated state plot and clarified that it also controls the residual and one-step-ahead prediction error plots.
* Corrected the seasonal prior labels to show standard deviations in the dependent variable's original units, without changing the priors or their defaults.
* Ignore the Bayesian state space dynamic regression option when no predictors remain in the model, preventing a crash after removing predictors or model terms.
* Allow any positive whole-number count of desired Bayesian state space MCMC draws, including counts not divisible by ten, and report invalid counts clearly.
* Preserve gaps in the Bayesian state space residual plot when responses are missing, instead of terminating the analysis while calculating the residual interval.
* Accept unambiguous dates with one- or two-digit days and months. Date-only End filters include the full UTC day; timestamp End filters retain the exact cutoff.
* Warn when a row filter skips observations between retained observations in Time order, while allowing the analysis to continue.
* Warn and use spreadsheet row order when decimal and year.month interpretations of text Time disagree; numeric Scale values keep decimal ordering.
* Omit unavailable ARIMA residuals from the Q-Q plot so missing observations do not prevent the remaining output and forecasts.
* Preserve numeric Time values, support decimal/negative Time filters, and align saved residuals and transformed columns by original spreadsheet rows, including tied times.
* Align ARIMA forecasts and future covariates after the final modeled row, including trailing missing responses, and preserve matrix dimensions with a single covariate.
* Allow ARIMA models without coefficients to display their estimated residual variance, diagnostics, and forecasts, with an explanatory coefficient-table note.
* Clarified that Stationarity plots reflect the selected transformations and moved ARIMA's observed time series controls into a Plots section.
* Clarified why applying a root transformation after a log transformation can fail in Stationarity.
* Invalidate the cached Bayesian State Space model and its outputs when the random seed changes.
* Validate Bayesian State Space control periods and explain invalid selections on the affected plots without hiding model tables.
* Clarified Bayesian State Space model-estimation failures with guidance for numerical sampling problems while preserving the underlying package error details.
* Use standard numeric formatting for Bayesian State Space coefficient means and SDs, allowing scientific notation for extreme values.
* Display a dot and an explanatory footnote when Harvey's goodness of fit is unavailable in Bayesian State Space Models.
* Clarified the Bayesian State Space validation message for missing or non-numeric covariate values, with guidance to use Fixed Factors for categorical predictors.
* Applied the selected burn-in consistently to Bayesian State Space tables, plots and forecasts, corrected zero-burn handling, and validated burn-in against completed MCMC draws.
* Synchronized the module version in `inst/Description.qml` with `DESCRIPTION`.
* Corrected the translation workflow to target the jaspTimeSeries Weblate components.
* Declared the directly used `tseries` package dependency.

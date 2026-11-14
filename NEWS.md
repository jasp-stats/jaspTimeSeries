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
* Standardized Bayesian state space plots with JASP styling, neutral credible-interval bands, clearer axis labels, and component panels that grow with the number of states.
* Time series analyses, including Bayesian state space models, use Time to order whole observations, treating successive observations as equal steps without filling gaps. Unusable or ambiguous Time falls back to row order with a warning; ties retain spreadsheet order. Bayesian models retain missing-response steps and keep covariates and fixed factors aligned.
* With Time supplied, ARIMA forecasts use steps ahead in tables and CSV exports, and observation steps in plots. With Time empty, plots, tables, and CSV exports use the same continuing row index. Stationarity detrending uses observation steps.
* Bayesian state space forecast plots with Time supplied label their axis as observation steps; future dates or numeric Time values are not inferred.
* Renamed the Bayesian State Space Models analysis to Gaussian State Space Models; the R function and saved-analysis identifiers remain unchanged.
* Updated module metadata to use the project website and a consistent package title.

## Fixed
* Changing only the fixed factors or the MCMC timeout in Bayesian state space models now refits the model, instead of possibly keeping the previous results.
* Stationarity tests now note that their p-values are not valid after removing a polynomial trend of degree 2 or higher, because the tests' reference distributions assume at most a linear trend.
* Relabeled ARIMA output: the residual time series plot shows ordinary residuals in the units of the dependent variable, not standardized residuals, and the forecast table's intervals are prediction intervals (as computed by the forecast package), not confidence intervals.
* ARIMA Ljung-Box p-values now subtract the fitted model's nonseasonal AR and MA coefficients (p + q) from the degrees of freedom, as is conventional for model residuals; previously they were too large. Lags up to p + q have no test and are omitted with a note. Seasonal and regression coefficients are not subtracted.
* Seasonal ARIMA no longer fits a seasonal period of 1, where seasonal and nonseasonal lag-1 terms coincide and could not be estimated separately (previously reported with missing standard errors and no warning). A custom period must be at least 2, with a new default of 12; when no dominant period is found, a nonseasonal model is fitted and a note says so. Saved analyses with seasonal components and a custom period of 1 now ask for a period, including Best-fitting analyses, which previously fitted a nonseasonal model without saying so.
* "Add to null model" in Bayesian state space models now takes effect: ticked terms are included in every model (prior inclusion probability 1), so the other predictors' inclusion Bayes factors and model-averaged estimates are conditional on them, as in Bayesian linear regression. With dynamic regression, inclusion probabilities and Bayes factors concern only the constant (static) coefficients, and the coefficient tables now say so. Previously the checkbox was ignored and ticked terms were selected like all others. An expected number of predictors of 0 or less now gives a clear message.
* Corrected the Bayesian state space inclusion Bayes factor, which reported posterior inclusion odds without dividing by the prior inclusion odds; it was correct only when the prior inclusion probability was 0.5. The Bayes factor is now left empty, with a note, when the prior inclusion probability is 0 or 1 or when all or none of the retained draws include the predictor. Inclusion summaries and the zero placeholder estimates are no longer shown for an intercept that bsts absorbs into the level component (local level, local linear trend or semi-local linear trend).
* Keep Bayesian state space plot labels visible, use shortened axes for calendar Time, show each selected trend component separately, and place control-chart explanations below the plot as standard notes.
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

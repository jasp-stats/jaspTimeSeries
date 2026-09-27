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
* Integrated the Bayesian State Space Models analysis from the standalone `jaspBsts` module, originally developed by Fridtjof Petersen.

## Changed
* Renamed the Bayesian State Space Models analysis to Gaussian State Space Models; the R function and saved-analysis identifiers remain unchanged.
* Updated module metadata to use the project website and a consistent package title.

## Fixed
* Invalidate the cached Bayesian State Space model and its outputs when the random seed changes.
* Validate Bayesian State Space control periods and explain invalid selections on the affected plots without hiding model tables.
* Clarified Bayesian State Space model-estimation failures while preserving the underlying package error details.
* Use standard numeric formatting for Bayesian State Space coefficient means and SDs, allowing scientific notation for extreme values.
* Display a dot and an explanatory footnote when Harvey's goodness of fit is unavailable in Bayesian State Space Models.
* Clarified the Bayesian State Space validation message for missing or non-numeric covariate values, with guidance to use Fixed Factors for categorical predictors.
* Applied the selected burn-in consistently to Bayesian State Space tables, plots and forecasts, corrected zero-burn handling, and validated burn-in against completed MCMC draws.
* Synchronized the module version in `inst/Description.qml` with `DESCRIPTION`.
* Corrected the translation workflow to target the jaspTimeSeries Weblate components.
* Declared the directly used `tseries` package dependency.

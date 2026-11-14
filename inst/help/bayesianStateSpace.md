Gaussian State Space Models
===

The Gaussian State Space Model decomposes the hidden state of a time series into different additive state components (e.g. trend, seasonality) and estimates them via the Kalman Filter. MCMC samples are drawn from the posterior distribution to allow for Bayesian inference. It also allows for inclusion of predictors and variable selection by the means of spike and slab regression and Bayesian model averaging.

### Assumptions
- continuous response variable
- normally distributed response variable and state components
- normally distributed error terms
- no missing values in predictors (but possibly in response variable)
- future prediction only without predictor variables

### Input
---
#### Assignment Box
- Dependent Variable: Response variable to be estimated (needed)
- Covariates: Numerical predictor variables (optional)
- Fixed Factors: Categorical predictor variables that will be recoded internally to dummy variables (optional)
- Time: Optional variable that determines observation order for the analysis. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use `YYYY-MM-DD` for dates or `YYYY-MM-DD HH:MM:SS` for dates with times; seconds can be omitted. Responses, covariates and fixed factors stay aligned, and the spreadsheet order is unchanged. Tied values retain spreadsheet row order. If any Time value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Output notes explain date interpretation, sorting and repeated Time values when applicable.

Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time. Missing response values retain their steps in the fitted model. State estimates can therefore be available at steps without an observed response. These steps are not removed or replaced by observed values. Predictors must be complete.

#### Output
- Posterior summary of coefficients: Shows model-averaged coefficient estimates, using all retained MCMC draws, including zeros when a predictor was excluded.
  - Show estimates conditional on inclusion: Adds a separate table of coefficient estimates using only retained draws in which the predictor was included. This table includes conditional means, standard deviations and credible intervals, together with posterior inclusion probabilities and the number of included draws.
  - Credible Interval: Sets the credible interval level for both coefficient tables. The model-averaged intervals include zero-valued excluded draws; the conditional intervals use only included draws.
- Expected predictors: Number of predictors that are expected, affects prior inclusion probability. It is divided over all design columns, including the intercept, each factor dummy and the columns of null-model terms; the prior inclusion probability actually used is shown in the P(incl) column.

### Model Components
---
#### Components and model terms
  - Components: All the independent variables that can be included in the model.
  - Model terms: The independent variables in the model. By default, all the main effects of the specified independent variables are included in the model. To include interactions, click multiple variables (e.g., by holding the ctrl/cmd button on your keyboard while clicking) and drag those into the `Model Terms` box. Ticking "Add to null model" includes a term in every model (prior inclusion probability 1), as in Bayesian linear regression. The null model then consists of the state components and these terms. Without a level component (local level, local linear trend or semi-local linear trend), the intercept is not part of the null model; it is selected like the other predictors. The inclusion Bayes factors of the other predictors compare models with and without them, all including the null-model terms; null-model terms themselves have no inclusion Bayes factor. With a dynamic regression component, every predictor also has a time-varying effect, and inclusion probabilities and Bayes factors concern only the constant (static) coefficient: whether a predictor has a constant effect in addition to its time-varying effect. For a factor or interaction, all of its columns are included, except columns that are zero in every observation (e.g., an empty combination of factor levels), which bsts excludes.

- Add autoregressive component: Adds an autoregressive component to the model where the current state is predicted from it's previous state via an autoregressive coefficient(e.g. alpha[t] = phi*alpha[t-1] + error for an AR(1) process)
  - Manually: The number in `No. of lags` determines the number of lags/coefficients added
  - Automatic: Determines the optimal amount of coefficients by putting a spike and slab prior on them, with `Maximal lags` being the limit.

- Add local level component: Adds a trend component consisting of a mean following a random walk to the state (mu[t] = mu[t-1] + error)

- Add local linear trend component: Adds a trend components consisting of a mean(mu[t] = mu[t-1] + delta[t-1] + error) and a slope (delta[t] = delta[t-1]+error) which both follow a random walk.

- Add dynamic regression component: When variables were added to the `Model Terms` box this option can be used to make the regression coefficients dynamic/change over time where they follow a random walk.

#### Seasonalities
- This box can be used to add one or more seasonality components to the model
  - Name: Name of the seasonality. If not provided automatic name containing the number and duration of seasonality will be created.
  - Number: The total number of seasons to be included after which cycle starts again (e.g. 12 for monthly seasonality or 52 for weekly). Each season gets a dummy variable with dynamic coefficients following a random walk
  - Duration: The number of data points each season contains. For example if we have a weekly time series (i.e. 54 data points for 54 weeks) but want to model a monthly seasonality we would set the `Number` to 12 and the `Duration` to 4 as each season lasts/contains 4 time points.
  - Inverse gamma prior for σ: This prior determines the expectations regarding the σ of the random walk. Parameterised as a scaled inverse Chi-Squared distribution:
    - σ: Prior guess for the standard deviation, in the dependent variable's original units.
    - n: Prior observation count/degrees of freedom that weight the σ guess
  - Normal Prior initial state: This normal distribution determines our expectation regarding the first value of our seasonality which is then used by the Kalman filter:
    - μ: Mean of prior distribution
    - σ: Standard deviation of the prior distribution, in the dependent variable's original units.

### Plots
---
- Credible interval (aggregated state and errors): Sets the percentage of the posterior distribution shown in the central credible bands for the aggregated state, residual, and one-step-ahead prediction error plots. This setting is available independently of which plots are selected. Component-state, prediction, and control-chart intervals remain at 95%.

#### State Plots
- Aggregated state contribution: Plots the aggregated state where all state components (such as local level or regression components) are added together
  - Show observation: Adds the actual observations on top of the plot.
- Component state contribution: Plots the individual state contributions of each component

#### Residuals
- Posterior distribution of residuals: Plots the residuals of estimated state vs actual observations after applying the Kalman filter and smoother (i.e. going forward and backward in time)
- Posterior distribution of one-step-ahead prediction error: Plots the prediction error when the Kalman filter is used to estimate the next time point from all previous observations(i.e. going forward in time)

#### Prediction
- This option plots the posterior predictive distribution sampled from the specified state space model. As of now only possible without predictor variables.
  - Horizon: Determines the number of observation steps predicted beyond the final step in analysis order, including any trailing missing responses. Value needs to be larger than 0 to make predictions and show a plot. The forecast plot uses observation-step numbers; it does not infer future dates or numeric Time values.

#### Control chart
- Show control chart: This option plots the aggregated state but adds a critical threshold that can be used as a control chart. The threshold is calculated by mean(state) +/- L*sigma
  - Control period end: The number of initial observations in analysis order used as the baseline for calculating the mean and sigma of the threshold. For example, 20 uses the first 20 steps after ordering by usable Time, including steps with missing responses. This is a step count, not a Time value or date.
  - Sigma threshold: How many sigmas a value has to exeed our mean to be considered as a critical value
  - Show probalistic control plot: Instead of plotting whether the aggregated state and its credible interval exeed the threshold, this plot shows the probability that the state exceeds the threshold.

### Advanced Options
---
- Desired MCMC draws: Integer which determines how many samples from the posterior distribution will be drawn.
- Timeout in seconds: Determines the amount of time after which the MCMC sampling stops even if not all MCMC samples are drawn. Sometimes sampling takes very long and can break the analysis. This option is a safeguard against this.

#### Burn-in Specification
- Automatic Suggestion: Suggests a burn-in period that ends when a certain threshold of log-likelihood is exceeded. This threshold is determined by looking at the tail fraction of all MCMC draws for the log-likelihood and then taking the 90% percent quantile as a cut-off/threshold.
  - Proportion: Determines the tail fraction from which the log-likelihood will be determined, not the fraction of draws discarded. The proportion must be greater than zero and large enough for the tail to contain at least one completed draw after rounding. If too few draws were completed, increase the proportion, request more draws or use manual burn-in.
- Manual: Manually select how many MCMC draws will be discarded as a burn-in
  - Number: Discard the first specified number of MCMC draws. Enter a whole number from zero up to one less than the number of draws actually completed. Zero retains all draws; at least one draw must remain. Retaining very few draws does not provide reliable posterior summaries.

All posterior tables, state and diagnostic plots, and forecasts use the same selected burn-in count. Changing burn-in updates these outputs using the existing fitted model without drawing a new MCMC sample.

### Output
---
#### Model Summary
This table summarises the overall fit of the model:
- Residual SD: The posterior mean of the residual standard deviation parameter.
- Prediction SD: Standard deviation of the one-step-ahead prediction error that result from the Kalman filter.
- R²: Proportion of the variance of the original series that is explained by the state space model
- Harvey's goodness of fit:  Proportion of variance explained when estimations are not compared with sample mean to compute variance but rather a random walk. More appropriate for time series data.

#### Model-Averaged Coefficient Estimates
This table summarises the prior and posterior information of the predictor variables and their coefficients. Coefficient estimates use all retained MCMC draws, including zeros when a predictor was excluded, and therefore account for uncertainty about inclusion:
- Coefficients: Names of the predictor variables
- P(incl): The prior probability that a predictor is included in the model.
- P(incl|data): The posterior probability that a predictor is included in the model, meaning its coefficient is nonzero, regardless of sign.
- BF_inclusion: Inclusion Bayes factor, the change from prior to posterior inclusion odds: (P(incl|data) / (1 - P(incl|data))) / (P(incl) / (1 - P(incl))). Values above 1 favour including the predictor, values below 1 favour excluding it. It is not shown in two situations: when the prior inclusion probability is 0 or 1, the Bayes factor is undefined; when all or none of the retained draws included the predictor, the posterior inclusion probability is estimated as 1 or 0 and the Bayes factor cannot be estimated (more MCMC draws may help).
- When the model has a level component (local level, local linear trend or semi-local linear trend), bsts absorbs the intercept into the level and sets its regression coefficient to zero in every draw. Inclusion probabilities, the Bayes factor and coefficient estimates are therefore not shown for the intercept; its contribution is part of the level.
- Mean: Mean of each coefficient across all retained MCMC draws.
- SD: Standard deviation of each coefficient across all retained MCMC draws.
- Credible Interval: Posterior credible interval for each coefficient across all retained MCMC draws, including zeros when the predictor was excluded.

A model-averaged interval can have both bounds at zero when sufficient posterior probability is concentrated at zero. This does not mean that the coefficient is certainly zero.

#### Coefficient Estimates Conditional on Inclusion
This optional table summarises each coefficient using only retained draws in which its predictor was included (the coefficient was nonzero, regardless of sign). Predictors appear in the same order as in the model-averaged table:
- Coefficients: Names of the predictor variables.
- P(incl|data): Posterior inclusion probability, calculated using all retained draws. This is repeated to keep the probability of inclusion visible alongside the conditional estimates.
- Included draws: Number of retained draws in which the predictor was included. Conditional estimates may be unreliable when based on few included draws.
- Mean: Mean of the coefficient across included draws.
- SD: Standard deviation of the coefficient across included draws.
- Credible Interval: Posterior credible interval for the coefficient conditional on inclusion, calculated from included draws only.

If no retained draws include a predictor, its conditional mean, SD and interval are unavailable. With only one included draw, the mean is shown, but the SD and interval are unavailable. Conditional intervals describe coefficient uncertainty assuming inclusion; they do not measure evidence for including the predictor.

#### State Plots:
- Aggregated State: The x-axis uses the interpreted Time values, or row positions if Time is empty or ignored. The y-axis shows the distribution of the estimated states. If the option `Actual observations` is selected, the observed dependent variable values will be shown as points; missing responses have no observation point.
- Component State plot: Plots the individual contribution of each state component to the overall state. The x-axis uses the interpreted Time values, or row positions if Time is empty or ignored, and the y-axis shows the distribution of each component. Each component has a single column where the label on the right corresponds to the component name. The shaded area reflects the credible interval.

#### Prediction Plot:
- Observation-step numbers are on the x-axis. The line before the dashed line shows the actual observations in analysis order. The dashed line represents the final step of the fitted series and the start of prediction, including any trailing missing responses. The black line after the dashed line represents the posterior predictive mean and the shaded area the 95 percent posterior predictive interval. Future dates or numeric Time values are not inferred.

### References
- Scott, S. L. (2020). bsts: Bayesian Structural Time Series (0.9.5) [Computer software]. https://CRAN.R-project.org/package=bsts
- Scott, S. L., & Varian, H. R. (2014). Predicting the present with Bayesian structural time series. International Journal of Mathematical Modelling and Numerical Optimisation, 5(1/2), 4. https://doi.org/10.1504/IJMMNO.2014.059942
- Scott, S. L., & Varian, H. R. (2015). Bayesian Variable Selection for Nowcasting Economic Time Series. In Economic Analysis of the Digital Economy (pp. 119–135). University of Chicago Press. https://doi.org/10.7208/chicago/9780226206981.003.0004


### R Packages
- bsts
- ggplot2

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

#### Output
- Posterior summary of coefficients: Output table containing the marginal posterior summary of coefficients
  - Show means across draws included: Option that additionally shows the mean of the coefficients only if they were above 0/included in the model
  - Credible Interval: Determines the credible interval shown for the coefficients, given they are included in the model
- Expected predictors: Number of predictors that are expected, affects prior inclusion probability

### Model Components
---
#### Components and model terms
  - Components: All the independent variables that can be included in the model.
  - Model terms: The independent variables in the model. By default, all the main effects of the specified independent variables are included in the model. To include interactions, click multiple variables (e.g., by holding the ctrl/cmd button on your keyboard while clicking) and drag those into the `Model Terms` box. Ticking the boxes on the right-hand side allows model terms to always be included (Inclusion probability = 1).

- Add autoregressive component: Adds an autoregressive component to the model where the current state is predicted from it's previous state via an autoregressive coefficient(e.g. alpha[t] = phi*alpha[t-1] + error for an AR(1) process)
  - Manually: The number in `No. of lags` determines the number of lags/coefficients added
  - Automatic: Determines the optimal amount of coefficients by putting a spike and slab prior on them, with `Maximal lags` being the limit.

- Add local level component: Adds a trend component consisting of a mean following a random walk to the state (mu[t] = mu[t-1] + error)

- Add local linear trend component: Adds a trend components consisting of a mean(mu[t] = mu[t-1] + delta[t-1] + error) and a slope (delta[t] = delta[t-1]+error) which both follow a random walk.

- Add dynamic regression component: When variables were added to the `Model Terms` box this option can be used to make the regression coefficients dynamic/change over time where they follow a random walk.
  - Lag of coefficient: If the dynamic coefficient should follow an autoregressive process instead of a random walk, this option determines the order of the AR(p) process

#### Seasonalities
- This box can be used to add one or more seasonality components to the model
  - Name: Name of the seasonality. If not provided automatic name containing the number and duration of seasonality will be created.
  - Number: The total number of seasons to be included after which cycle starts again (e.g. 12 for monthly seasonality or 52 for weekly). Each season gets a dummy variable with dynamic coefficients following a random walk
  - Duration: The number of data points each season contains. For example if we have a weekly time series (i.e. 54 data points for 54 weeks) but want to model a monthly seasonality we would set the `Number` to 12 and the `Duration` to 4 as each season lasts/contains 4 time points.
  - Inverse gamma prior for σ: This prior determines the expectations regarding the σ of the random walk. Parameterised as a scaled inverse Chi-Squared distribution:
    - σ: Prior guess for the standard deviation
    - n: Prior observation count/degrees of freedom that weight the σ guess
  - Normal Prior initial state: This normal distribution determines our expectation regarding the first value of our seasonality which is then used by the Kalman filter:
    - μ: Mean of prior distribution
    - σ: Standard deviation of prior distribution

### Plots
---
#### State Plots
- Aggregated state contribution: Plots the aggregated state where all state components (such as local level or regression components) are added together
  - Credible Interval: Changes the percentile of MCMC draws that is used to plot the credible interval
  - Show observation: Adds the actual observations on top of the plot.
- Component state contribution: Plots the individual state contributions of each component

#### Residuals
- Posterior distribution of residuals: Plots the residuals of estimated state vs actual observations after applying the Kalman filter and smoother (i.e. going forward and backward in time)
- Posterior distribution of one-step-ahead prediction error: Plots the prediction error when the Kalman filter is used to estimate the next time point from all previous observations(i.e. going forward in time)

#### Prediction
- This option plots the posterior predictive distribution sampled from the specified state space model. As of now only possible without predictor variables.
  - Horizon: Determines the number of time points into the future that are predicted. Value needs to be larger than 0 to make predictions and show a plot.

#### Control chart
- Show control chart: This option plots the aggregated state but adds a critical threshold that can be used as a control chart. The threshold is calculated by mean(state) +/- L*sigma
  - Control period end: The end of the baseline period that is used to calculate the mean and sigma for the threshold
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

#### Posterior Summary of Coefficients
This table summarises the prior and posterior information of the predictor variables and their coefficients:
- Coefficients: Names of the predictor variables
- P(incl): The posterior probability that a variable is included
- P(incl|data): The posterior probability that a coefficient is positive and thus included in the model
- BF_inclusion: Quantifies how strongly our beliefs about the inclusion probability of each predictor changes.
- Mean: The mean of each coefficient across all MCMC draws where (regardless of whether variable was included as predictor)
- SD: Standard deviation of each coefficient across all MCMC draws
- Mean_inclusion: Mean of each coefficient across the MCMC draws where they were included in the model/positive
- SD_inclusion: Standard deviation of each coefficient given they were included
- Credible Interval: Credible Interval of the mean of each coefficient given they were included in the model

#### State Plots:
- Aggregated State: Time stamps are on the x-axis and the actual distribution of the estimated states on the y-axis. If the option `Actual observations` is selected, the real dependent variable values will be shown as points.
- Component State plot: Plots the individual contribution of each state contribution to the overall state. Time stamps are on the x-axis and the distribution of each component on the y axis. Each component has a single column where the label on the right corresponds to the component name. The blue shaded area reflects the credible interval.

#### Prediction Plot:
- Time stamps are on the x-axis and the actual distribution of the estimated states on the y-axis. The line before the dashed line are the actual observations. The dashed line represents the end of the data set and the start of the prediction. The black line after the dashed line represents the mean of the predicted state and the blue shaded area the 95 percent credible interval.

### References
- Scott, S. L. (2020). bsts: Bayesian Structural Time Series (0.9.5) [Computer software]. https://CRAN.R-project.org/package=bsts
- Scott, S. L., & Varian, H. R. (2014). Predicting the present with Bayesian structural time series. International Journal of Mathematical Modelling and Numerical Optimisation, 5(1/2), 4. https://doi.org/10.1504/IJMMNO.2014.059942
- Scott, S. L., & Varian, H. R. (2015). Bayesian Variable Selection for Nowcasting Economic Time Series. In Economic Analysis of the Digital Economy (pp. 119–135). University of Chicago Press. https://doi.org/10.7208/chicago/9780226206981.003.0004


### R Packages
- bsts
- ggplot2

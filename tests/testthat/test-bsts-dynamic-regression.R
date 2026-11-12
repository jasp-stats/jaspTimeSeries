.bstsDynamicRegressionTestData <- function() {
  step <- seq_len(60L)
  data.frame(y = 10 + .03 * step + sin(step / 2.7) + .4 * cos(step / 1.3),
             x = sin(step / 5) + step / 20,
             group = factor(rep(c("a", "b"), 30L)))
}

.bstsDynamicRegressionTestOptions <- function() {
  list(dependent = "y", covariates = character(), fixedFactors = character(),
       modelTerms = list(), autoregressiveComponent = FALSE,
       localLevelComponent = TRUE, localLinearTrendComponent = FALSE,
       semiLocalLinearTrendComponent = FALSE, dynamicRegressionComponent = TRUE,
       seasonalities = NULL, mcmcDraws = 60L, seed = 123,
       expectedPredictors = 1, mcmcTimeoutSeconds = 120)
}

test_that("BSTS ignores a retained dynamic regression setting without predictors", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  data <- .bstsDynamicRegressionTestData()
  dataBefore <- serialize(data, NULL)
  cases <- list(unassigned = .bstsDynamicRegressionTestOptions(),
                covariateWithoutTerms = .bstsDynamicRegressionTestOptions(),
                factorWithoutTerms = .bstsDynamicRegressionTestOptions(),
                removedAssignments = .bstsDynamicRegressionTestOptions())
  cases$covariateWithoutTerms$covariates <- "x"
  cases$factorWithoutTerms$fixedFactors <- "group"
  # A saved or intermediate option state may still contain old model terms.
  cases$removedAssignments$modelTerms <- list(list(components = "x", isNuisance = FALSE))

  for (options in cases) {
    optionsBefore <- serialize(options, NULL)
    set.seed(123)
    actual <- fit(data, options)
    off <- options
    off$dynamicRegressionComponent <- FALSE
    set.seed(123)
    expected <- fit(data, off)

    expect_s3_class(actual, "bsts")
    expect_length(actual$state.specification, 1L)
    expect_s3_class(actual$state.specification[[1L]], "LocalLevel")
    for (name in c("sigma.obs", "state.contributions", "one.step.prediction.errors",
                   "log.likelihood", "final.state"))
      expect_identical(actual[[name]], expected[[name]])
    expect_identical(serialize(options, NULL), optionsBefore)
    expect_identical(serialize(data, NULL), dataBefore)
  }
})

test_that("BSTS still fits dynamic regression for covariates and fixed factors", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  data <- .bstsDynamicRegressionTestData()
  dataBefore <- serialize(data, NULL)
  for (predictor in c("x", "group")) {
    options <- .bstsDynamicRegressionTestOptions()
    if (predictor == "x")
      options$covariates <- predictor
    else
      options$fixedFactors <- predictor
    options$modelTerms <- list(list(components = predictor, isNuisance = FALSE))
    optionsBefore <- serialize(options, NULL)
    set.seed(123)
    actual <- fit(data, options)

    # Reference reproduces model construction before the no-predictors guard.
    set.seed(123)
    formula <- stats::reformulate(predictor, response = "y")
    states <- bsts::AddLocalLevel(list(), data$y)
    states <- bsts::AddDynamicRegression(states, formula = formula, data = data)
    expected <- bsts::bsts(
      formula = formula, data = data, state.specification = states,
      niter = options$mcmcDraws, ping = 0, timestamps = NULL, seed = options$seed,
      expected.model.size = options$expectedPredictors,
      model.options = bsts::BstsOptions(timeout.seconds = options$mcmcTimeoutSeconds))

    expect_length(actual$state.specification, 2L)
    expect_s3_class(actual$state.specification[[2L]], "DynamicRegression")
    for (name in c("sigma.obs", "state.contributions", "one.step.prediction.errors",
                   "log.likelihood", "final.state", "coefficients"))
      expect_identical(actual[[name]], expected[[name]])
    expect_identical(serialize(options, NULL), optionsBefore)
    expect_identical(serialize(data, NULL), dataBefore)

    off <- options
    off$dynamicRegressionComponent <- FALSE
    static <- fit(data, off)
    expect_length(static$state.specification, 1L)
    expect_s3_class(static$state.specification[[1L]], "LocalLevel")
    expect_true("coefficients" %in% names(static))
  }
})

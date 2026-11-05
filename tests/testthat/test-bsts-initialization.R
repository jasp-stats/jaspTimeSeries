.bstsInitializationTestData <- function() {
  step <- seq_len(80L)
  predictor <- -sin(step / 3)
  data.frame(y = 20 - 3 * predictor + .1 * cos(step * .73) + .03 * step,
             x_negative = predictor, x_noise = cos(step * 1.7))
}

.bstsInitializationTestOptions <- function(component = "localLevel", regression = FALSE) {
  predictors <- if (regression) c("x_negative", "x_noise") else character()
  list(dependent = "y", covariates = predictors, fixedFactors = character(),
       modelTerms = lapply(predictors, function(x) list(components = x, isNuisance = FALSE)),
       autoregressiveComponent = FALSE, localLevelComponent = component == "localLevel",
       localLinearTrendComponent = component == "localLinearTrend",
       semiLocalLinearTrendComponent = component == "semilocalTrend",
       dynamicRegregressionComponent = FALSE, seasonalities = NULL,
       samples = 40L, seed = 123, expectedPredictors = 1, timeout = 120)
}

test_that("BSTS fits level components without removing missing time steps", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  patterns <- list(leading = c(1L, 2L), internal = 37L,
                   trailing = c(79L, 80L), mixed = c(1L, 2L, 37L, 79L, 80L))
  for (component in c("localLevel", "semilocalTrend")) {
    for (regression in c(FALSE, TRUE)) {
      options <- .bstsInitializationTestOptions(component, regression)
      optionsBefore <- serialize(options, NULL)
      for (pattern in names(patterns)) {
        missing <- patterns[[pattern]]
        data <- .bstsInitializationTestData()
        data$y[missing] <- NA_real_
        dataBefore <- serialize(data, NULL)
        set.seed(123)
        result <- fit(data, options)

        expect_s3_class(result, "bsts")
        expect_equal(result$niter, options$samples)
        expect_identical(as.numeric(result$original.series), data$y)
        expect_equal(dim(result$state.contributions)[3L], nrow(data))
        priorName <- if (component == "localLevel") "initial.state.prior" else "initial.level.prior"
        prior <- result$state.specification[[1L]][[priorName]]
        # The first observed position is known for these fixtures; missing
        # positions still belong to the fitted series, including those before it.
        firstObserved <- if (pattern %in% c("leading", "mixed")) 3L else 1L
        expect_equal(prior$mu, data$y[firstObserved])
        expect_equal(prior$sigma, stats::sd(data$y, na.rm = TRUE))

        residuals <- bsts::residuals.bsts(result, burn = 10L)
        expect_equal(dim(residuals), c(options$samples - 10L, nrow(data)))
        expect_identical(unname(which(colSums(!is.na(residuals)) == 0L)), missing)
        expect_true(all(is.finite(residuals[, -missing, drop = FALSE])))
        expect_true(all(is.finite(result$sigma.obs)))
        expect_identical(serialize(data, NULL), dataBefore)
        expect_identical(serialize(options, NULL), optionsBefore)
      }
    }
  }
})

test_that("BSTS complete-data level priors and posterior draws remain unchanged", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  data <- .bstsInitializationTestData()
  for (component in c("localLevel", "semilocalTrend")) {
    for (regression in c(FALSE, TRUE)) {
      options <- .bstsInitializationTestOptions(component, regression)
      set.seed(123)
      actual <- fit(data, options)

      set.seed(123)
      # Reproduce the original constructor calls on complete data, independently
      # of the module's initialization preparation.
      state <- if (component == "localLevel")
        bsts::AddLocalLevel(list(), y = data$y)
      else
        bsts::AddSemilocalLinearTrend(list(), y = data$y)
      formula <- if (regression) stats::reformulate(options$covariates, response = "y") else data$y
      expected <- bsts::bsts(
        formula = formula, data = data, state.specification = state,
        niter = options$samples, ping = 0, timestamps = NULL, seed = options$seed,
        expected.model.size = options$expectedPredictors,
        model.options = bsts::BstsOptions(timeout.seconds = options$timeout))

      for (name in c("state.specification", "prior", "original.series", "sigma.obs",
                     "state.contributions", "one.step.prediction.errors",
                     "log.likelihood", "final.state", if (regression) "coefficients"))
        expect_identical(actual[[name]], expected[[name]])
    }
  }
})

test_that("BSTS residual plotting works after fitting a response with leading gaps", {
  data <- .bstsInitializationTestData()
  missing <- c(1L, 2L, 37L, 79L, 80L)
  data$y[missing] <- NA_real_
  options <- .bstsInitializationTestOptions(regression = TRUE)
  set.seed(123)
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")(data, options)
  residuals <- bsts::residuals.bsts(fit, burn = 10L)

  builder <- getFromNamespace(".bstsResidualPlot", "jaspTimeSeries")
  scope <- new.env(parent = environment(builder))
  # Collect an ordinary R plot object without constructing native result objects.
  scope$createJaspPlot <- function(...) new.env(parent = emptyenv())
  environment(builder) <- scope
  plots <- new.env(parent = emptyenv())
  time <- seq_len(nrow(data))
  builder(plots, fit, list(residualPlot = TRUE, burn = 10L, time = time,
                          aggregatedStatesPlotCiLevel = .95), ready = TRUE)
  built <- ggplot2::ggplot_build(plots$bstsResidualPlot$plotObject)
  ribbon <- built$data[[1L]]
  line <- built$data[[2L]]
  expect_equal(line$x, time)
  expect_equal(ribbon$x, time)
  expect_identical(which(is.na(line$y)), missing)
  expect_identical(which(is.na(ribbon$ymin)), missing)
  expect_identical(which(is.na(ribbon$ymax)), missing)
  observed <- setdiff(time, missing)
  expect_equal(line$y[observed], unname(colMeans(residuals[, observed, drop = FALSE])))
  expect_equal(ribbon$ymin[observed], unname(apply(residuals[, observed, drop = FALSE], 2,
                                                stats::quantile, probs = .025)))
  expect_equal(ribbon$ymax[observed], unname(apply(residuals[, observed, drop = FALSE], 2,
                                                stats::quantile, probs = .975)))
})

test_that("BSTS leaves the local-linear-trend prior's original time span intact", {
  data <- .bstsInitializationTestData()
  data$y[c(1L, 2L, 37L, 79L, 80L)] <- NA_real_
  options <- .bstsInitializationTestOptions("localLinearTrend", regression = TRUE)
  set.seed(123)
  actual <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")(data, options)
  # This constructor's default initial slope uses the original series length.
  expected <- bsts::AddLocalLinearTrend(list(), y = data$y)
  expect_identical(actual$state.specification, expected)
  expect_identical(as.numeric(actual$original.series), data$y)
})

test_that("BSTS level initialization explains insufficient observed responses", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  for (component in c("localLevel", "semilocalTrend")) {
    for (observed in c(0L, 1L)) {
      data <- .bstsInitializationTestData()
      data$y[] <- NA_real_
      if (observed == 1L) data$y[3L] <- 20
      options <- .bstsInitializationTestOptions(component)
      error <- tryCatch(fit(data, options), error = identity)
      expect_s3_class(error, "validationError")
      expect_identical(conditionMessage(error),
        "At least two observed values of the dependent variable are required to initialize a local level or semilocal trend component.")
    }
  }
})

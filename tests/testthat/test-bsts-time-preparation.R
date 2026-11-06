.bstsTimePreparationOptions <- function(regression = FALSE, dynamic = FALSE) {
  predictors <- if (regression) c("x", "group") else character()
  list(dependent = "y", time = "Time",
       covariates = if (regression) "x" else character(),
       fixedFactors = if (regression) "group" else character(),
       modelTerms = lapply(predictors, function(x) list(components = x, isNuisance = FALSE)),
       autoregressiveComponent = FALSE, localLevelComponent = TRUE,
       localLinearTrendComponent = FALSE, semiLocalLinearTrendComponent = FALSE,
       dynamicRegressionComponent = dynamic, seasonalities = NULL,
       mcmcDraws = 30L, seed = 123, expectedPredictors = 1,
       mcmcTimeoutSeconds = 120, predictionHorizon = 0L)
}

.bstsTimePreparationFixture <- function() {
  step <- seq_len(48L)
  group <- factor(rep(c("control", "treatment"), length.out = 48L),
                  levels = c("treatment", "control"))
  predictor <- sin(step / 4)
  data.frame(y = 20 + .03 * step - 1.4 * predictor +
               .9 * (group == "treatment") + .12 * cos(step * .8),
             Time = -3 + cumsum(rep(c(.1, .3, .2), 16L)),
             x = predictor, group = group,
             row.names = as.character(2L * step))
}

test_that("BSTS Time preparation sorts full rows without inferring gaps", {
  prepare <- getFromNamespace(".bstsPrepareData", "jaspTimeSeries")
  options <- .bstsTimePreparationOptions(regression = TRUE)
  # Spreadsheet identities intentionally differ from both input and Time order.
  input <- data.frame(y = c(NA_real_, 10, 20, NA_real_, 50),
                      Time = c(2001.8, -2.5, 2001.5, 2001.6, 4000),
                      x = c(3, 1, 2, 4, 5),
                      group = factor(c("b", "a", "c", "a", "b"),
                                     levels = c("c", "a", "b", "unused")),
                      extra = c("three", "one", "two", "four", "five"),
                      row.names = c("12", "4", "18", "7", "25"))
  before <- serialize(input, NULL)
  optionsBefore <- serialize(options, NULL)
  expected <- input[c(2L, 3L, 4L, 1L, 5L), , drop = FALSE]
  actual <- prepare(NULL, input, options, TRUE)

  expect_identical(actual$data, expected)
  expect_identical(actual$time, c(-2.5, 2001.5, 2001.6, 2001.8, 4000))
  expect_identical(which(is.na(actual$data$y)), c(3L, 4L))
  expect_identical(levels(actual$data$group), levels(input$group))
  expect_identical(serialize(input, NULL), before)
  expect_identical(serialize(options, NULL), optionsBefore)
  expect_identical(prepare(NULL, actual$data, options, TRUE), actual)
})

test_that("BSTS Time preparation preserves tied observation order and column classes", {
  prepare <- getFromNamespace(".bstsPrepareData", "jaspTimeSeries")
  input <- data.frame(y = c(10, 20, 30, 40, 50),
                      Time = c(2, 1, 2, 1, 3),
                      group = ordered(c("low", "high", "low", "high", "low")),
                      row.names = c("20", "2", "8", "17", "4"))
  expected <- input[c(2L, 4L, 1L, 3L, 5L), , drop = FALSE]
  actual <- prepare(NULL, input, .bstsTimePreparationOptions(), TRUE)
  expect_identical(actual$data, expected)
  expect_identical(actual$time, c(1, 1, 2, 2, 3))
  expect_identical(rownames(actual$data), c("2", "17", "20", "8", "4"))
  expect_true(is.ordered(actual$data$group))
})

test_that("BSTS Time accepts numeric text and unambiguous dates without changing step count", {
  prepare <- getFromNamespace(".bstsPrepareData", "jaspTimeSeries")
  dateText <- c("2020-03-17 12:00:00", "2020-01-10 12:00:00", "2020-02-01 12:00:00")
  dates <- as.POSIXct(dateText, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  cases <- list(
    list(time = c("30", "-1.5", "2"), expected = c(-1.5, 2, 30)),
    list(time = factor(c("30", "-1.5", "2")), expected = c(-1.5, 2, 30)),
    list(time = dateText, expected = dates[c(2L, 3L, 1L)]),
    list(time = factor(dateText), expected = dates[c(2L, 3L, 1L)]),
    list(time = dates, expected = dates[c(2L, 3L, 1L)]),
    list(time = as.Date(dates), expected = as.POSIXct(as.Date(dates[c(2L, 3L, 1L)]), tz = "UTC")))
  for (case in cases) {
    input <- data.frame(y = c(30, 10, 20), Time = case$time,
                        row.names = c("9", "4", "12"))
    actual <- prepare(NULL, input, .bstsTimePreparationOptions(), TRUE)
    expect_identical(actual$data, input[c(2L, 3L, 1L), , drop = FALSE])
    expect_equal(actual$time, case$expected)
    expect_identical(nrow(actual$data), 3L)
  }
})

test_that("BSTS unusable or absent Time retains spreadsheet order and response gaps", {
  prepare <- getFromNamespace(".bstsPrepareData", "jaspTimeSeries")
  cases <- list(c("France", "Germany", "Italy"),
                c("01-10", "02-10", "03-10"),
                c("03/02/2020", "01/02/2020", "02/02/2020"),
                c("2020-01-01", "not-a-date", "2020-01-03"),
                c(3, NA_real_, 1), c(3, Inf, 1))
  for (time in cases) {
    input <- data.frame(y = c(10, NA_real_, 20), Time = time,
                        row.names = c("8", "2", "19"))
    actual <- prepare(NULL, input, .bstsTimePreparationOptions(), TRUE)
    expect_identical(actual$data, input)
    expect_identical(actual$time, seq_len(3L))
  }
  options <- .bstsTimePreparationOptions()
  options$time <- ""
  input <- data.frame(y = c(10, NA_real_, 20), row.names = c("8", "2", "19"))
  actual <- prepare(NULL, input, options, TRUE)
  expect_identical(actual$data, input)
  expect_identical(actual$time, seq_len(3L))
})

test_that("BSTS preparation clears stale Time notes when unassigned or not ready", {
  prepare <- getFromNamespace(".bstsPrepareData", "jaspTimeSeries")
  noteNames <- c("timeSortingNote", "timeInterpretationNote", "timeWarning",
                 "timeTiesNote", "timeFilterWarning")
  for (ready in c(TRUE, FALSE)) {
    # Ordinary R collector: these branches clear notes and create no C++ output.
    results <- list2env(setNames(as.list(rep("stale note", length(noteNames))), noteNames),
                       parent = emptyenv())
    options <- .bstsTimePreparationOptions()
    options$time <- ""
    input <- if (ready) data.frame(y = c(3, NA_real_, 2)) else NULL
    actual <- prepare(results, input, options, ready)
    for (name in noteNames) expect_null(results[[name]])
    expect_length(actual$time, if (ready) 3L else 0L)
  }
})

test_that("BSTS entry validates covariates against original rows before using sorted data", {
  raw <- data.frame(y = c(10, 20, 30), Time = c(3, 1, 2), x = c(NA, "2", "3"))
  namespace <- asNamespace("jaspTimeSeries")
  scope <- new.env(parent = namespace)
  scope$.readDataSetToEnd <- function(...) jaspBase::.vdf(raw, ...)
  for (name in c("bayesianStateSpaceInternal", ".bstsReadData", ".bstsErrorHandling")) {
    fun <- get(name, namespace)
    environment(fun) <- scope
    scope[[name]] <- fun
  }
  options <- .bstsTimePreparationOptions()
  options$covariates <- "x"
  # With a NULL results collector, preparation is data-only. Validation must stop
  # before fitting/creating output. Comparing a sorted numeric mask against an
  # unsorted diagnostic factor reread would falsely call this nonnumeric input.
  error <- tryCatch(scope$bayesianStateSpaceInternal(NULL, NULL, options),
                    validationError = identity)
  expect_s3_class(error, "validationError")
  expect_match(conditionMessage(error), "covariates contain missing values: x.", fixed = TRUE)
  expect_false(grepl("cannot be interpreted", conditionMessage(error), fixed = TRUE))
})

test_that("BSTS ordered and shuffled inputs yield the same seeded model with response gaps", {
  prepare <- getFromNamespace(".bstsPrepareData", "jaspTimeSeries")
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  expectedData <- .bstsTimePreparationFixture()
  missing <- c(1L, 2L, 17L, 47L, 48L)
  expectedData$y[missing] <- NA_real_
  permutation <- c(seq(2L, 48L, 2L), seq(1L, 47L, 2L))
  dateText <- format(as.Date("2020-01-01") + cumsum(rep(c(1, 3, 7), 16L)))
  for (configuration in c("response", "regression", "dynamic")) {
    options <- .bstsTimePreparationOptions(configuration != "response", configuration == "dynamic")
    set.seed(123)
    reference <- fit(expectedData, options)
    for (time in list(expectedData$Time, dateText)) {
      input <- expectedData
      input$Time <- time
      input <- input[permutation, , drop = FALSE]
      before <- serialize(input, NULL)
      prepared <- prepare(NULL, input, options, TRUE)
      set.seed(123)
      actual <- fit(prepared$data, options)

      expect_s3_class(actual, "bsts")
      expect_identical(as.numeric(actual$original.series), expectedData$y)
      expect_equal(dim(actual$state.contributions)[3L], nrow(expectedData))
      for (name in c("sigma.obs", "state.contributions", "one.step.prediction.errors",
                     "log.likelihood", "final.state",
                     if (configuration != "response") "coefficients"))
        expect_identical(actual[[name]], reference[[name]])
      residuals <- bsts::residuals.bsts(actual, burn = 10L)
      expect_identical(unname(which(colSums(!is.na(residuals)) == 0L)), missing)
      expect_identical(prepared$data$group, expectedData$group)
      expect_identical(prepared$data$x, expectedData$x)
      # Reading row names may materialize an internal ALTREP representation;
      # values and attributes must remain identical to the independent snapshot.
      expect_identical(input, unserialize(before))
    }
  }
})

.bstsTimePreparationRun <- function(input, time = TRUE) {
  options <- jaspTools:::readQML(system.file("qml", "bayesianStateSpace.qml", package = "jaspTimeSeries"))
  options$dependent <- list(types = "scale", value = "y")
  options$time <- if (time)
    list(types = if (is.numeric(input$Time)) "scale" else "nominal", value = "Time")
  else list(types = list(), value = "")
  options$covariates <- options$fixedFactors <- list(types = list(), value = list())
  options$modelTerms <- options$seasonalities <- list()
  options$mcmcDraws <- 30L
  options$seed <- 123L
  options$burninMethod <- "manual"
  options$burninManualDraws <- 5L
  options$aggregatedStatesPlot <- options$residualPlot <- TRUE
  options$predictionHorizon <- 2L
  encoded <- jaspTools:::encodeOptionsAndDataset(jaspTools:::fixOptionsForVariableTypes(options), input)
  rownames(encoded$dataset) <- rownames(input)
  set.seed(123)
  result <- jaspTools::runAnalysis("bayesianStateSpace", dataset = encoded$dataset,
                                  options = encoded$options, encodedDataset = TRUE,
                                  view = FALSE, makeTests = FALSE)
  expect_identical(result$status, "complete")
  expect_false(isTRUE(result$results$error))
  fits <- Filter(function(value) inherits(value, "bsts"), result$state$other)
  expect_length(fits, 1L)
  list(result = result, fit = fits[[1L]])
}

.bstsTimePreparationPlot <- function(run, key, container = NULL) {
  collection <- run$result$results$bstsMainContainer$collection
  prefix <- "bstsMainContainer"
  if (!is.null(container)) {
    prefix <- paste(prefix, container, sep = "_")
    collection <- collection[[prefix]]$collection
  }
  record <- collection[[paste(prefix, key, sep = "_")]]
  expect_identical(record$status, "complete")
  plot <- run$result$state$figures[[record$data]]$obj
  expect_s3_class(plot, "ggplot")
  plot
}

.bstsExpectTimePreparationPlots <- function(run, time, expectedY) {
  fit <- run$fit
  draws <- fit$state.contributions[6L:30L, , , drop = FALSE]
  stateMean <- colMeans(rowSums(aperm(draws, c(1L, 3L, 2L)), dims = 2L))
  residualMean <- colMeans(bsts::residuals.bsts(fit, burn = 5L))
  aggregated <- ggplot2::ggplot_build(.bstsTimePreparationPlot(run, "bstsAggregatedStatePlot", "bstsStatePlots"))
  residual <- ggplot2::ggplot_build(.bstsTimePreparationPlot(run, "bstsResidualPlot", "bstsErrorPlots"))
  expect_equal(aggregated$data[[2L]]$x, as.numeric(time))
  expect_equal(aggregated$data[[2L]]$y, unname(stateMean), tolerance = 1e-12)
  expect_equal(residual$data[[2L]]$x, as.numeric(time))
  expect_equal(residual$data[[2L]]$y, unname(residualMean), tolerance = 1e-12)
  expect_identical(which(is.na(residual$data[[2L]]$y)), which(is.na(expectedY)))

  forecast <- .bstsTimePreparationPlot(run, "bstsPredictionPlot")
  expect_equal(forecast$data$time, seq_len(length(expectedY) + 2L))
  expect_equal(forecast$data$mean[seq_along(expectedY)], expectedY)
  expect_equal(which(is.na(forecast$data$mean)), which(is.na(expectedY)))
}

test_that("BSTS analysis uses prepared numeric and date order in fitting, notes and plots", {
  ordered <- .bstsTimePreparationFixture()
  ordered$y[c(1L, 2L, 17L, 47L, 48L)] <- NA_real_
  reference <- .bstsTimePreparationRun(ordered, time = FALSE)
  permutation <- c(seq(2L, 48L, 2L), seq(1L, 47L, 2L))
  dates <- as.Date("2020-01-01") + cumsum(rep(c(1, 3, 7), 16L))
  for (time in list(ordered$Time, as.character(dates))) {
    input <- ordered
    input$Time <- time
    input <- input[permutation, , drop = FALSE]
    run <- .bstsTimePreparationRun(input)
    # The supported reader serializes doubles; compare input values within
    # rounding precision, then require exact equality of the two fitted models.
    expect_equal(as.numeric(run$fit$original.series), ordered$y, tolerance = 1e-12)
    for (name in c("sigma.obs", "state.contributions", "one.step.prediction.errors",
                   "log.likelihood", "final.state"))
      expect_identical(run$fit[[name]], reference$fit[[name]])
    expect_match(run$result$results$timeSortingNote$rawtext, "sorted by Time", fixed = TRUE)
    expect_null(run$result$results$timeWarning)
    expectedTime <- if (is.numeric(time)) time else as.POSIXct(dates, tz = "UTC")
    if (is.numeric(time)) {
      expect_null(run$result$results$timeInterpretationNote)
    } else {
      expect_match(run$result$results$timeInterpretationNote$rawtext,
                   "year-month-day (YYYY-MM-DD)", fixed = TRUE)
    }
    .bstsExpectTimePreparationPlots(run, expectedTime, ordered$y)
    expect_identical(.bstsTimePreparationPlot(run, "bstsPredictionPlot")$labels$x, "Observation step")
  }
  expect_null(reference$result$results$timeSortingNote)
  expect_null(reference$result$results$timeInterpretationNote)
  expect_identical(.bstsTimePreparationPlot(reference, "bstsPredictionPlot")$labels$x, "Time")
})

test_that("BSTS analysis reports ignored Time and fits spreadsheet order after fallback", {
  input <- .bstsTimePreparationFixture()
  input$y[c(1L, 2L, 17L, 47L, 48L)] <- NA_real_
  input <- input[c(seq(2L, 48L, 2L), seq(1L, 47L, 2L)), , drop = FALSE]
  reference <- .bstsTimePreparationRun(input, time = FALSE)
  cases <- list(
    list(time = rep(c("France", "Germany", "Italy"), 16L), text = "cannot be interpreted as numbers or dates"),
    list(time = rep(c("01-10", "02-10", "03-10"), 16L), text = "date format in Time is ambiguous"),
    list(time = replace(input$Time, 7L, NA_real_), text = "missing or non-finite values"))
  for (case in cases) {
    input$Time <- case$time
    run <- .bstsTimePreparationRun(input)
    expect_equal(as.numeric(run$fit$original.series), input$y, tolerance = 1e-12)
    for (name in c("sigma.obs", "state.contributions", "one.step.prediction.errors",
                   "log.likelihood", "final.state"))
      expect_identical(run$fit[[name]], reference$fit[[name]])
    expect_match(run$result$results$timeWarning$rawtext, case$text, fixed = TRUE)
    expect_match(run$result$results$timeWarning$rawtext, "spreadsheet row order", fixed = TRUE)
    expect_null(run$result$results$timeSortingNote)
    expect_null(run$result$results$timeInterpretationNote)
    .bstsExpectTimePreparationPlots(run, seq_len(nrow(input)), input$y)
  }
})

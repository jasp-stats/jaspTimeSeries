# Supported analysis integration: expected orders are specified independently,
# and reference analyses leave Time unassigned. No loaded namespace is changed.
arimaPreparationRun <- function(input, time = TRUE, covariates = character(), ...) {
  options <- jaspTools:::readQML(system.file("qml", "ARIMATimeSeries.qml", package = "jaspTimeSeries"))
  options$dependent <- list(types = "scale", value = "y")
  options$time <- if (time) list(types = if (is.numeric(input$Time)) "scale" else "nominal", value = "Time") else
    list(types = list(), value = "")
  options$covariates <- list(types = rep("scale", length(covariates)), value = covariates)
  options$modelSpecification <- "custom"
  options$p <- 1L
  options$d <- options$q <- 0L
  options$intercept <- TRUE
  options$seasonal <- FALSE
  # These two plots depend on the separately versioned shared graph recipe API.
  # Forecasts use the existing independent plot path, which is exercised below.
  options$timeSeriesPlot <- options$residualTimeSeries <- FALSE
  options$residualAcf <- options$residualQQ <- options$residualLjungBox <- FALSE
  options$residualSavedToData <- FALSE
  options$residualColumn <- ""
  options$forecastSave <- ""
  options$forecastLength <- 3L
  options$forecastTable <- options$forecastTimeSeries <- options$forecastTimeSeriesObserved <- TRUE
  options <- utils::modifyList(options, list(...))
  encoded <- jaspTools:::encodeOptionsAndDataset(jaspTools:::fixOptionsForVariableTypes(options), input)
  # Model the native global-filter contract: supplied rows keep spreadsheet IDs.
  rownames(encoded$dataset) <- rownames(input)
  result <- jaspTools::runAnalysis("ARIMATimeSeries", dataset = encoded$dataset,
                                  options = encoded$options, encodedDataset = TRUE, view = FALSE)
  expect_identical(result$status, "complete")
  expect_false(isTRUE(result$results$error))
  # The runtime drops unassigned columns before encoding. Map selected names
  # through their encoded options, not through positions in the input frame.
  columnNames <- setNames(c("y", if (time) "Time", covariates),
    c(encoded$options$dependent, if (time) encoded$options$time, encoded$options$covariates))
  list(result = result, options = encoded$options, dataset = encoded$dataset,
       columnNames = columnNames)
}

arimaPreparationFit <- function(run) {
  fits <- Filter(function(value) inherits(value, "Arima"), run$result$state$other)
  expect_length(fits, 1L)
  fits[[1L]]
}

arimaPreparationCoefficients <- function(run) {
  rows <- run$result$results$coefTable$data
  # runAnalysis retains encoded variable labels in raw results. Decode only
  # those labels, without dropping row grouping or coefficient metadata.
  lapply(rows, function(row) {
    if (row$coefficients %in% names(run$columnNames))
      row$coefficients <- unname(run$columnNames[row$coefficients])
    row
  })
}

arimaPreparationForecast <- function(run) {
  rows <- run$result$results$forecastTable$data
  columns <- c("y", "lower80", "upper80", "lower95", "upper95")
  setNames(lapply(columns, function(name) vapply(rows, `[[`, numeric(1L), name)), columns)
}

arimaPreparationForecastPlot <- function(run) {
  id <- run$result$results$forecastPlot$data
  expect_length(id, 1L)
  plot <- run$result$state$figures[[id]]$obj
  expect_s3_class(plot, "ggplot")
  plot
}

expectArimaPreparedLike <- function(actual, reference, expectedY) {
  fit <- arimaPreparationFit(actual)
  expectedFit <- arimaPreparationFit(reference)
  fields <- c("coef", "sigma2", "loglik", "aic", "aicc", "bic", "arma", "residuals", "x", "fitted")
  expect_equal(fit[fields], expectedFit[fields], tolerance = 1e-10)
  expect_equal(as.numeric(fit$x), expectedY, tolerance = 1e-12)
  expect_equal(actual$result$results$modelTable$data,
               reference$result$results$modelTable$data, tolerance = 1e-10)
  expect_equal(arimaPreparationCoefficients(actual), arimaPreparationCoefficients(reference), tolerance = 1e-10)
  expect_equal(actual$result$results$coefTable$schema, reference$result$results$coefTable$schema)
  expect_equal(actual$result$results$coefTable$footnotes, reference$result$results$coefTable$footnotes)
  expect_equal(arimaPreparationForecast(actual), arimaPreparationForecast(reference), tolerance = 1e-10)

  plot <- arimaPreparationForecastPlot(actual)
  referencePlot <- arimaPreparationForecastPlot(reference)
  expect_equal(plot$data$y, referencePlot$data$y, tolerance = 1e-10)
  expect_equal(plot$data$y[seq_along(expectedY)], expectedY, tolerance = 1e-12)
  expect_equal(plot$data$t, seq_len(length(expectedY) + actual$options$forecastLength))
  expect_identical(plot$scales$get_scales("x")$name, "Observation step")
  for (layer in 1:2) {
    expect_equal(plot$layers[[layer]]$data[, c("y", "lower80", "upper80", "lower95", "upper95")],
                 referencePlot$layers[[layer]]$data[, c("y", "lower80", "upper80", "lower95", "upper95")],
                 tolerance = 1e-10)
    expect_equal(plot$layers[[layer]]$data$t, length(expectedY) + seq_len(actual$options$forecastLength))
  }
  expect_identical(vapply(actual$result$results$forecastTable$data, `[[`, "", "t"),
                   as.character(seq_len(actual$options$forecastLength)))
  expect_identical(actual$result$results$forecastTable$schema$fields[[1L]]$title, "Steps ahead")
}

arimaPreparationFixture <- function() {
  k <- 1:48
  data.frame(y = 10 + .6 * sin(k / 5) - .3 * cos(k / 9) + sin(k / 3) + ((k * 7) %% 11) / 20,
             Time = -3 + cumsum(rep(c(.1, .3, .2), 16)),
             x1 = sin(k / 5), x2 = cos(k / 9))
}

test_that("ARIMA orders numeric and irregular date Time before fitting and forecasting", {
  data <- arimaPreparationFixture()
  reference <- arimaPreparationRun(data, time = FALSE)
  permutation <- c(seq(2, 48, 2), seq(1, 47, 2))
  dates <- format(as.Date("2020-01-01") + cumsum(rep(c(1, 3, 7), 16)))
  for (time in list(data$Time, dates)) {
    data$Time <- time
    input <- data[permutation, ]
    rownames(input) <- seq_len(nrow(input))
    actual <- arimaPreparationRun(input)
    expectArimaPreparedLike(actual, reference, data$y)
    note <- paste(unlist(actual$result$results$timeSortingNote, use.names = FALSE), collapse = " ")
    expect_match(note, "sorted by Time", fixed = TRUE)
  }
})

test_that("ARIMA retains internal and trailing missing responses as modeled steps", {
  data <- arimaPreparationFixture()
  data$y[c(7, 19, 47, 48)] <- NA_real_
  input <- data[c(25:48, 1:24), ]
  rownames(input) <- seq_len(nrow(input))
  actual <- arimaPreparationRun(input, p = 0L, d = 1L, intercept = FALSE)
  reference <- arimaPreparationRun(data, time = FALSE, p = 0L, d = 1L, intercept = FALSE)
  expectArimaPreparedLike(actual, reference, data$y)
  fit <- arimaPreparationFit(actual)
  expect_identical(which(is.na(fit$x)), c(7L, 19L, 47L, 48L))
  expected <- forecast::forecast(fit, h = 3L)
  forecast <- arimaPreparationForecast(actual)
  expect_equal(forecast$y, as.numeric(expected$mean), tolerance = 1e-10)
  # In a random walk, the two trailing unobserved steps also increase variance.
  expect_equal(forecast$upper95 - forecast$y,
               stats::qnorm(.975) * sqrt((2 + 1:3) * fit$sigma2), tolerance = 1e-10)
})

test_that("ARIMA uses numeric and date filters after ordering, without adding rows", {
  data <- arimaPreparationFixture()
  permutation <- c(seq(2, 48, 2), seq(1, 47, 2))
  expected <- data[9:38, ]
  reference <- arimaPreparationRun(expected, time = FALSE)
  input <- data[permutation, ]
  rownames(input) <- seq_len(nrow(input))
  actual <- arimaPreparationRun(input, filter = TRUE, filterBy = "time",
                                timeStart = data$Time[9], timeEnd = data$Time[38])
  expectArimaPreparedLike(actual, reference, expected$y)

  dates <- format(as.Date("2020-01-01") + cumsum(rep(c(1, 3, 7), 16)))
  data$Time <- dates
  input <- data[permutation, ]
  rownames(input) <- seq_len(nrow(input))
  actual <- arimaPreparationRun(input, filter = TRUE, filterBy = "date",
                                dateStart = dates[9], dateEnd = dates[38])
  expectArimaPreparedLike(actual, reference, expected$y)
})

test_that("one or two future regressors follow the final filtered row even across a Time tie", {
  data <- arimaPreparationFixture()
  data$Time <- rep(seq(-1.2, -.1, by = .1), each = 4)
  data$y[c(11, 37:48)] <- NA_real_
  # Shuffle whole groups so tied values keep their known relative order. The
  # selected 38 rows stop halfway through a tie; rows 39 and 40 are future rows.
  groups <- c(6, 2, 9, 1, 4, 3, 8, 5, 7)
  permutation <- c(unlist(lapply(groups, function(group) (4 * group - 3):(4 * group))), 37:48)
  input <- data[permutation, ]
  rownames(input) <- as.character(seq(2, 96, 2))
  for (covariates in list("x1", c("x1", "x2"))) {
    actual <- arimaPreparationRun(input, covariates = covariates, filter = TRUE,
                                  filterBy = "row", rowStart = 2L, rowEnd = 76L)
    reference <- arimaPreparationRun(data, time = FALSE, covariates = covariates,
                                     filter = TRUE, filterBy = "row", rowStart = 1L, rowEnd = 38L)
    expectArimaPreparedLike(actual, reference, data$y[1:38])
    fit <- arimaPreparationFit(actual)
    expect_equal(unname(fit$xreg), unname(as.matrix(data[1:38, covariates, drop = FALSE])), tolerance = 1e-12)
    expectedXreg <- as.matrix(data[39:41, covariates, drop = FALSE])
    colnames(expectedXreg) <- colnames(fit$xreg)
    expected <- forecast::forecast(fit, h = 3L, xreg = expectedXreg)
    forecast <- arimaPreparationForecast(actual)
    expect_equal(forecast$y, as.numeric(expected$mean), tolerance = 1e-10)
    expect_equal(forecast$lower95, as.numeric(expected$lower[, "95%"]), tolerance = 1e-10)
    expect_equal(forecast$upper95, as.numeric(expected$upper[, "95%"]), tolerance = 1e-10)
    expect_identical(vapply(arimaPreparationCoefficients(actual), `[[`, "", "coefficients"),
                     c("Intercept", "AR(1)", covariates))
    expect_match(paste(unlist(actual$result$results$timeTiesNote), collapse = " "),
                 "successive steps", fixed = TRUE)

    # Real preparation and the real supported-runtime fit feed the unchanged
    # saving helper. Only the column sink is collected; this is not a Desktop
    # persistent-state or spreadsheet-write test.
    prepared <- jaspDescriptives::.tsPrepareData(NULL, actual$dataset, actual$options, TRUE, covariates = TRUE)
    expect_identical(rownames(prepared$data), rownames(input)[match(1:38, permutation)])
    expect_identical(rownames(prepared$raw), rownames(input)[match(1:48, permutation)])
    written <- NULL
    scope <- new.env(parent = asNamespace("jaspTimeSeries"))
    scope$createJaspColumn <- function(columnName, ...) {
      expect_identical(columnName, "savedResiduals")
      column <- new.env(parent = emptyenv())
      column$dependOn <- function(...) invisible(NULL)
      column$setScale <- function(value) { written <<- value; invisible(NULL) }
      column
    }
    saveResiduals <- getFromNamespace(".tsSaveResiduals", "jaspTimeSeries")
    environment(saveResiduals) <- scope
    options <- actual$options
    options$residualSavedToData <- TRUE
    options$residualColumn <- "savedResiduals"
    saveResiduals(prepared$data, prepared$raw, fit, options, new.env(parent = emptyenv()), TRUE, character())
    expectedResiduals <- rep(NA_real_, 96)
    expectedResiduals[2 * match(1:38, permutation)] <- as.numeric(stats::residuals(arimaPreparationFit(reference)))
    expect_equal(written, expectedResiduals, tolerance = 1e-10)
    expect_true(all(is.na(written[c(seq(1, 95, 2), 78:96)])))
  }
})

test_that("ARIMA residual distributions preserve gaps and explain missing residuals", {
  data <- arimaPreparationFixture()
  data$y[c(7, 19, 47, 48)] <- NA_real_
  reference <- arimaPreparationRun(data, time = FALSE)
  referenceFit <- arimaPreparationFit(reference)
  expect_s3_class(referenceFit$residuals, "ts")
  expectedResiduals <- as.numeric(residuals(referenceFit))
  note <- "Missing residuals appear as gaps in the time-series plot and are excluded from the distribution."
  permutation <- c(25:48, 1:24)
  dates <- format(as.Date("2020-01-01") + cumsum(rep(c(1, 3, 7), 16)))

  for (time in list(data$Time, dates)) for (distribution in c("density", "histogram")) {
    data$Time <- time
    input <- data[permutation, ]
    rownames(input) <- seq_len(nrow(input))
    actual <- arimaPreparationRun(input, residualTimeSeries = TRUE,
                                  residualTimeSeriesType = "both",
                                  residualTimeSeriesDistribution = distribution)
    expectArimaPreparedLike(actual, reference, data$y)
    plots <- actual$result$results$residContainer$collection
    expect_match(paste(unlist(plots$residContainer_residualTimeSeriesMissingNote), collapse = " "),
                 note, fixed = TRUE)

    plot <- actual$result$state$figures[[plots$residContainer_residualTimeSeriesPlot$data]]$obj
    if (jaspGraphs::isJaspPlotRecipe(plot))
      plot <- jaspGraphs::materializeJaspPlotRecipe(plot)
    expect_s3_class(plot, "jaspMatrixPlot")
    expect_true("rightPlot" %in% names(plot$subplots))
    main <- plot$subplots$mainPlot
    expect_equal(as.numeric(main$data$y), expectedResiduals, tolerance = 1e-10)
    expect_identical(which(is.na(main$data$y)), c(7L, 19L, 47L, 48L))
    expectedTime <- if (is.numeric(time)) time else as.POSIXct(time, tz = "UTC")
    expect_equal(as.numeric(main$data$x), as.numeric(expectedTime))
    # Missing entries must reach the line layer so it breaks at internal gaps.
    line <- which(vapply(main$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1L)))
    expect_length(line, 1L)
    lineData <- ggplot2::ggplot_build(main)$data[[line]]
    expect_identical(which(is.na(lineData$y)), c(7L, 19L, 47L, 48L))
  }
})

test_that("ARIMA omits the missing-residual note when it does not apply", {
  data <- arimaPreparationFixture()
  complete <- arimaPreparationRun(data, residualTimeSeries = TRUE,
                                  residualTimeSeriesDistribution = "density")
  expect_null(complete$result$results$residContainer$collection$residContainer_residualTimeSeriesMissingNote)

  data$y[c(7, 19)] <- NA_real_
  noDistribution <- arimaPreparationRun(data, residualTimeSeries = TRUE,
                                        residualTimeSeriesDistribution = "none")
  expect_null(noDistribution$result$results$residContainer$collection$residContainer_residualTimeSeriesMissingNote)
  noPlot <- arimaPreparationRun(data, residualTimeSeries = FALSE,
                                residualTimeSeriesDistribution = "density")
  expect_null(noPlot$result$results$residContainer$collection$residContainer_residualTimeSeriesMissingNote)
})

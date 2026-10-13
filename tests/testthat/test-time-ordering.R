# Real backend models with plain R result/column collectors. Supported analysis
# runtime is exercised by the existing analysis tests as well.
scope <- new.env(parent = asNamespace("jaspTimeSeries"))
for (name in c(".tsArimaResults", ".tsForecasts", ".tsFillforecastPlot",
               ".tsCreateTableForecasts", ".tsSaveForecasts", ".tsSaveResiduals",
               ".tsTransformData", ".tsComputePolyResults", ".tsSaveTransformation",
               ".tsResidualDiagnostics")) {
  fun <- getFromNamespace(name, "jaspTimeSeries")
  environment(fun) <- scope
  scope[[name]] <- fun
}
scope$createJaspState <- function(object) {
  state <- new.env(parent = emptyenv())
  state$object <- object
  state$dependOn <- function(...) invisible(NULL)
  state
}
optionsFor <- function(...) modifyList(list(
  modelSpecification = "custom", modelSpecificationAutoIc = "aicc",
  p = 1, d = 0, q = 0, seasonal = FALSE, intercept = TRUE,
  covariates = character(), time = "Time", dependent = "y",
  forecastLength = 3L, forecastSave = "", forecastTable = TRUE,
  forecastTimeSeriesObserved = TRUE, forecastTimeSeriesType = "both",
  log = FALSE, root = FALSE, boxCox = FALSE, detrend = TRUE,
  difference = FALSE, polynomialSpecification = "custom", detrendPoly = 1L,
  polynomialSpecificationAutoMax = 2L, polynomialSpecificationAutoIc = "aic"
), list(...))
fitSource <- function(data, options) {
  result <- new.env(parent = emptyenv())
  scope$.tsArimaResults(result, data, options, TRUE, character())
  result$arimaResult$object
}
forecastSource <- function(fit, data, raw, options) {
  result <- new.env(parent = emptyenv())
  scope$.tsForecasts(fit, data, raw, options, result, TRUE)
  result$forecastResult$object
}

test_that("detrending uses observation steps and retains missing-response positions", {
  step <- seq_len(12L)
  y <- 2 + 0.6 * step + sin(step)
  y[c(3, 8)] <- NA_real_
  expected <- residuals(lm(y ~ step, na.action = na.exclude))
  for (time in list(step, c(1, 2, 200, 200, 400, 450, 1000, 1001, 2000, 9000, 9001, 1e12),
                   as.POSIXct("2020-01-01", tz = "UTC") + step^3)) {
    data <- data.frame(y = y, t = time)
    actual <- scope$.tsTransformData(new.env(parent = emptyenv()), data, optionsFor(), TRUE)
    expect_equal(actual$y, as.numeric(expected), tolerance = 1e-12)
    expect_identical(actual$t, time)
    expect_identical(which(is.na(actual$y)), c(3L, 8L))

    result <- new.env(parent = emptyenv())
    options <- optionsFor(polynomialSpecification = "auto")
    auto <- scope$.tsTransformData(result, data, options, TRUE)
    references <- lapply(0:2, function(degree) {
      fit <- if (degree == 0L) lm(y ~ 1) else lm(y ~ poly(step, degree))
      list(aic = AIC(fit), residuals = as.numeric(residuals(fit)))
    })
    best <- which.min(vapply(references, function(x) x$aic, numeric(1L)))
    expect_equal(auto$y[!is.na(y)], references[[best]]$residuals, tolerance = 1e-12)
    expect_identical(which(is.na(auto$y)), c(3L, 8L))
  }
})

test_that("saved columns distinguish tied timestamps by original row identity", {
  raw <- data.frame(t = c(1, 1, 2, 2, 5), y = c(10, 20, 30, 40, 50),
                    row.names = c("8", "2", "9", "5", "12"))
  data <- raw[c(2, 3, 4), , drop = FALSE]
  data$y <- c(101, 102, NA_real_)
  writes <- list()
  scope$createJaspColumn <- function(columnName, ...) {
    column <- new.env(parent = emptyenv())
    column$dependOn <- function(...) invisible(NULL)
    column$setScale <- function(value) { writes[[columnName]] <<- value; invisible(NULL) }
    column
  }
  options <- optionsFor(transformationSavedToData = TRUE, transformationColumn = "transformed",
                        residualSavedToData = TRUE, residualColumn = "residual")
  scope$.tsSaveTransformation(data, raw, options, new.env(parent = emptyenv()), TRUE, character())
  scope$.tsSaveResiduals(data, raw, list(residuals = c(11, 12, 13)), options,
                        new.env(parent = emptyenv()), TRUE, character())
  transformed <- residual <- rep(NA_real_, 12)
  transformed[c(2, 9, 5)] <- c(101, 102, NA_real_)
  residual[c(2, 9, 5)] <- c(11, 12, 13)
  expect_identical(writes$transformed, transformed)
  expect_identical(writes$residual, residual)
})

test_that("forecast horizons start after all fitted rows including trailing missing responses", {
  y <- c(3, 4, 2, 5, 6, 4, 7, 5, NA, NA)
  data <- data.frame(y = y, t = 2001:2010, row.names = as.character(21:30))
  options <- optionsFor(p = 0, d = 1, intercept = FALSE)
  fit <- fitSource(data, options)
  reference <- forecast::forecast(fit, h = 3L)
  expect_identical(length(fit$x), 10L)
  expect_equal(unname(start(reference$mean)), c(11, 1))
  expect_equal(as.numeric(reference$upper[, "95%"] - reference$mean),
               qnorm(.975) * sqrt((2 + 1:3) * fit$sigma2), tolerance = 1e-10)
  actual <- forecastSource(fit, data, data, options)
  expect_identical(actual$t, 1:3)
  expect_equal(actual$y, as.numeric(reference$mean))
  expect_equal(actual$upper95, as.numeric(reference$upper[, "95%"]))

  noTime <- data; noTime$t <- 21:30
  actualNoTime <- forecastSource(fit, noTime, noTime, optionsFor(p = 0, d = 1, intercept = FALSE, time = ""))
  expect_equal(actualNoTime$t, 31:33)
})

test_that("one or two future covariates follow the final modeled row, including ties", {
  k <- 1:28
  raw <- data.frame(t = rep(1:7, each = 4), y = 10 + sin(k/3) + cos(k/7),
                    xreg1 = sin(k/5), xreg2 = cos(k/9), row.names = as.character(28:1))
  raw$y[19:20] <- NA_real_
  for (number in 1:2) {
    selected <- c("y", "t", paste0("xreg", 1:number))
    input <- raw[, selected, drop = FALSE]
    data <- input[1:20, , drop = FALSE]
    options <- optionsFor(covariates = paste0("x", 1:number))
    fit <- fitSource(data, options)
    actual <- forecastSource(fit, data, input, options)
    xreg <- as.matrix(input[21:23, paste0("xreg", 1:number), drop = FALSE])
    reference <- forecast::forecast(fit, h = 3, xreg = xreg)
    expect_equal(actual$y, as.numeric(reference$mean))
    expect_equal(actual$upper95, as.numeric(reference$upper[, "95%"]))
    expect_identical(actual$t, 1:3)
    expect_error(forecastSource(fit, data, data, options), "unless the covariates", class = "validationError")
    expect_error(forecastSource(fit, data, input[1:21, , drop = FALSE], options),
                 "maximum number of forecasts is 1", class = "validationError")
  }
})

test_that("forecast plots use ordinal observation steps when Time is assigned", {
  y <- c(3, 4, 2, 5, 6, 4, 7, 5, NA, NA)
  for (time in list(2001:2010, as.POSIXct("2020-01-01", tz = "UTC") + (1:10)^3)) {
    data <- data.frame(y = y, t = time)
    for (showObserved in c(TRUE, FALSE)) {
      options <- optionsFor(p = 0, d = 1, intercept = FALSE, forecastTimeSeriesObserved = showObserved)
      fit <- fitSource(data, options)
      plot <- new.env(parent = emptyenv())
      result <- new.env(parent = emptyenv())
      scope$.tsFillforecastPlot(plot, fit, data, data, options, result, TRUE)
      expect_equal(plot$plotObject$data$t, if (showObserved) 1:13 else 11:13)
      expect_equal(plot$plotObject$layers[[1]]$data$t, 11:13)
      expect_identical(plot$plotObject$scales$get_scales("x")$name, "Observation step")
      expect_identical(result$forecastResult$object$t, 1:3)
    }
  }
})

test_that("no-Time forecast plots, tables and CSVs retain filtered indices", {
  scope$decodeColNames <- identity
  for (originalRows in list(1:40, seq(2, 80, by = 2))) {
    # The second case represents input after a native global filter: row names
    # retain spreadsheet identity while t indexes the supplied observations.
    raw <- data.frame(y = 10 + sin(1:40), t = 1:40,
                      row.names = as.character(originalRows))
    data <- raw[originalRows >= 10 & originalRows <= 30, , drop = FALSE]
    expectedForecast <- tail(data$t, 1L) + 1:3
    options <- optionsFor(time = "", forecastSave = tempfile(fileext = ".csv"))
    fit <- fitSource(data, options)
    tableRows <- NULL
    scope$createJaspTable <- function(...) {
      table <- new.env(parent = emptyenv())
      table$dependOn <- function(...) invisible(NULL)
      table$addColumnInfo <- function(...) invisible(NULL)
      table$addRows <- function(rows) { tableRows <<- rows; invisible(NULL) }
      table
    }
    for (showObserved in c(TRUE, FALSE)) {
      options$forecastTimeSeriesObserved <- showObserved
      result <- new.env(parent = emptyenv())
      plot <- new.env(parent = emptyenv())
      scope$.tsFillforecastPlot(plot, fit, data, raw, options, result, TRUE)
      scope$.tsCreateTableForecasts(result, fit, data, raw, options, TRUE, 1, character())
      scope$.tsSaveForecasts(result, fit, data, raw, options, TRUE)
      exported <- read.csv(options$forecastSave)
      expect_equal(plot$plotObject$data$t,
                   if (showObserved) c(data$t, expectedForecast) else expectedForecast)
      expect_equal(plot$plotObject$layers[[1]]$data$t, expectedForecast)
      expect_equal(plot$plotObject$layers[[2]]$data$t, expectedForecast)
      expect_identical(plot$plotObject$scales$get_scales("x")$name, "t")
      expect_equal(result$forecastResult$object$t, expectedForecast)
      expect_equal(as.numeric(tableRows$t), expectedForecast)
      expect_equal(exported$t, expectedForecast)
    }
    unlink(options$forecastSave)
  }
})

test_that("forecast tables and CSVs identify steps ahead when Time is assigned", {
  data <- data.frame(y = c(3, 4, 2, 5, 6, 4, 7, 5, 8, 9), t = 21:30)
  columns <- list(); rows <- NULL
  scope$createJaspTable <- function(...) {
    table <- new.env(parent = emptyenv())
    table$dependOn <- function(...) invisible(NULL)
    table$addColumnInfo <- function(...) { columns[[length(columns) + 1L]] <<- list(...); invisible(NULL) }
    table$addRows <- function(value) { rows <<- value; invisible(NULL) }
    table
  }
  scope$decodeColNames <- identity
  for (assigned in c(TRUE, FALSE)) {
    options <- optionsFor(time = if (assigned) "Time" else "", forecastSave = tempfile(fileext = ".csv"))
    fit <- fitSource(data, options)
    result <- new.env(parent = emptyenv())
    columns <- list()
    scope$.tsCreateTableForecasts(result, fit, data, data, options, TRUE, 1, character())
    expect_identical(columns[[1L]]$title, if (assigned) "Steps ahead" else "t")
    expect_identical(rows$t, as.character(if (assigned) 1:3 else 31:33))
    scope$.tsSaveForecasts(result, fit, data, data, options, TRUE)
    exported <- read.csv(options$forecastSave)
    unlink(options$forecastSave)
    expect_identical(names(exported)[1L], if (assigned) "stepAhead" else "t")
    expect_equal(exported[[1L]], if (assigned) 1:3 else 31:33)
  }
})



test_that("residual Q-Q plots omit missing residuals without changing the fitted series", {
  scope$createJaspContainer <- function(...) {
    result <- new.env(parent = emptyenv())
    result$dependOn <- function(...) invisible(NULL)
    result
  }
  scope$createJaspPlot <- function(...) {
    result <- new.env(parent = emptyenv())
    result$dependOn <- function(...) invisible(NULL)
    result
  }
  for (missing in list(c(3, 8), c(11, 12))) {
    data <- data.frame(y = 2 + sin(1:12), t = 2000 + 1:12)
    data$y[missing] <- NA_real_
    options <- optionsFor(residualTimeSeries = FALSE, residualAcf = FALSE,
                          residualQQ = TRUE, residualLjungBox = FALSE)
    fit <- fitSource(data, options)
    originalFit <- fit
    result <- new.env(parent = emptyenv())
    expect_no_error(scope$.tsResidualDiagnostics(result, fit, data, options, TRUE, 1, character()))
    actual <- result$residContainer$residualQQPlot$plotObject
    expected <- jaspGraphs::plotQQnorm(stats::residuals(fit)[!is.na(stats::residuals(fit))], ablineColor = "darkred")
    expect_equal(actual$data, expected$data)
    expect_equal(ggplot2::ggplot_build(actual)$data, ggplot2::ggplot_build(expected)$data)
    expect_identical(fit, originalFit)
    expect_identical(length(fit$x), nrow(data))
    expect_identical(which(is.na(fit$x)), as.integer(missing))
  }
})

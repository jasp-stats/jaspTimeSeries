# Real analysis-runtime checks of the shared ordering contract. The main time
# series plot is disabled; ACF and spectral plot data are inspected directly.
ssPreparationOptions <- function(analysis, ...) {
  options <- jaspTools::analysisOptions(analysis)
  options[["dependent"]] <- "y"
  options[["dependent.types"]] <- "scale"
  options[["time"]] <- "Time"
  options[["filter"]] <- FALSE
  options[["rowStart"]] <- options[["rowEnd"]] <- ""
  options[["timeStart"]] <- options[["timeEnd"]] <- ""
  options[["dateStart"]] <- options[["dateEnd"]] <- ""
  if (analysis == "StationarityTimeSeries") {
    # Explicit native default: readQML can misread the subsequent Label.
    options[["transformationSavedToData"]] <- FALSE
    options[["timeSeriesPlot"]] <- FALSE
    options[["acf"]] <- TRUE
    options[["acfMaxLag"]] <- 8L
    options[["pacf"]] <- FALSE
    options[["adfTest"]] <- options[["ppTestRegressionCoefficient"]] <- TRUE
    options[["ppTestStudentized"]] <- options[["kpssLevel"]] <- options[["kpssTrend"]] <- TRUE
    options[["log"]] <- options[["root"]] <- options[["boxCox"]] <- FALSE
    options[["detrend"]] <- options[["difference"]] <- FALSE
  } else {
    options[["kernel"]] <- FALSE
    options[["kernelTerm"]] <- list(list(kernelDimension = 2L))
    options[["taper"]] <- 0.1
    options[["demean"]] <- TRUE
    options[["detrend"]] <- FALSE
    options[["whiteNoise"]] <- options[["pinkNoise"]] <- options[["brownNoise"]] <- FALSE
  }
  utils::modifyList(options, list(...))
}

ssPreparationRun <- function(analysis, data, options) {
  options[["time.types"]] <- if (options[["time"]] == "") character() else
    if (is.numeric(data[[options[["time"]]]])) "scale" else "nominal"
  encoded <- jaspTools:::encodeOptionsAndDataset(
    jaspTools:::fixOptionsForVariableTypes(options), data)
  # Synthetic global-filter fixtures retain original spreadsheet identities.
  rownames(encoded$dataset) <- rownames(data)
  result <- jaspTools::runAnalysis(analysis, dataset = encoded$dataset,
    options = encoded$options, encodedDataset = TRUE, view = FALSE,
    quiet = TRUE, makeTests = FALSE)
  expect_identical(result$status, "complete")
  result
}

ssPreparationTable <- function(result, name) {
  expect_identical(result$results[[name]]$status, "complete")
  expect_true(length(result$results[[name]]$data) > 0L)
  result$results[[name]]$data
}

ssPreparationPlot <- function(result, name) {
  expect_identical(result$results[[name]]$status, "complete")
  key <- result$results[[name]]$data
  plot <- result$state$figures[[key]]$obj
  expect_true(inherits(plot, "ggplot"))
  plot
}

ssPreparationAcf <- function(result) {
  plot <- ssPreparationPlot(result, "acfPlot")
  candidates <- lapply(plot$layers, function(layer) layer$data)
  matching <- vapply(candidates, function(data)
    is.data.frame(data) && all(c("acf", "lag") %in% names(data)), logical(1L))
  expect_true(any(matching))
  candidates[[which(matching)[[1L]]]][, c("acf", "lag"), drop = FALSE]
}

ssPreparationFixture <- function() {
  step <- seq_len(48L)
  data.frame(y = 10 + 0.03 * step + sin(step / 2.7) + 0.4 * cos(step / 1.3),
             Time = 2000 + step^2)
}

ssPreparationSpectrum <- function(result, y, options) {
  expected <- stats::spec.pgram(stats::ts(y), kernel = NULL,
    taper = options[["taper"]], demean = options[["demean"]],
    detrend = options[["detrend"]], plot = FALSE, na.action = na.exclude)
  plot <- ssPreparationPlot(result, "powerSpectralDensity")
  expect_equal(plot$data$x, log(as.numeric(expected$freq)), tolerance = 1e-10)
  expect_equal(plot$data$y, log(as.numeric(expected$spec)), tolerance = 1e-10)
  rows <- ssPreparationTable(result, "bandWidthTable")
  actual <- unlist(lapply(rows, function(row) row[["bandwidth"]]), use.names = FALSE)
  expect_equal(as.numeric(actual), as.numeric(expected$bandwidth), tolerance = 1e-10)
}

test_that("Stationarity prepares numeric and date Time before detrending and differencing", {
  data <- ssPreparationFixture()
  data$y[c(8L, 27L)] <- NA_real_
  options <- ssPreparationOptions("StationarityTimeSeries", detrend = TRUE,
    polynomialSpecification = "custom", detrendPoly = 2L,
    difference = TRUE, differenceLag = 1L, differenceOrder = 1L)
  reference <- ssPreparationRun("StationarityTimeSeries", data,
    utils::modifyList(options, list(time = "")))
  shuffle <- c(seq(2L, 48L, 2L), seq(1L, 48L, 2L))
  for (time in list(data$Time, as.character(as.Date("2020-01-01") + seq_len(48L)^2))) {
    input <- data
    input$Time <- time
    input <- input[shuffle, , drop = FALSE]
    actual <- ssPreparationRun("StationarityTimeSeries", input, options)
    expect_equal(ssPreparationTable(actual, "stationaryTable"),
                 ssPreparationTable(reference, "stationaryTable"), tolerance = 1e-10)
    expect_equal(ssPreparationAcf(actual), ssPreparationAcf(reference), tolerance = 1e-10)
    expect_true(!is.null(actual$results$timeSortingNote))
  }
})

test_that("Stationarity row filters preserve original identities before automatic detrending", {
  data <- ssPreparationFixture()[c(48:25, 1:24), , drop = FALSE]
  # Nonconsecutive identities represent rows remaining after a global filter.
  rownames(data) <- as.character(seq(2L, 96L, 2L))
  selected <- which(as.numeric(rownames(data)) >= 14 & as.numeric(rownames(data)) <= 82)
  selected <- selected[order(data$Time[selected], selected)]
  options <- ssPreparationOptions("StationarityTimeSeries", filter = TRUE,
    filterBy = "row", rowStart = 14, rowEnd = 82, detrend = TRUE,
    polynomialSpecification = "auto", polynomialSpecificationAutoMax = 3L,
    polynomialSpecificationAutoIc = "aic")
  actual <- ssPreparationRun("StationarityTimeSeries", data, options)
  reference <- ssPreparationRun("StationarityTimeSeries", data[selected, , drop = FALSE],
    utils::modifyList(options, list(time = "", filter = FALSE)))
  expect_equal(ssPreparationTable(actual, "stationaryTable"),
               ssPreparationTable(reference, "stationaryTable"), tolerance = 1e-10)
  expect_equal(ssPreparationTable(actual, "polyTable"),
               ssPreparationTable(reference, "polyTable"), tolerance = 1e-10)
  expect_equal(ssPreparationAcf(actual), ssPreparationAcf(reference), tolerance = 1e-10)
})

test_that("Spectral numeric and date filters match explicit selection and spec.pgram", {
  data <- ssPreparationFixture()
  selected <- 8:40
  options <- ssPreparationOptions("SpectralTimeSeries")
  reference <- ssPreparationRun("SpectralTimeSeries", data[selected, , drop = FALSE],
    utils::modifyList(options, list(time = "")))
  shuffle <- c(48:25, 1:24)
  dates <- as.character(as.Date("2020-01-01") + seq_len(48L)^2)
  for (type in c("time", "date")) {
    input <- data
    limits <- if (type == "time") list(filterBy = "time", timeStart = data$Time[8], timeEnd = data$Time[40]) else
      list(filterBy = "date", dateStart = dates[8], dateEnd = dates[40])
    if (type == "date") input$Time <- dates
    actual <- ssPreparationRun("SpectralTimeSeries", input[shuffle, , drop = FALSE],
      utils::modifyList(options, c(list(filter = TRUE), limits)))
    expect_equal(ssPreparationTable(actual, "bandWidthTable"),
                 ssPreparationTable(reference, "bandWidthTable"), tolerance = 1e-10)
    expect_equal(ssPreparationPlot(actual, "powerSpectralDensity")$data,
                 ssPreparationPlot(reference, "powerSpectralDensity")$data, tolerance = 1e-10)
    ssPreparationSpectrum(actual, data$y[selected], options)
  }
})

test_that("Spectral analysis uses stable ties and whole-column fallback", {
  data <- ssPreparationFixture()[c(48:25, 1:24), , drop = FALSE]
  options <- ssPreparationOptions("SpectralTimeSeries")
  for (fallback in c(FALSE, TRUE)) {
    input <- data
    if (fallback) {
      input$Time <- as.character(input$Time)
      input$Time[7L] <- "not a date"
      selected <- seq_len(nrow(input))
    } else {
      input$Time <- rep(c(3, 1, 2), 16L)
      selected <- order(input$Time, seq_len(nrow(input)))
    }
    actual <- ssPreparationRun("SpectralTimeSeries", input, options)
    reference <- ssPreparationRun("SpectralTimeSeries", input[selected, , drop = FALSE],
      utils::modifyList(options, list(time = "")))
    expect_equal(ssPreparationTable(actual, "bandWidthTable"),
                 ssPreparationTable(reference, "bandWidthTable"), tolerance = 1e-10)
    expect_equal(ssPreparationPlot(actual, "powerSpectralDensity")$data,
                 ssPreparationPlot(reference, "powerSpectralDensity")$data, tolerance = 1e-10)
    expect_true(!is.null(actual$results[[if (fallback) "timeWarning" else "timeTiesNote"]]))
    ssPreparationSpectrum(actual, input$y[selected], options)
  }
})

test_that("saved Stationarity transformations retain original row identity after preparation", {
  # Only output sinks are replaced in local copies. Parsing, selection, the
  # production entrypoint and lm/diff computations are genuine. This checks the
  # column payload, not native Desktop spreadsheet writeback.
  scope <- new.env(parent = asNamespace("jaspTimeSeries"))
  for (name in c("StationarityTimeSeries", ".tsTransformData", ".tsSaveTransformation")) {
    fun <- getFromNamespace(name, "jaspTimeSeries")
    environment(fun) <- scope
    scope[[name]] <- fun
  }
  for (name in c(".tsPrepareData", ".tsInterpretTime", ".tsFilterPreparedData")) {
    fun <- getFromNamespace(name, "jaspDescriptives")
    environment(fun) <- scope
    scope[[name]] <- fun
  }
  scope$createJaspHtml <- function(...) list(...)
  writes <- list()
  scope$createJaspColumn <- function(columnName, ...) {
    column <- new.env(parent = emptyenv())
    column$dependOn <- function(...) invisible(NULL)
    column$setScale <- function(value) { writes[[columnName]] <<- value; invisible(NULL) }
    column
  }
  data <- ssPreparationFixture()[c(48:25, 1:24), , drop = FALSE]
  rownames(data) <- as.character(seq(2L, 96L, 2L))
  data$y[c(9L, 25L)] <- NA_real_
  data$Time <- rep(c(-1.5, 0, 2.5, 4), 12L)
  options <- ssPreparationOptions("StationarityTimeSeries", acf = FALSE,
    adfTest = FALSE, ppTestRegressionCoefficient = FALSE, ppTestStudentized = FALSE,
    kpssLevel = FALSE, kpssTrend = FALSE, detrend = TRUE,
    polynomialSpecification = "custom", detrendPoly = 1L,
    difference = TRUE, differenceLag = 1L, differenceOrder = 1L,
    transformationSavedToData = TRUE, transformationColumn = "preparedTransformation",
    filter = TRUE)
  for (type in c("row", "time", "date")) {
    input <- data
    options[["filterBy"]] <- type
    options[["rowStart"]] <- 14
    options[["rowEnd"]] <- 82
    options[["timeStart"]] <- 0
    options[["timeEnd"]] <- 4
    options[["dateStart"]] <- "2020-01-02"
    options[["dateEnd"]] <- "2020-01-10"
    if (type == "date") input$Time <- as.character(as.Date("2020-01-01") + rep(c(0, 1, 5, 9), 12L))
    ordered <- order(input$Time, seq_len(nrow(input)))
    keep <- if (type == "row") as.numeric(rownames(input)) >= 14 & as.numeric(rownames(input)) <= 82 else
      if (type == "time") input$Time >= 0 & input$Time <= 4 else input$Time >= "2020-01-02" & input$Time <= "2020-01-10"
    selected <- ordered[keep[ordered]]
    y <- input$y[selected]
    step <- seq_along(y)
    residual <- as.numeric(stats::residuals(stats::lm(y ~ step, na.action = na.exclude)))
    transformed <- c(NA_real_, diff(residual))
    expected <- rep(NA_real_, 96L)
    expected[as.numeric(rownames(input))[selected]] <- transformed
    result <- new.env(parent = emptyenv())
    writes <- list()
    expect_no_error(scope$StationarityTimeSeries(result, input, options))
    expect_equal(writes$preparedTransformation, expected, tolerance = 1e-10)
    expect_true(!is.null(result$timeTiesNote))
    expect_identical(which(!is.na(writes$preparedTransformation)), which(!is.na(expected)))
  }
})

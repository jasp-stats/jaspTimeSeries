stationarityDifferenceOptions <- function(...) {
  options <- jaspTools::analysisOptions("StationarityTimeSeries")
  options <- utils::modifyList(options, list(
    dependent = "y", dependent.types = "scale", time = "Time",
    filter = FALSE, log = FALSE, root = FALSE, boxCox = FALSE, detrend = FALSE,
    difference = TRUE, differenceLag = 1L, differenceOrder = 1L,
    transformationSavedToData = FALSE, timeSeriesPlot = FALSE,
    acf = FALSE, pacf = FALSE, adfTest = FALSE,
    ppTestRegressionCoefficient = FALSE, ppTestStudentized = FALSE,
    kpssLevel = FALSE, kpssTrend = FALSE
  ))
  utils::modifyList(options, list(...))
}

# Independent finite-difference formula: (1 - B^lag)^order, with unavailable
# initial positions kept in place. Do not use diff() in the reference.
stationarityDifferenceReference <- function(y, lag, order) {
  expected <- rep(NA_real_, length(y))
  if (length(y) <= lag * order) return(expected)
  powers <- 0:order
  weights <- (-1)^powers * choose(order, powers)
  for (i in seq.int(lag * order + 1L, length(y)))
    expected[i] <- sum(weights * y[i - lag * powers])
  expected
}

stationarityDifferenceTransform <- function(data, options) {
  getFromNamespace(".tsTransformData", "jaspTimeSeries")(NULL, data, options, TRUE)
}

stationarityDifferenceRun <- function(data, options) {
  options[["time.types"]] <- if (is.numeric(data$Time)) "scale" else "nominal"
  # Native import creates a fresh spreadsheet; direct tests below separately
  # preserve and verify row identities from a global filter.
  rownames(data) <- NULL
  result <- jaspTools::runAnalysis("StationarityTimeSeries", dataset = data,
    options = options, view = FALSE,
    quiet = TRUE, makeTests = FALSE)
  expect_identical(result$status, "complete")
  expect_identical(result$results$timeSeriesPlot$status, "complete")
  plot <- result$state$figures[[result$results$timeSeriesPlot$data]]$obj
  if (jaspGraphs::isJaspPlotRecipe(plot))
    plot <- jaspGraphs::materializeJaspPlotRecipe(plot)
  main <- if (inherits(plot, "jaspMatrixPlot")) plot$subplots$mainPlot else plot
  expect_s3_class(main, "ggplot")
  list(result = result, plot = main)
}

test_that("Stationarity applies the requested differencing order and lag", {
  data <- data.frame(y = seq_len(12L)^3, t = seq_len(12L)^2, other = letters[1:12])
  rownames(data) <- as.character(seq(2L, 24L, 2L))
  original <- data
  for (lag in c(1L, 2L, 3L)) {
    for (order in 1:3) {
      options <- stationarityDifferenceOptions(differenceLag = lag, differenceOrder = order)
      actual <- stationarityDifferenceTransform(data, options)
      expected <- stationarityDifferenceReference(data$y, lag, order)
      expect_equal(actual$y, expected)
      expect_identical(actual[-1L], data[-1L])
      expect_identical(rownames(actual), rownames(data))
      expect_identical(which(is.na(actual$y)), seq_len(lag * order))
    }
  }
  expect_identical(data, original)
  # A cubic's third difference is constant; the wrong `order` argument instead
  # yields first differences. The default remains the usual adjacent subtraction.
  expect_equal(stationarityDifferenceTransform(data,
    stationarityDifferenceOptions(differenceOrder = 3L))$y[-(1:3)], rep(6, 9L))
  expect_equal(stationarityDifferenceTransform(data,
    stationarityDifferenceOptions())$y, c(NA_real_, data$y[-1L] - data$y[-12L]))
})

test_that("higher-order differencing propagates missings without shifting observations", {
  for (missing in list(1L, 8L, 18L, c(1L, 8L, 18L))) {
    data <- data.frame(y = seq_len(18L)^3, t = 100 + seq_len(18L))
    data$y[missing] <- NA_real_
    for (lag in c(1L, 2L)) {
      for (order in 1:3) {
        actual <- stationarityDifferenceTransform(data,
          stationarityDifferenceOptions(differenceLag = lag, differenceOrder = order))
        expected <- stationarityDifferenceReference(data$y, lag, order)
        expect_equal(actual$y, expected)
        expect_identical(which(is.na(actual$y)), which(is.na(expected)))
        expect_identical(actual$t, data$t)
        expect_identical(nrow(actual), nrow(data))
      }
    }
  }
})

test_that("series too short for the requested differences retain all rows as missing", {
  for (lag in c(1L, 2L, 4L)) {
    for (order in 1:3) {
      for (n in unique(c(1L, lag * order))) {
        data <- data.frame(y = seq_len(n)^2, t = seq_len(n))
        actual <- stationarityDifferenceTransform(data,
          stationarityDifferenceOptions(differenceLag = lag, differenceOrder = order))
        expect_equal(actual$y, rep(NA_real_, n))
        expect_identical(actual$t, data$t)
        expect_identical(rownames(actual), rownames(data))
      }
    }
  }
})

test_that("higher-order differences save to original rows after Time ordering and filtering", {
  step <- seq_len(36L)
  data <- data.frame(y = step^3 + 2 * step, Time = 100 + step^2)
  data$y[c(9L, 25L)] <- NA_real_
  data <- data[c(seq(2L, 36L, 2L), seq(1L, 35L, 2L)), ]
  rownames(data) <- as.character(seq(2L, 72L, 2L))
  options <- stationarityDifferenceOptions(differenceLag = 2L, differenceOrder = 3L,
    filter = TRUE, rowStart = 8L, rowEnd = 64L,
    timeStart = 116, timeEnd = 1000,
    dateStart = "2020-01-05", dateEnd = "2020-01-31",
    transformationSavedToData = TRUE, transformationColumn = "differenced")
  for (type in c("row", "time", "date")) {
    input <- data
    options$filterBy <- type
    if (type == "date") input$Time <- as.character(as.Date("2020-01-01") + sqrt(data$Time - 100))
    rowIds <- as.integer(rownames(input))
    keep <- if (type == "row") rowIds >= 8L & rowIds <= 64L else
      if (type == "time") input$Time >= 116 & input$Time <= 1000 else
        input$Time >= "2020-01-05" & input$Time <= "2020-01-31"
    selected <- order(input$Time)
    selected <- selected[keep[selected]]
    expected <- rep(NA_real_, 72L)
    expected[rowIds[selected]] <- stationarityDifferenceReference(input$y[selected], 2L, 3L)
    prepared <- jaspDescriptives::.tsPrepareData(NULL, input, options, TRUE)
    transformed <- stationarityDifferenceTransform(prepared$data, options)

    # Use the real writeback mapping and a plain-R column sink. This checks the
    # payload; actual native spreadsheet writes/save-reopen remain GUI checks.
    written <- NULL
    scope <- new.env(parent = asNamespace("jaspTimeSeries"))
    scope$createJaspColumn <- function(columnName, ...) {
      expect_identical(columnName, "differenced")
      column <- new.env(parent = emptyenv())
      column$dependOn <- function(...) invisible(NULL)
      column$setScale <- function(value) { written <<- value; invisible(NULL) }
      column
    }
    save <- getFromNamespace(".tsSaveTransformation", "jaspTimeSeries")
    environment(save) <- scope
    save(transformed, prepared$raw, options, new.env(parent = emptyenv()), TRUE, character())
    expect_equal(written, expected)
    expect_identical(which(is.na(written)), which(is.na(expected)))
    expect_identical(rownames(transformed), rownames(input)[selected])
  }
})

test_that("Stationarity plots use higher-order differences with Time and missing positions intact", {
  step <- seq_len(40L)
  data <- data.frame(y = step^3 + sin(step), Time = 2000 + step^2)
  data$y[c(15L, 40L)] <- NA_real_
  shuffled <- data[c(seq(2L, 40L, 2L), seq(1L, 39L, 2L)), ]
  for (case in list(c(lag = 1L, order = 2L), c(lag = 2L, order = 3L))) {
    options <- stationarityDifferenceOptions(differenceLag = case[["lag"]],
      differenceOrder = case[["order"]], timeSeriesPlot = TRUE,
      timeSeriesPlotType = "both", timeSeriesPlotDistribution = "none")
    actual <- stationarityDifferenceRun(shuffled, options)
    expected <- stationarityDifferenceReference(data$y, case[["lag"]], case[["order"]])
    expect_equal(as.numeric(actual$plot$data$y), expected, tolerance = 1e-10)
    expect_equal(as.numeric(actual$plot$data$x), data$Time)
    expect_identical(which(is.na(actual$plot$data$y)), which(is.na(expected)))
    expect_true(!is.null(actual$result$results$timeSortingNote))
  }
})

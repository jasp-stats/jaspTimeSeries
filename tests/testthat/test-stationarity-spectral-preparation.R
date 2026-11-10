# Real analysis-runtime checks of the shared ordering contract. Plot data and
# transformed values are inspected directly.
ssPreparationOptions <- function(analysis, ...) {
  options <- jaspTools::analysisOptions(analysis)
  options[["dependent"]] <- "y"
  options[["dependent.types"]] <- "scale"
  options[["time"]] <- "Time"
  options[["filter"]] <- FALSE
  if (analysis == "StationarityTimeSeries") {
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
  # The bridge imports fresh rows 1..n. Global-filter row identities are tested
  # directly with the preparation/transformation/saving helpers below.
  rownames(data) <- NULL
  result <- jaspTools::runAnalysis(analysis, dataset = data,
    options = options, view = FALSE,
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
    difference = TRUE, differenceLag = 1L, differenceOrder = 1L,
    testNaAction = "longest", acfNaAction = "longest")
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

test_that("Stationarity log and root analyses retain gaps in plots and saved values", {
  data <- ssPreparationFixture()
  data$y[c(1L, 17L, 48L)] <- NA_real_
  permutation <- c(seq(2L, 48L, 2L), seq(1L, 47L, 2L))
  input <- data[permutation, ]
  rownames(input) <- as.character(seq(2L, 96L, 2L))
  cases <- list(
    list(options = list(log = TRUE, logBase = "e"), expected = log),
    list(options = list(root = TRUE, rootIndex = "square"), expected = sqrt),
    list(options = list(log = TRUE, logBase = "10", root = TRUE, rootIndex = "cube"),
         expected = function(y) log10(y)^(1 / 3))
  )
  for (case in cases) {
    options <- utils::modifyList(ssPreparationOptions("StationarityTimeSeries",
      acf = FALSE, timeSeriesPlot = TRUE, timeSeriesPlotType = "both",
      timeSeriesPlotDistribution = "none"), case$options)
    result <- ssPreparationRun("StationarityTimeSeries", input, options)
    expect_identical(result$results$timeSeriesPlot$status, "complete")
    plot <- result$state$figures[[result$results$timeSeriesPlot$data]]$obj
    if (jaspGraphs::isJaspPlotRecipe(plot))
      plot <- jaspGraphs::materializeJaspPlotRecipe(plot)
    main <- if (inherits(plot, "jaspMatrixPlot")) plot$subplots$mainPlot else plot
    expect_s3_class(main, "ggplot")
    expected <- case$expected(data$y)
    expect_equal(as.numeric(main$data$y), expected, tolerance = 1e-12)
    expect_equal(as.numeric(main$data$x), data$Time)
    expect_identical(which(is.na(main$data$y)), c(1L, 17L, 48L))
    line <- which(vapply(main$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1L)))
    expect_length(line, 1L)
    expect_identical(which(is.na(ggplot2::ggplot_build(main)$data[[line]]$y)), c(1L, 17L, 48L))

    # Exercise real preparation/transformation with only the native column sink
    # collected. This does not certify Desktop spreadsheet writes or test policy.
    prepared <- jaspDescriptives::.tsPrepareData(NULL, input, options, TRUE)
    transformed <- getFromNamespace(".tsTransformData", "jaspTimeSeries")(NULL, prepared$data, options, TRUE)
    written <- NULL
    scope <- new.env(parent = asNamespace("jaspTimeSeries"))
    scope$createJaspColumn <- function(columnName, ...) {
      expect_identical(columnName, "preparedTransformation")
      column <- new.env(parent = emptyenv())
      column$dependOn <- function(...) invisible(NULL)
      column$setScale <- function(value) { written <<- value; invisible(NULL) }
      column
    }
    save <- getFromNamespace(".tsSaveTransformation", "jaspTimeSeries")
    environment(save) <- scope
    options$transformationSavedToData <- TRUE
    options$transformationColumn <- "preparedTransformation"
    save(transformed, prepared$raw, options, new.env(parent = emptyenv()), TRUE, character())
    expectedColumn <- rep(NA_real_, 96L)
    expectedColumn[2L * match(seq_len(48L), permutation)] <- expected
    expect_equal(written, expectedColumn, tolerance = 1e-12)
    expect_identical(which(is.na(written)), which(is.na(expectedColumn)))
  }
})

test_that("Stationarity row filters preserve original identities before automatic detrending", {
  data <- ssPreparationFixture()[c(48:25, 1:24), , drop = FALSE]
  rownames(data) <- NULL
  selected <- 7:41
  selected <- selected[order(data$Time[selected], selected)]
  options <- ssPreparationOptions("StationarityTimeSeries", filter = TRUE,
    filterBy = "row", rowStart = 7, rowEnd = 41, detrend = TRUE,
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

  # The native bridge's dataframe import cannot represent a global row filter.
  # Preserve that distinct contract check with the real shared preparation:
  # these same observations now occupy every second original spreadsheet row.
  filtered <- data
  rownames(filtered) <- as.character(seq(2L, 96L, 2L))
  filteredOptions <- utils::modifyList(options, list(rowStart = 14L, rowEnd = 82L))
  prepared <- jaspDescriptives::.tsPrepareData(NULL, filtered, filteredOptions, TRUE)
  expect_equal(prepared$data$y, data$y[selected])
  expect_equal(prepared$data$t, data$Time[selected])
  expect_identical(rownames(prepared$data), as.character(2L * selected))
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

test_that("Stationarity policies select differenced values without changing plotted gaps", {
  data <- ssPreparationFixture()
  options <- ssPreparationOptions("StationarityTimeSeries", difference = TRUE,
    differenceLag = 1L, differenceOrder = 1L, timeSeriesPlot = TRUE,
    timeSeriesPlotType = "both", timeSeriesPlotDistribution = "none")
  expected <- as.numeric(stats::acf(diff(data$y), plot = FALSE, lag.max = 8L)$acf)[-1L]
  for (policy in c("listwise", "longest")) {
    actual <- ssPreparationRun("StationarityTimeSeries", data,
      utils::modifyList(options, list(testNaAction = policy, acfNaAction = policy)))
    expect_true(length(ssPreparationTable(actual, "stationaryTable")) > 0L)
    expect_equal(ssPreparationAcf(actual)$acf, expected, tolerance = 1e-10)
    expect_match(paste(unlist(actual$results$acfMissingNote), collapse = " "), "47 of 48", fixed = TRUE)
    note <- paste(unlist(actual$results$stationaryTable$footnotes), collapse = " ")
    expect_match(note, if (policy == "listwise") "consecutive time steps" else "steps 2 to 48", fixed = TRUE)
    expect_identical(actual$results$timeSeriesPlot$status, "complete")
    plot <- actual$state$figures[[actual$results$timeSeriesPlot$data]]$obj
    if (jaspGraphs::isJaspPlotRecipe(plot))
      plot <- jaspGraphs::materializeJaspPlotRecipe(plot)
    main <- if (inherits(plot, "jaspMatrixPlot")) plot$subplots$mainPlot else plot
    expect_equal(as.numeric(main$data$y), c(NA_real_, diff(data$y)), tolerance = 1e-10)
    expect_equal(as.numeric(main$data$x), data$Time)
  }
})

test_that("Stationarity unusable segments fail locally while listwise diagnostics complete", {
  data <- ssPreparationFixture()
  data$y[seq(2L, 48L, 2L)] <- NA_real_
  options <- ssPreparationOptions("StationarityTimeSeries", testNaAction = "longest",
    acfNaAction = "listwise", pacf = TRUE, pacfNaAction = "longest",
    timeSeriesPlot = TRUE, timeSeriesPlotType = "both", timeSeriesPlotDistribution = "none")
  actual <- ssPreparationRun("StationarityTimeSeries", data, options)
  for (output in c("stationaryTable", "pacfPlot"))
    expect_match(paste(unlist(actual$results[[output]]), collapse = " "), "fewer than two values", fixed = TRUE)
  expect_null(actual$results$pacfMissingNote)
  expect_identical(actual$results$timeSeriesPlot$status, "complete")
  expect_equal(ssPreparationAcf(actual)$acf,
    as.numeric(stats::acf(data$y[!is.na(data$y)], plot = FALSE, lag.max = 8L)$acf)[-1L], tolerance = 1e-10)
  expect_match(paste(unlist(actual$results$acfMissingNote), collapse = " "), "24 of 48", fixed = TRUE)
})

test_that("Spectral policy selects a complete segment after ordering and filtering", {
  data <- ssPreparationFixture()
  data$y[c(1L, 12L, 48L)] <- NA_real_
  input <- data[c(25:48, 1:24), ]
  options <- ssPreparationOptions("SpectralTimeSeries", filter = TRUE, filterBy = "time",
    timeStart = data$Time[5], timeEnd = data$Time[43])
  for (policy in c("listwise", "longest")) {
    actual <- ssPreparationRun("SpectralTimeSeries", input, utils::modifyList(options, list(naAction = policy)))
    selected <- if (policy == "listwise") c(5:11, 13:43) else 13:43
    reference <- ssPreparationRun("SpectralTimeSeries", data[selected, ],
      ssPreparationOptions("SpectralTimeSeries", time = ""))
    expect_equal(ssPreparationTable(actual, "bandWidthTable"), ssPreparationTable(reference, "bandWidthTable"), tolerance = 1e-10)
    expect_equal(ssPreparationPlot(actual, "powerSpectralDensity")$data,
                 ssPreparationPlot(reference, "powerSpectralDensity")$data, tolerance = 1e-10)
    ssPreparationSpectrum(actual, data$y[selected], options)
    expect_match(paste(unlist(actual$results$spectralMissingNote), collapse = " "),
      paste(length(selected), "of 39"), fixed = TRUE)
    expect_match(paste(unlist(actual$results$bandWidthTable$footnotes), collapse = " "),
      if (policy == "listwise") "internal gaps are ignored" else "steps 9 to 39", fixed = TRUE)
  }
})

test_that("Spectral outputs report unusable filtered data locally for both policies", {
  data <- ssPreparationFixture()
  data$y[5:43] <- NA_real_
  input <- data[c(25:48, 1:24), ]
  for (policy in c("listwise", "longest")) {
    options <- ssPreparationOptions("SpectralTimeSeries", filter = TRUE, filterBy = "time",
      timeStart = data$Time[5], timeEnd = data$Time[43], naAction = policy)
    actual <- ssPreparationRun("SpectralTimeSeries", input, options)
    for (output in c("bandWidthTable", "powerSpectralDensity"))
      expect_match(paste(unlist(actual$results[[output]]), collapse = " "), "no observed values", fixed = TRUE)
    expect_null(actual$results$spectralMissingNote)
  }
})

test_that("Stationarity defaults omit all missing observations with local explanatory notes", {
  data <- ssPreparationFixture()
  data$y[c(1L, 12L, 29L, 48L)] <- NA_real_
  input <- data[c(25:48, 1:24), ]
  before <- serialize(input, NULL)
  options <- ssPreparationOptions("StationarityTimeSeries", pacf = TRUE, pacfMaxLag = 8L)
  for (name in c("testNaAction", "acfNaAction", "pacfNaAction"))
    expect_identical(options[[name]], "listwise")
  actual <- ssPreparationRun("StationarityTimeSeries", input, options)
  observed <- data[!is.na(data$y), ]
  reference <- ssPreparationRun("StationarityTimeSeries", observed,
    utils::modifyList(options, list(time = "")))
  # The omission note shifts footnote identifiers, but no test results.
  values <- function(rows) lapply(rows, function(row) row[names(row) != ".footnotes"])
  expect_equal(values(ssPreparationTable(actual, "stationaryTable")),
               values(ssPreparationTable(reference, "stationaryTable")), tolerance = 1e-10)
  expect_equal(ssPreparationAcf(actual)$acf,
    as.numeric(stats::acf(observed$y, plot = FALSE, lag.max = 8L)$acf)[-1L], tolerance = 1e-10)
  pacf <- ssPreparationPlot(actual, "pacfPlot")
  layer <- Filter(function(layer) inherits(layer$geom, "GeomLinerange"), pacf$layers)[[1L]]
  expect_equal(layer$data$acf, as.numeric(stats::pacf(observed$y, plot = FALSE, lag.max = 8L)$acf), tolerance = 1e-10)
  for (note in list(actual$results$stationaryTable$footnotes,
                   actual$results$acfMissingNote, actual$results$pacfMissingNote)) {
    text <- paste(unlist(note), collapse = " ")
    expect_match(text, "44 of 48", fixed = TRUE)
    expect_match(text, "consecutive time steps", fixed = TRUE)
    expect_match(text, "internal gaps are ignored", fixed = TRUE)
  }
  order <- vapply(actual$results$.meta, `[[`, "", "name")
  expect_true(all(c("acfPlot", "acfMissingNote", "pacfPlot", "pacfMissingNote") %in% order))
  expect_equal(match("acfMissingNote", order), match("acfPlot", order) + 1L)
  expect_equal(match("pacfMissingNote", order), match("pacfPlot", order) + 1L)
  expect_null(reference$results$acfMissingNote)
  expect_null(reference$results$pacfMissingNote)
  expect_identical(serialize(input, NULL), before)
})

test_that("Spectral default omission compresses only the calculation and explains it", {
  data <- ssPreparationFixture()
  data$y[c(1L, 12L, 29L, 48L)] <- NA_real_
  input <- data[c(25:48, 1:24), ]
  before <- serialize(input, NULL)
  options <- ssPreparationOptions("SpectralTimeSeries")
  expect_identical(options$naAction, "listwise")
  actual <- ssPreparationRun("SpectralTimeSeries", input, options)
  ssPreparationSpectrum(actual, data$y[!is.na(data$y)], options)
  for (note in list(actual$results$bandWidthTable$footnotes, actual$results$spectralMissingNote)) {
    text <- paste(unlist(note), collapse = " ")
    expect_match(text, "44 of 48", fixed = TRUE)
    expect_match(text, "consecutive time steps", fixed = TRUE)
    expect_match(text, "internal gaps are ignored", fixed = TRUE)
  }
  order <- vapply(actual$results$.meta, `[[`, "", "name")
  expect_true(all(c("powerSpectralDensity", "spectralMissingNote") %in% order))
  expect_equal(match("spectralMissingNote", order), match("powerSpectralDensity", order) + 1L)
  complete <- ssPreparationRun("SpectralTimeSeries", ssPreparationFixture(), options)
  expect_null(complete$results$spectralMissingNote)
  expect_identical(serialize(input, NULL), before)
})

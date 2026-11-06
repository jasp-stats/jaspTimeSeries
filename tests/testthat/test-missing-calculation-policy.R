.tsPolicyOutput <- function(title = "", ...) {
  object <- new.env(parent = emptyenv())
  object$title <- title
  object$rows <- object$footnotes <- object$columns <- list()
  object$dependOn <- function(options) object$dependencies <- options
  object$setError <- function(message) object$error <- message
  object$addColumnInfo <- function(name, ...) object$columns[[name]] <- list(...)
  object$addRows <- function(row) object$rows[[length(object$rows) + 1L]] <- row
  object$setRowName <- function(...) NULL
  object$addFootnote <- function(message, ...) object$footnotes[[length(object$footnotes) + 1L]] <- list(message = message, ...)
  object
}

.tsPolicyScope <- function() {
  scope <- new.env(parent = asNamespace("jaspTimeSeries"))
  for (name in c(".tsCreateTableStationarityTests", ".tsACFTransformation", ".tsPACFTransformation",
                 ".tsComputeSpectralResults", ".tsPowerSpectralDensity", ".tsFillPowerSpectralDensity",
                 ".tsCreateTableBandWith", ".tsResidualDiagnostics", ".tsFillLjungBoxPlot")) {
    fun <- getFromNamespace(name, "jaspTimeSeries")
    environment(fun) <- scope
    scope[[name]] <- fun
  }
  scope$createJaspTable <- scope$createJaspPlot <- scope$createJaspContainer <- .tsPolicyOutput
  scope$createJaspHtml <- function(...) list(...)
  scope$createJaspState <- function(object) {
    state <- .tsPolicyOutput()
    state$object <- object
    state
  }
  scope
}

.tsPolicyOptions <- function(...) utils::modifyList(list(
  adfTest = FALSE, ppTestRegressionCoefficient = FALSE, ppTestStudentized = FALSE,
  kpssLevel = TRUE, kpssTrend = FALSE, testMissingValues = "error",
  acf = TRUE, acfZeroLag = FALSE, acfMaxLag = 3L, acfCi = TRUE, acfCiLevel = .95,
  acfCiType = "whiteNoise", acfMissingValues = "error",
  pacf = TRUE, pacfMaxLag = 3L, pacfCi = TRUE, pacfCiLevel = .95,
  pacfCiType = "whiteNoise", pacfMissingValues = "error",
  kernel = FALSE, taper = .1, demean = TRUE, detrend = FALSE, missingValues = "error",
  whiteNoise = FALSE, pinkNoise = FALSE, brownNoise = FALSE,
  residualAcf = TRUE, residualAcfCi = TRUE, residualAcfCiLevel = .95,
  residualAcfZeroLag = FALSE, residualAcfMissingValues = "error",
  residualLjungBox = TRUE, residualLjungBoxMissingValues = "error", residualMaxLag = 3L,
  ljungBoxSignificanceLevel = .05, residualTimeSeries = FALSE, residualQQ = FALSE
), list(...))

.tsPolicyPValueBounds <- function(code) withCallingHandlers(code, warning = function(warning) {
  if (grepl("^p-value (smaller|greater) than printed p-value$", conditionMessage(warning)))
    invokeRestart("muffleWarning")
})

test_that("new Time Series diagnostic and wrapper defaults omit missing observations", {
  analyses <- list(
    StationarityTimeSeries = c("testMissingValues", "acfMissingValues", "pacfMissingValues"),
    SpectralTimeSeries = "missingValues",
    ARIMATimeSeries = c("residualAcfMissingValues", "residualLjungBoxMissingValues")
  )
  for (analysis in names(analyses)) {
    options <- jaspTools::analysisOptions(analysis)
    for (name in analyses[[analysis]]) expect_identical(options[[name]], "omit")
  }
  for (package in c("jaspTimeSeries", "jaspDescriptives")) {
    wrapper <- getExportedValue(package, "DescriptivesTimeSeries")
    expect_identical(formals(wrapper)$acfMissingValues, "omit")
    expect_identical(formals(wrapper)$pacfMissingValues, "omit")
  }
})

test_that("Stationarity tests select complete inputs without changing the observation grid", {
  scope <- .tsPolicyScope()
  values <- 2 + sin(1:20) + (1:20) / 50
  fixtures <- list(
    list(y = c(NA_real_, values, NA_real_), policy = "trim", expected = values),
    list(y = c(NA_real_, values, NA_real_, values + 5), policy = "longest", expected = values)
  )
  for (fixture in fixtures) {
    data <- data.frame(y = fixture$y, t = seq_along(fixture$y))
    before <- serialize(data, NULL)
    result <- new.env(parent = emptyenv())
    # tseries warns when a p-value reaches the bounds of its critical-value table.
    .tsPolicyPValueBounds(scope$.tsCreateTableStationarityTests(result, data,
      .tsPolicyOptions(testMissingValues = fixture$policy), TRUE, 2, "dependent"))
    table <- result$stationaryTable
    expected <- .tsPolicyPValueBounds(tseries::kpss.test(fixture$expected, null = "Level"))
    expect_null(table$error)
    expect_equal(table$rows[[1L]]$statistic, unname(expected$statistic))
    expect_equal(table$rows[[1L]]$p, unname(expected$p.value))
    expect_match(table$footnotes[[1L]]$message, "steps 2 to 21", fixed = TRUE)
    expect_true("testMissingValues" %in% table$dependencies)
    expect_identical(serialize(data, NULL), before)
  }
  for (policy in c("error", "trim")) {
    result <- new.env(parent = emptyenv())
    scope$.tsCreateTableStationarityTests(result, data.frame(y = c(values, NA_real_, values), t = 1:41),
      .tsPolicyOptions(testMissingValues = policy), TRUE, 2, "dependent")
    expect_match(result$stationaryTable$error, "missing", ignore.case = TRUE)
    expect_length(result$stationaryTable$rows, 0L)
  }
})

test_that("Stationarity backend failures remain local to the table", {
  scope <- .tsPolicyScope()
  result <- new.env(parent = emptyenv())
  expect_no_error(scope$.tsCreateTableStationarityTests(result,
    data.frame(y = c(1, 2), t = 1:2), .tsPolicyOptions(adfTest = TRUE, kpssLevel = FALSE), TRUE, 2, character()))
  expect_match(result$stationaryTable$error, "ADF test failed", fixed = TRUE)
  for (values in list(c(1, Inf), c(1, 1), c(NA_real_, NA_real_))) {
    result <- new.env(parent = emptyenv())
    expect_no_error(scope$.tsCreateTableStationarityTests(result, data.frame(y = values, t = 1:2),
      .tsPolicyOptions(), TRUE, 2, character()))
    expect_type(result$stationaryTable$error, "character")
  }
})

test_that("Stationarity ACF and PACF apply independent policies and note dependencies", {
  scope <- .tsPolicyScope()
  data <- data.frame(y = c(NA_real_, 2 + sin(1:20), NA_real_), t = 1:22)
  before <- serialize(data, NULL)
  result <- new.env(parent = emptyenv())
  options <- .tsPolicyOptions(acfMissingValues = "trim", pacfMissingValues = "error")
  scope$.tsACFTransformation(result, data, options, TRUE, 3, c("dependent", "acf"))
  scope$.tsPACFTransformation(result, data, options, TRUE, 4, c("dependent", "pacf"))
  expect_null(result$acfPlot$error)
  expect_s3_class(result$acfPlot$plotObject, "ggplot")
  expect_match(result$acfMissingNote$text, "20 of 22", fixed = TRUE)
  expect_identical(result$acfMissingNote$dependencies, result$acfPlot$dependencies)
  expect_true("acfMissingValues" %in% result$acfPlot$dependencies)
  expect_match(result$pacfPlot$error, "missing observations", fixed = TRUE)
  expect_null(result$pacfMissingNote)
  expect_true("pacfMissingValues" %in% result$pacfPlot$dependencies)
  expect_identical(serialize(data, NULL), before)
})

test_that("Ljung-Box uses the selected complete segment and validates its maximum lag", {
  scope <- .tsPolicyScope()
  values <- 2 + sin(1:12)
  data <- data.frame(y = c(NA_real_, values, NA_real_, 1, 2), t = 1:16)
  before <- serialize(data, NULL)
  plot <- .tsPolicyOutput()
  note <- scope$.tsFillLjungBoxPlot(plot, data, .tsPolicyOptions(residualLjungBoxMissingValues = "longest"))
  expect_null(plot$error)
  expect_match(note, "12 of 16", fixed = TRUE)
  expected <- vapply(1:3, function(lag) stats::Box.test(values, lag, type = "Ljung-Box")$p.value, numeric(1L))
  expect_equal(ggplot2::ggplot_build(plot$plotObject)$data[[2L]]$y, expected)
  for (options in list(.tsPolicyOptions(),
      .tsPolicyOptions(residualLjungBoxMissingValues = "trim"),
      .tsPolicyOptions(residualLjungBoxMissingValues = "longest", residualMaxLag = 12L))) {
    plot <- .tsPolicyOutput()
    expect_null(scope$.tsFillLjungBoxPlot(plot, data, options))
    expect_type(plot$error, "character")
    expect_null(plot$plotObject)
  }
  expect_identical(serialize(data, NULL), before)
})

test_that("Spectral state caches selection notes and local failures", {
  scope <- .tsPolicyScope()
  values <- 2 + sin(1:20) + cos((1:20) / 3)
  data <- data.frame(y = c(NA_real_, values, NA_real_), t = 1:22)
  before <- serialize(data, NULL)
  result <- new.env(parent = emptyenv())
  options <- .tsPolicyOptions(missingValues = "trim")
  dependencies <- getFromNamespace(".tsSpectralDependencies", "jaspTimeSeries")()
  legacyState <- list(object = list(freq = 1, spec = 5, bandwidth = .1))
  result$spectralResult <- legacyState
  scope$.tsPowerSpectralDensity(result, data, options, TRUE, 2, dependencies)
  scope$.tsCreateTableBandWith(result, data, options, TRUE, 1, dependencies)
  expected <- stats::spec.pgram(stats::ts(values), taper = .1, demean = TRUE, detrend = FALSE, plot = FALSE, na.action = na.fail)
  expect_equal(result$spectralResultWithMissingValues$object$spectrum$spec, expected$spec)
  expect_equal(result$bandWidthTable$rows[[1L]]$bandwidth, expected$bandwidth)
  expect_match(result$spectralMissingNote$text, "steps 2 to 21", fixed = TRUE)
  expect_identical(result$spectralMissingNote$text, result$bandWidthTable$footnotes[[1L]]$message)
  expect_true("missingValues" %in% result$spectralResultWithMissingValues$dependencies)
  expect_true("missingValues" %in% result$powerSpectralDensity$dependencies)
  expect_identical(result$spectralResult, legacyState)
  state <- result$spectralResultWithMissingValues
  scope$.tsComputeSpectralResults(data, options, result, TRUE)
  expect_identical(result$spectralResultWithMissingValues, state)
  failed <- new.env(parent = emptyenv())
  expect_no_error(scope$.tsPowerSpectralDensity(failed, data, .tsPolicyOptions(), TRUE, 2, "dependent"))
  expect_no_error(scope$.tsCreateTableBandWith(failed, data, .tsPolicyOptions(), TRUE, 1, "dependent"))
  expect_identical(failed$powerSpectralDensity$error, failed$bandWidthTable$error)
  expect_match(failed$spectralResultWithMissingValues$object$error, "missing observations", fixed = TRUE)
  expect_null(failed$spectralMissingNote)
  expect_null(failed$spectralResultWithMissingValues$object$spectrum)
  expect_identical(serialize(data, NULL), before)
})

test_that("zero spectral density fails only the logarithmic plot", {
  scope <- .tsPolicyScope()
  result <- new.env(parent = emptyenv())
  data <- data.frame(y = 1:8, t = 1:8)
  options <- .tsPolicyOptions(detrend = TRUE)
  scope$.tsPowerSpectralDensity(result, data, options, TRUE, 2, "dependent")
  scope$.tsCreateTableBandWith(result, data, options, TRUE, 1, "dependent")
  expect_match(result$powerSpectralDensity$error, "logarithmic scale", fixed = TRUE)
  expect_null(result$bandWidthTable$error)
  expect_length(result$bandWidthTable$rows, 1L)
  expect_null(result$spectralResultWithMissingValues$object$error)
})

test_that("ARIMA residual policies affect diagnostic copies without changing the fit", {
  scope <- .tsPolicyScope()
  k <- 1:48
  y <- 10 + .6 * sin(k / 5) - .3 * cos(k / 9) + sin(k / 3) + ((k * 7) %% 11) / 20
  y[c(1L, 2L, 25L, 48L)] <- NA_real_
  fit <- forecast::Arima(stats::ts(y), order = c(1, 0, 0))
  before <- serialize(fit, NULL)
  data <- data.frame(y = y, t = 1:48)
  for (policy in c("error", "longest")) {
    result <- new.env(parent = emptyenv())
    options <- .tsPolicyOptions(residualAcfMissingValues = policy, residualLjungBoxMissingValues = policy)
    expect_no_error(scope$.tsResidualDiagnostics(result, fit, data, options, TRUE, 5, "dependent"))
    plots <- result$residContainer
    if (policy == "error") {
      expect_match(plots$residualACFPlot$error, "missing observations", fixed = TRUE)
      expect_match(plots$ljungPlot$error, "missing observations", fixed = TRUE)
      expect_null(plots$residualAcfMissingNote)
      expect_null(plots$residualLjungBoxMissingNote)
    } else {
      expect_null(plots$residualACFPlot$error)
      expect_null(plots$ljungPlot$error)
      expect_match(plots$residualAcfMissingNote$text, "steps 3 to 24", fixed = TRUE)
      expect_identical(plots$residualAcfMissingNote$text, plots$residualLjungBoxMissingNote$text)
      expect_identical(plots$residualAcfMissingNote$dependencies, plots$residualACFPlot$dependencies)
      expect_identical(plots$residualLjungBoxMissingNote$dependencies, plots$ljungPlot$dependencies)
    }
    expect_identical(serialize(fit, NULL), before)
  }
  modelDependencies <- getFromNamespace(".tsArimaDependencies", "jaspTimeSeries")()
  expect_false(any(c("residualAcfMissingValues", "residualLjungBoxMissingValues") %in% modelDependencies))
})

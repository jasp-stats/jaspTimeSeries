.arimaLbFunction <- function(name) getFromNamespace(name, "jaspTimeSeries")

.arimaLbPlot <- function() {
  plot <- new.env(parent = emptyenv())
  plot$setError <- function(message) plot$error <- message
  plot
}

.arimaLbOptions <- function(...) utils::modifyList(list(
  residualLjungBoxNaAction = "listwise", residualMaxLag = 6L, ljungBoxSignificanceLevel = .05,
  seasonal = FALSE, periodSpecification = "custom", m = 12L
), list(...))

.arimaLbRun <- function(y, ...) {
  options <- jaspTools::analysisOptions("ARIMATimeSeries")
  options$dependent <- "y"
  options$dependent.types <- "scale"
  options$time <- ""
  options$timeSeriesPlot <- options$residualTimeSeries <- FALSE
  options$residualAcf <- options$residualQQ <- FALSE
  options$residualLjungBox <- TRUE
  options$residualMaxLag <- 8L
  options$residualSavedToData <- FALSE
  options$residualColumn <- ""
  options$forecastSave <- ""
  options$forecastLength <- 0L
  options$forecastTable <- options$forecastTimeSeries <- FALSE
  options <- utils::modifyList(options, list(...))
  result <- jaspTools::runAnalysis("ARIMATimeSeries", dataset = data.frame(y = y),
                                  options = options, view = FALSE, quiet = TRUE)
  fits <- Filter(function(value) inherits(value, "Arima"), result$state$other)
  list(result = result, fit = if (length(fits)) fits[[1L]] else NULL)
}

.arimaLbPoints <- function(run) {
  plots <- run$result$results$residContainer$collection
  expect_identical(plots$residContainer_ljungPlot$status, "complete")
  plot <- run$result$state$figures[[plots$residContainer_ljungPlot$data]]$obj
  ggplot2::ggplot_build(plot)$data[[2L]][, c("x", "y")]
}

.arimaLbSeries <- function(n = 120L, seed = 20261111L) {
  set.seed(seed)
  as.numeric(stats::arima.sim(list(ar = .5, ma = .3), n = n)) + 10
}

test_that("Ljung-Box p-values subtract fitted coefficients and omit untestable lags", {
  fill <- .arimaLbFunction(".tsFillLjungBoxPlot")
  y <- .arimaLbSeries()
  data <- data.frame(y = y, t = seq_along(y))
  for (fitdf in 0:3) {
    plot <- .arimaLbPlot()
    note <- fill(plot, data, .arimaLbOptions(), fitdf = fitdf)
    expect_null(plot$error)
    built <- ggplot2::ggplot_build(plot$plotObject)$data[[2L]]
    lags <- seq.int(fitdf + 1L, 6L)
    expect_equal(built$x, lags)
    expect_equal(built$y, vapply(lags, function(lag) stats::Box.test(y, lag, type = "Ljung-Box", fitdf = fitdf)$p.value, numeric(1L)))
    if (fitdf == 0L) expect_null(note) else expect_match(note, paste0("account for the ", fitdf, " estimated"), fixed = TRUE)
  }
  plot <- .arimaLbPlot()
  expect_null(fill(plot, data, .arimaLbOptions(residualMaxLag = 2L), fitdf = 2L))
  expect_match(plot$error, "larger than the number of estimated AR and MA coefficients (2)", fixed = TRUE)
  expect_null(plot$plotObject)
})

test_that("ARIMA Ljung-Box uses the fitted nonseasonal orders in manual and automatic models", {
  y <- .arimaLbSeries()
  for (spec in list(list(modelSpecification = "custom", p = 1L, d = 0L, q = 1L),
                    list(modelSpecification = "custom", p = 2L, d = 1L, q = 0L),
                    list(modelSpecification = "auto"))) {
    run <- do.call(.arimaLbRun, c(list(y = y), spec))
    fitdf <- sum(forecast::arimaorder(run$fit)[c("p", "q")])
    if (spec$modelSpecification == "custom") expect_identical(as.integer(fitdf), spec$p + spec$q)
    residuals <- getFromNamespace(".tsResiduals", "jaspTimeSeries")(run$fit)
    residuals <- residuals[!is.na(residuals)]
    lags <- seq.int(fitdf + 1L, 8L)
    points <- .arimaLbPoints(run)
    expect_equal(points$x, lags)
    expect_equal(points$y, vapply(lags, function(lag)
      stats::Box.test(residuals, lag, type = "Ljung-Box", fitdf = fitdf)$p.value, numeric(1L)), tolerance = 1e-10)
  }
})

test_that("ARIMA Ljung-Box does not subtract seasonal coefficients", {
  set.seed(7)
  y <- 10 + rep(c(2, -1, 0, -1), 30) + as.numeric(stats::arima.sim(list(ar = .4), n = 120))
  run <- .arimaLbRun(y, modelSpecification = "custom", p = 1L, d = 0L, q = 0L,
                     seasonal = TRUE, periodSpecification = "custom", m = 4L, P = 1L, D = 0L, Q = 1L)
  order <- forecast::arimaorder(run$fit)
  expect_equal(order[c("p", "q", "P", "Q", "Frequency")], c(p = 1, q = 0, P = 1, Q = 1, Frequency = 4))
  expect_equal(.arimaLbPoints(run)$x, 2:8)
})

test_that("seasonal ARIMA requires a period of at least 2", {
  period <- .arimaLbFunction(".tsArimaSeasonalPeriod")
  y <- .arimaLbSeries()
  expect_identical(period(y, .arimaLbOptions(seasonal = FALSE, m = 1L)), list(seasonal = FALSE, m = 1, note = NULL))
  expect_identical(period(y, .arimaLbOptions(seasonal = TRUE, m = 4L)), list(seasonal = TRUE, m = 4L, note = NULL))
  expect_error(period(y, .arimaLbOptions(seasonal = TRUE, m = 1L)), "seasonal period must be at least 2")
  for (spec in c("custom", "auto")) {
    run <- .arimaLbRun(y, modelSpecification = spec, p = 1L, d = 0L, q = 0L, P = 1L, D = 0L, Q = 0L,
                       seasonal = TRUE, periodSpecification = "custom", m = 1L)
    expect_false(identical(run$result$status, "complete"))
    expect_match(paste(unlist(run$result$results), collapse = " "), "seasonal period must be at least 2", fixed = TRUE)
  }
})

test_that("a dominant period of 1 fits a nonseasonal model with a note instead of duplicate lag-1 terms", {
  set.seed(11)
  y <- rnorm(120) + 5
  expect_equal(forecast::findfrequency(y), 1)
  for (spec in c("custom", "auto")) {
    run <- .arimaLbRun(y, modelSpecification = spec, p = 1L, d = 0L, q = 0L, P = 1L, D = 0L, Q = 0L,
                       seasonal = TRUE, periodSpecification = "dominant")
    expect_identical(run$result$status, "complete")
    # A nonseasonal fit reports only p, d and q.
    expect_identical(names(forecast::arimaorder(run$fit)), c("p", "d", "q"))
    labels <- vapply(run$result$results$coefTable$data, function(row) row$coefficients, character(1L))
    expect_false(any(grepl("seasonal", labels, fixed = TRUE)))
    notes <- vapply(run$result$results$modelTable$footnotes, function(note) note$text, character(1L))
    expect_true(any(grepl("No dominant seasonal period was found", notes, fixed = TRUE)))
  }
})

test_that("a detected dominant period is used and reported", {
  set.seed(5)
  y <- 20 + 3 * sin(2 * pi * (1:120) / 6) + rnorm(120, sd = .3)
  m <- forecast::findfrequency(y)
  expect_gte(m, 2)
  run <- .arimaLbRun(y, modelSpecification = "custom", p = 1L, d = 0L, q = 0L, P = 1L, D = 0L, Q = 0L,
                     seasonal = TRUE, periodSpecification = "dominant")
  expect_identical(run$result$status, "complete")
  order <- forecast::arimaorder(run$fit)
  expect_equal(order[c("P", "Q", "Frequency")], c(P = 1, Q = 0, Frequency = m))
  notes <- vapply(run$result$results$modelTable$footnotes, function(note) note$text, character(1L))
  expect_true(any(grepl(paste0("Seasonal period: ", m, " (dominant period)"), notes, fixed = TRUE)))
})

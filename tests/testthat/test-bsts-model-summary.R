# Deterministic draws exercise the real bsts summary without running MCMC.
.bstsSummaryTestFit <- function() {
  loadNamespace("bsts")
  structure(list(
    niter = 4L,
    family = "gaussian",
    sigma.obs = c(0.1, 0.2, 0.3, 0.4),
    original.series = c(1, 4, 2, 6),
    one.step.prediction.errors = matrix(seq_len(16) / 10, nrow = 4),
    has.regression = FALSE,
    timestamp.info = list(timestamps.are.trivial = TRUE, timestamps = 1:4)
  ), class = "bsts")
}

# Plain R output collector, not an internal JASP C++ result object.
.bstsSummaryTestTable <- function() {
  table <- new.env(parent = emptyenv())
  table$rows <- list()
  table$footnotes <- list()
  table$addRows <- function(row) table$rows[[length(table$rows) + 1L]] <- row
  table$addFootnote <- function(message, colNames) {
    table$footnotes[[length(table$footnotes) + 1L]] <- list(message = message, colNames = colNames)
  }
  table
}

test_that("BSTS preserves available summary statistics and the selected burn-in", {
  fill <- getFromNamespace(".bstsFillModelSummaryTable", "jaspTimeSeries")
  fit <- .bstsSummaryTestFit()
  before <- serialize(fit, NULL)
  for (burn in c(0, 1, 3)) {
    table <- .bstsSummaryTestTable()
    expected <- summary(fit, burn = burn)
    fill(table, fit, TRUE, burn)
    expect_identical(table$rows, list(list(resSd = expected$residual.sd,
      predSd = expected$prediction.sd, R2 = expected$rsquare, relGof = expected$relative.gof)))
    expect_length(table$footnotes, 0)
  }
  expect_identical(serialize(fit, NULL), before)
})

test_that("BSTS explains unavailable Harvey statistics for missing responses", {
  fill <- getFromNamespace(".bstsFillModelSummaryTable", "jaspTimeSeries")
  fit <- .bstsSummaryTestFit()
  fit$original.series[2] <- NA_real_
  before <- serialize(fit, NULL)
  expected <- summary(fit, burn = 1)
  table <- .bstsSummaryTestTable()
  fill(table, fit, TRUE, 1)
  expect_true(is.na(expected$relative.gof))
  expect_identical(table$rows, list(list(resSd = expected$residual.sd,
    predSd = expected$prediction.sd, R2 = expected$rsquare, relGof = ".")))
  expect_identical(table$footnotes, list(list(
    message = "Harvey's goodness of fit is unavailable because the dependent variable contains missing observations.",
    colNames = "relGof")))
  expect_identical(serialize(fit, NULL), before)
})

test_that("BSTS does not misattribute other undefined Harvey statistics to missing data", {
  fill <- getFromNamespace(".bstsFillModelSummaryTable", "jaspTimeSeries")
  fit <- .bstsSummaryTestFit()
  fit$original.series <- 1:4
  # Zero variance in the first differences and zero prediction SSE yield NaN.
  fit$one.step.prediction.errors[,] <- 0
  expect_true(is.nan(summary(fit, burn = 1)$relative.gof))
  table <- .bstsSummaryTestTable()
  fill(table, fit, TRUE, 1)
  expect_identical(table$rows[[1]]$relGof, ".")
  expect_identical(table$footnotes, list(list(
    message = "Harvey's goodness of fit could not be computed.", colNames = "relGof")))

  # Do not change the existing handling of an infinite (rather than NA) value.
  fit$one.step.prediction.errors[,] <- 1
  table <- .bstsSummaryTestTable()
  fill(table, fit, TRUE, 1)
  expect_identical(table$rows[[1]]$relGof, -Inf)
  expect_length(table$footnotes, 0)
})

test_that("BSTS does not populate a not-ready summary", {
  table <- .bstsSummaryTestTable()
  getFromNamespace(".bstsFillModelSummaryTable", "jaspTimeSeries")(table, NULL, FALSE, 0)
  expect_length(table$rows, 0)
  expect_length(table$footnotes, 0)
})

.bstsThresholdNoteFixture <- function() {
  state <- array(outer(c(-.3, -.2, -.1, .1, .2, .3), c(0, 1, 0, 5, 6, 7), "+"),
                 dim = c(6L, 1L, 6L))
  list(state.contributions = state, niter = 6L)
}

.bstsThresholdNoteOptions <- function(time = seq_len(6L), ...) {
  utils::modifyList(list(time = time, burn = 1L, controlChartPeriodEnd = 3L,
    controlChartSigmaThreshold = 2, controlChartPlot = TRUE,
    controlChartProbabilisticPlot = TRUE), list(...))
}

.bstsThresholdNoteScope <- function() {
  scope <- new.env(parent = asNamespace("jaspTimeSeries"))
  for (name in c(".bstsCreateControlPlots", ".bstsFillControlPlotThreshold",
                 ".bstsFillControlPlotProbability")) {
    fun <- getFromNamespace(name, "jaspTimeSeries")
    environment(fun) <- scope
    scope[[name]] <- fun
  }
  scope$createJaspPlot <- scope$createJaspContainer <- function(...) {
    object <- list2env(list(...), parent = emptyenv())
    object$dependOn <- function(dependencies) object$dependencies <- dependencies
    object$setError <- function(message) object$error <- message
    object
  }
  scope$createJaspHtml <- function(...) list(...)
  scope
}

.bstsThresholdNoteRun <- function(options = .bstsThresholdNoteOptions(), ready = TRUE,
                                  fit = .bstsThresholdNoteFixture()) {
  scope <- .bstsThresholdNoteScope()
  main <- new.env(parent = emptyenv())
  main$bstsModelResults <- list(object = fit)
  scope$.bstsCreateControlPlots(list(bstsMainContainer = main), options, ready)
  main$bstsControlPlots
}

test_that("BSTS threshold explanations are separate notes immediately below the plot", {
  times <- list(2000 + seq_len(6L) / 4,
    as.Date("2024-01-01") + 0:5,
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + (0:5) * 3600)
  for (time in times) {
    result <- .bstsThresholdNoteRun(.bstsThresholdNoteOptions(time))
    note <- result$bstsControlPlotThresholdNote
    expect_match(note$text, "<i>Note.</i> The lower bound of the 95% state credible interval", fixed = TRUE)
    expect_match(note$text, "upper control limit (2&sigma;)", fixed = TRUE)
    expect_match(note$text, paste0("at Time = ", as.character(time[4L]), "."), fixed = TRUE)
    expect_null(result$bstsControlPlotThreshold$plotObject$labels$caption)
    expect_lt(result$bstsControlPlotThreshold$position, note$position)
    expect_lt(note$position, result$bstsControlPlotProbability$position)
    expect_equal(lengths(regmatches(note$text, gregexpr("<i>", note$text, fixed = TRUE))), 1L)
  }
})

test_that("BSTS threshold notes distinguish no crossing and changed control settings", {
  result <- .bstsThresholdNoteRun(.bstsThresholdNoteOptions(controlChartSigmaThreshold = 100))
  expect_match(result$bstsControlPlotThresholdNote$text,
    "does not exceed the upper control limit (100&sigma;) at any time point.", fixed = TRUE)
  expect_false(grepl("at Time =", result$bstsControlPlotThresholdNote$text, fixed = TRUE))

  changedSigma <- .bstsThresholdNoteRun(.bstsThresholdNoteOptions(controlChartSigmaThreshold = 10))
  expect_match(changedSigma$bstsControlPlotThresholdNote$text, "at Time = 6.", fixed = TRUE)
  changedPeriod <- .bstsThresholdNoteRun(.bstsThresholdNoteOptions(controlChartPeriodEnd = 4L))
  expect_match(changedPeriod$bstsControlPlotThresholdNote$text, "at Time = 6.", fixed = TRUE)

  # Equality is not a crossing: the existing comparison is strictly greater.
  fit <- .bstsThresholdNoteFixture()
  fit$state.contributions[] <- 0
  equalLimit <- .bstsThresholdNoteRun(fit = fit)
  expect_match(equalLimit$bstsControlPlotThresholdNote$text, "does not exceed", fixed = TRUE)
})

test_that("BSTS threshold notes inherit control dependencies and are absent without a valid plot", {
  result <- .bstsThresholdNoteRun()
  expect_true(all(c("time", "burninMethod", "burninAutomaticProportion", "burninManualDraws",
    "controlChartPlot", "controlChartPeriodEnd", "controlChartSigmaThreshold",
    "controlChartProbabilisticPlot") %in% result$dependencies))

  hidden <- .bstsThresholdNoteRun(.bstsThresholdNoteOptions(controlChartPlot = FALSE))
  expect_null(hidden$bstsControlPlotThreshold)
  expect_null(hidden$bstsControlPlotThresholdNote)
  expect_s3_class(hidden$bstsControlPlotProbability$plotObject, "ggplot")

  invalid <- .bstsThresholdNoteRun(.bstsThresholdNoteOptions(controlChartPeriodEnd = 1L))
  expect_match(invalid$bstsControlPlotThreshold$error, "Control period end", fixed = TRUE)
  expect_null(invalid$bstsControlPlotThresholdNote)
  expect_null(.bstsThresholdNoteRun(ready = FALSE))
})

test_that("BSTS threshold note text is translated as complete messages", {
  scope <- .bstsThresholdNoteScope()
  messages <- character()
  scope$gettextf <- function(fmt, ...) {
    messages <<- c(messages, fmt)
    paste0("translated: ", sprintf(fmt, ...))
  }
  for (sigma in c(2, 100)) {
    output <- new.env(parent = emptyenv())
    scope$.bstsFillControlPlotThreshold(output, .bstsThresholdNoteFixture(),
      .bstsThresholdNoteOptions(controlChartSigmaThreshold = sigma), TRUE)
    expect_match(output$bstsControlPlotThresholdNote$text, "translated: <i>Note.</i>", fixed = TRUE)
  }
  expect_length(unique(messages), 2L)
  expect_true(all(grepl("%1$.0f%%", messages, fixed = TRUE)))
  expect_true(all(grepl("%2$g", messages, fixed = TRUE)))
  expect_true(any(grepl("%3$s", messages, fixed = TRUE)))
})

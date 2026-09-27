test_that("BSTS validates only requested control plots and keeps model output", {
  builder <- getFromNamespace(".bstsCreateControlPlots", "jaspTimeSeries")
  run <- function(period, threshold = TRUE, probability = TRUE) {
    scope <- new.env(parent = environment(builder))
    container <- new.env(parent = emptyenv())
    container$dependOn <- function(...) NULL
    scope$createJaspContainer <- function(...) container
    # Plain R collectors, not internal JASP C++ result objects.
    scope$createJaspPlot <- function(...) {
      plot <- new.env(parent = emptyenv())
      plot$setError <- function(message) plot$error <- message
      plot
    }
    called <- character()
    scope$.bstsFillControlPlotThreshold <- function(...) called <<- c(called, "threshold")
    scope$.bstsFillControlPlotProbability <- function(...) called <<- c(called, "probability")
    environment(builder) <- scope
    main <- new.env(parent = emptyenv())
    fit <- list(state.contributions = array(seq_len(24), c(4, 1, 6)))
    main$bstsModelResults <- list(object = fit)
    main$bstsModelSummaryTable <- "existing model summary"
    builder(list(bstsMainContainer = main),
            list(controlPeriod = period, controlChartPlot = threshold,
                 probalisticControlPlot = probability), TRUE)
    expect_identical(main$bstsModelResults$object, fit)
    expect_identical(main$bstsModelSummaryTable, "existing model summary")
    list(plots = main$bstsControlPlots, called = called)
  }

  for (period in list(1, 0, -1, 2.5, 7, NA_real_, Inf, NULL, c(2, 3), "2")) {
    result <- run(period)
    expect_length(result$called, 0)
    for (key in c("bstsControlPlotThreshold", "bstsControlPlotProbability"))
      expect_identical(result$plots[[key]]$error,
        "Control period end must be a whole number of at least 2 and cannot exceed the number of time points (6).")
  }
  for (period in c(2, 6))
    expect_identical(run(period)$called, c("threshold", "probability"))

  threshold <- run(1, probability = FALSE)
  expect_false(is.null(threshold$plots$bstsControlPlotThreshold$error))
  expect_null(threshold$plots$bstsControlPlotProbability)
  probability <- run(1, threshold = FALSE)
  expect_false(is.null(probability$plots$bstsControlPlotProbability$error))
  expect_null(probability$plots$bstsControlPlotThreshold)
  hidden <- run(1, threshold = FALSE, probability = FALSE)
  expect_length(hidden$called, 0)
  expect_null(hidden$plots$bstsControlPlotThreshold)
  expect_null(hidden$plots$bstsControlPlotProbability)
})

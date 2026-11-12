# Exercise the real component builder with deterministic draws. In particular,
# bsts can give distinct model components the same name, "trend".
.bstsAxesFixture <- function(names = rep("trend", 3L), specification = TRUE) {
  centers <- rbind(7 + sin(seq_len(80L) / 10),
                   50 + seq_len(80L) / 10,
                   -30 + cos(seq_len(80L) / 10))
  offsets <- seq(-1, 1, length.out = 11L)
  state <- array(NA_real_, c(11L, 3L, 80L), dimnames = list(NULL, names, NULL))
  for (i in seq_len(3L))
    state[, i, ] <- outer(offsets * c(7, 2, .2)[i], centers[i, ], "+")
  fit <- list(niter = 11L, state.contributions = state)
  if (specification)
    fit$state.specification <- lapply(c("LocalLevel", "LocalLinearTrend", "SemilocalLinearTrend"),
                                     function(class) structure(list(), class = class))
  fit
}

.bstsAxesComponent <- function(fit, time) {
  builder <- getFromNamespace(".bstsComponentStatePlot", "jaspTimeSeries")
  scope <- new.env(parent = environment(builder))
  scope$createJaspPlot <- function(...) new.env(parent = emptyenv())
  environment(builder) <- scope
  output <- new.env(parent = emptyenv())
  builder(output, fit, list(burn = 1L, time = time), TRUE)
  output$bstsComponentStatePlot$plotObject
}

test_that("component plots retain every identically named trend component", {
  fit <- .bstsAxesFixture()
  original <- serialize(fit, NULL)
  plot <- .bstsAxesComponent(fit, 2000 + (seq_len(80L) - 1L) / 4)
  built <- ggplot2::ggplot_build(plot)
  expect_identical(as.character(built$layout$layout$component),
                   c("Local level", "Local linear trend", "Semi-local linear trend"))
  expect_equal(nrow(built$data[[1L]]), 240L)
  expect_equal(nrow(built$data[[2L]]), 240L)
  for (i in seq_len(3L)) {
    draws <- fit$state.contributions[-1L, i, ]
    line <- built$data[[2L]][as.integer(built$data[[2L]]$PANEL) == i, ]
    ribbon <- built$data[[1L]][as.integer(built$data[[1L]]$PANEL) == i, ]
    expect_equal(line$y, colMeans(draws))
    expect_equal(ribbon$ymin, apply(draws, 2, stats::quantile, probs = .025))
    expect_equal(ribbon$ymax, apply(draws, 2, stats::quantile, probs = .975))
    # Labelled ticks should bracket the entire ribbon, not just its center.
    ticks <- built$layout$panel_params[[i]]$y$get_breaks()
    ticks <- ticks[is.finite(ticks)]
    expect_lte(min(ticks), min(ribbon$ymin))
    expect_gte(max(ticks), max(ribbon$ymax))
  }
  expect_identical(serialize(fit, NULL), original)
})

test_that("duplicate or reserved component names cannot hide series", {
  for (names in list(rep("trend", 3L), c("time", "time 1", "trend"))) {
    fit <- .bstsAxesFixture(names, specification = FALSE)
    plot <- .bstsAxesComponent(fit, seq_len(80L))
    built <- ggplot2::ggplot_build(plot)
    expect_equal(nrow(built$layout$layout), 3L)
    expect_equal(nrow(built$data[[2L]]), 240L)
    for (i in seq_len(3L)) {
      line <- built$data[[2L]][as.integer(built$data[[2L]]$PANEL) == i, ]
      expect_equal(line$x, seq_len(80L))
      expect_equal(line$y, colMeans(fit$state.contributions[-1L, i, ]))
    }
  }
})

test_that("component endpoints remain labelled for small and constant ranges", {
  fit <- .bstsAxesFixture(c("level", "linear", "semi-local"))
  values <- list(seq(0, .7, length.out = 80L),
                 seq(-.1, .6, length.out = 80L), rep(0, 80L))
  for (i in seq_len(3L))
    fit$state.contributions[, i, ] <- rep(values[[i]], each = fit$niter)
  built <- ggplot2::ggplot_build(.bstsAxesComponent(fit, seq_len(80L)))
  for (i in seq_len(3L)) {
    panel <- built$layout$panel_params[[i]]
    ticks <- panel$y$get_breaks()
    ticks <- ticks[is.finite(ticks)]
    expect_lte(min(ticks), min(values[[i]]))
    expect_gte(max(ticks), max(values[[i]]))
    expect_true(all(diff(ticks) > diff(range(ticks)) * sqrt(.Machine$double.eps)))
    expect_gt(diff(panel$y.range), 0)
    expect_equal(built$data[[2L]]$y[as.integer(built$data[[2L]]$PANEL) == i], values[[i]])
  }
})

test_that("calendar component axes are capped without losing time values or conversion", {
  times <- list(as.Date("2024-01-01") + 0:79,
                as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + (0:79) * 3600)
  fit <- .bstsAxesFixture(c("level", "linear", "semi-local"))
  for (time in times) {
    plot <- .bstsAxesComponent(fit, time)
    built <- ggplot2::ggplot_build(plot)
    expect_identical(plot$guides$guides$x$params$cap, "both")
    expect_identical(plot$guides$guides$y$params$cap, "both")
    expect_equal(built$data[[2L]]$x, rep(as.numeric(time), 3L))
    expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
    converted <- withCallingHandlers(
      jaspGraphs::convertGgplotToPlotly(plot, returnJSON = FALSE),
      warning = function(warning) {
        if (identical(conditionMessage(warning), "Coercing LHS to a list"))
          invokeRestart("muffleWarning")
      })
    expect_s3_class(converted, "plotly")
    expect_gt(length(converted$x$data), 0L)
  }
})

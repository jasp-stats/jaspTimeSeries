# These deterministic draws exercise the seven real plot builders, without
# sampling or constructing native result objects. These checks do not exercise
# the native image editor.
.bstsStyleFixture <- function(components = 3L, missing = integer()) {
  centers <- rbind(
    level = c(0, .2, .1, .3, 3, 3.2, 3.5, 3.7),
    trend = c(10, 11, 10, 11, 20, 21, 22, 23),
    seasonal = c(-100, -90, -95, -92, -70, -60, -50, -40)
  )[seq_len(components), , drop = FALSE]
  offsets <- c(-.3, -.2, -.1, .1, .2, .3)
  state <- array(NA_real_, c(length(offsets), components, ncol(centers)),
    dimnames = list(NULL, rownames(centers), NULL))
  for (component in seq_len(components))
    state[, component, ] <- outer(offsets * component, centers[component, ], "+")
  response <- colSums(centers) + c(-.5, .1, .7, .2, -.2, .3, .6, -.1)
  response[missing] <- NA_real_
  structure(list(niter = length(offsets), family = "gaussian",
    original.series = stats::ts(response), state.contributions = state,
    one.step.prediction.errors = outer(seq_along(offsets) / 10, c(-2, -1, 1, 2, 3, 1, -1, .5), "*"),
    sigma.obs = rep(1, length(offsets)), has.regression = FALSE,
    timestamp.info = list(timestamps.are.trivial = TRUE, timestamps = seq_along(response))),
    class = "bsts")
}

.bstsStyleOptions <- function(time = seq_len(8L), ...) utils::modifyList(list(
  burn = 1L, time = time, time0 = "Time", aggregatedStateAndErrorPlotsCiLevel = .8,
  aggregatedStatesPlot = TRUE, aggregatedStatesPlotObservationsShown = TRUE,
  componentStatesPlot = TRUE, residualPlot = TRUE, forecastErrorPlot = TRUE,
  predictionHorizon = 3L, controlChartPeriodEnd = 3L, controlChartSigmaThreshold = 2
), list(...))

.bstsStyleForecast <- function(fit) list(original.series = as.numeric(fit$original.series),
  mean = c(4, 5, 6), interval = rbind(c(3, 3.5, 4), c(5, 6.5, 8)))

.bstsStyleKinds <- c("aggregated", "component", "prediction", "threshold", "probability", "residual", "forecastError")
.bstsStyleTitles <- c(aggregated = "Aggregated State", component = "Component States",
  prediction = "Prediction Plot", threshold = "Threshold Plot", probability = "Probability Plot",
  residual = "Residual Plot", forecastError = "Forecast Error Plot")
.bstsStyleYLabels <- c(aggregated = "State contribution", component = "State contribution",
  prediction = "Observed and predicted values", threshold = "State contribution",
  probability = "Probability", residual = "Residual", forecastError = "Forecast error")

.bstsStyleBuild <- function(kind, fit, options, translate = identity) {
  functions <- c(aggregated = ".bstsAggregatedStatePlot", component = ".bstsComponentStatePlot",
    prediction = ".bstsCreatePredictionPlot", threshold = ".bstsFillControlPlotThreshold",
    probability = ".bstsFillControlPlotProbability", residual = ".bstsResidualPlot",
    forecastError = ".bstsForecastErrorPlot")
  keys <- c(aggregated = "bstsAggregatedStatePlot", component = "bstsComponentStatePlot",
    prediction = "bstsPredictionPlot", threshold = "bstsControlPlotThreshold",
    probability = "bstsControlPlotProbability", residual = "bstsResidualPlot",
    forecastError = "bstsForecastErrorPlot")
  builder <- getFromNamespace(functions[[kind]], "jaspTimeSeries")
  scope <- new.env(parent = environment(builder))
  scope$createJaspPlot <- function(title = "", height = 320, width = 480, ...) {
    output <- new.env(parent = emptyenv())
    output$title <- title
    output$height <- height
    output$width <- width
    output$dependOn <- function(dependencies) output$dependencies <- dependencies
    output
  }
  scope$createJaspHtml <- function(text, position, ...) list(text = text, position = position)
  scope$gettext <- function(message, ...) translate(message)
  environment(builder) <- scope
  output <- new.env(parent = emptyenv())
  if (kind == "prediction") {
    output$bstsModelResults <- list(object = fit)
    output$bstsModelPredictions <- list(object = .bstsStyleForecast(fit))
    builder(list(bstsMainContainer = output), options, TRUE)
  } else if (kind == "aggregated") {
    builder(output, fit, NULL, options, TRUE)
  } else {
    builder(output, fit, options, TRUE)
  }
  record <- output[[keys[[kind]]]]
  expect_s3_class(record$plotObject, "ggplot")
  list(record = record, plot = record$plotObject, built = ggplot2::ggplot_build(record$plotObject),
       note = output[["bstsControlPlotThresholdNote"]])
}

.bstsStyleLayer <- function(actual, geom) {
  index <- which(vapply(actual$plot$layers, function(layer) inherits(layer$geom, geom), logical(1L)))
  expect_length(index, 1L)
  actual$built$data[[index]]
}

.bstsStyleCheck <- function(actual) {
  theme <- jaspGraphs::themeJaspRaw()
  expect_equal(actual$plot$theme$text$family, theme$text$family)
  expect_equal(actual$plot$theme$text$size, theme$text$size)
  expect_s3_class(actual$plot$theme$panel.grid, "element_blank")
  expect_equal(actual$plot$theme$panel.border, theme$panel.border)
  expect_identical(actual$plot$theme$legend.position, "none")
  frames <- vapply(actual$plot$layers, function(layer) inherits(layer$geom, "GeomRangeFrame"), logical(1L))
  expect_equal(sum(frames), 1L)
  line <- .bstsStyleLayer(actual, "GeomLine")
  expect_true(all(line$colour == "black"))
  expect_true(all(line$linewidth == .7))
  if (any(vapply(actual$plot$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1L)))) {
    ribbon <- .bstsStyleLayer(actual, "GeomRibbon")
    expect_true(all(ribbon$fill == "grey70"))
    expect_true(all(ribbon$alpha == .5))
  }
}

test_that("all BSTS plot styles preserve posterior summaries and forecast values", {
  fit <- .bstsStyleFixture()
  before <- serialize(fit, NULL)
  options <- .bstsStyleOptions()
  keep <- seq_len(fit$niter) > options$burn
  total <- apply(fit$state.contributions[keep, , , drop = FALSE], c(1, 3), sum)
  residuals <- bsts::residuals.bsts(fit, burn = options$burn)
  errors <- fit$one.step.prediction.errors[keep, , drop = FALSE]
  stateMean <- colMeans(total)
  limit <- mean(stateMean[1:3]) + 2 * stats::sd(stateMean[1:3])
  forecast <- .bstsStyleForecast(fit)
  componentSummary <- function(fun, ...) unlist(lapply(seq_len(3L), function(component)
    apply(fit$state.contributions[keep, component, , drop = FALSE], 3, fun, ...)), use.names = FALSE)
  means <- list(aggregated = stateMean, component = componentSummary(mean),
    prediction = c(as.numeric(fit$original.series), forecast$mean), threshold = stateMean,
    probability = colMeans(total > limit), residual = colMeans(residuals), forecastError = colMeans(errors))
  for (kind in .bstsStyleKinds) {
    actual <- .bstsStyleBuild(kind, fit, options)
    .bstsStyleCheck(actual)
    expect_identical(actual$record$title, .bstsStyleTitles[[kind]])
    expect_identical(actual$plot$labels$y, .bstsStyleYLabels[[kind]])
    expect_identical(actual$plot$labels$x, if (kind == "prediction") "Observation step" else "Time")
    line <- .bstsStyleLayer(actual, "GeomLine")
    expect_equal(line$y, unname(means[[kind]]), tolerance = 1e-12)
    expectedTime <- if (kind == "prediction") 1:11 else rep(1:8, if (kind == "component") 3L else 1L)
    expect_equal(line$x, expectedTime)
    if (kind != "probability") {
      ribbon <- .bstsStyleLayer(actual, "GeomRibbon")
      if (kind == "prediction") {
        lower <- c(rep(NA_real_, 8L), forecast$interval[1, ])
        upper <- c(rep(NA_real_, 8L), forecast$interval[2, ])
      } else if (kind == "component") {
        lower <- componentSummary(stats::quantile, probs = .025)
        upper <- componentSummary(stats::quantile, probs = .975)
      } else {
        draws <- switch(kind, residual = residuals, forecastError = errors, total)
        probabilities <- if (kind == "threshold") c(.025, .975) else c(.1, .9)
        lower <- apply(draws, 2, stats::quantile, probs = probabilities[1L])
        upper <- apply(draws, 2, stats::quantile, probs = probabilities[2L])
      }
      expect_equal(ribbon$ymin, unname(lower), tolerance = 1e-12)
      expect_equal(ribbon$ymax, unname(upper), tolerance = 1e-12)
    } else {
      expect_equal(actual$plot$scales$get_scales("y")$limits, c(0, 1))
    }
  }
  expect_identical(serialize(fit, NULL), before)
})

test_that("BSTS plot axes preserve numeric, Date and POSIXct observation times", {
  fit <- .bstsStyleFixture()
  times <- list(2000 + cumsum(c(.1, .3, .2, .5, .1, .4, .2, .3)),
    as.Date("2098-01-01") + c(0, 1, 3, 6, 10, 15, 21, 28),
    as.POSIXct("2098-01-01 00:00:00", tz = "UTC") + c(0, 1, 3, 6, 10, 15, 21, 28) * 3600)
  for (time in times) for (kind in setdiff(.bstsStyleKinds, "prediction")) {
    actual <- .bstsStyleBuild(kind, fit, .bstsStyleOptions(time))
    line <- .bstsStyleLayer(actual, "GeomLine")
    expect_equal(line$x, rep(as.numeric(time), if (kind == "component") 3L else 1L))
    if (inherits(time, "Date"))
      expect_s3_class(actual$built$layout$panel_scales_x[[1L]], "ScaleContinuousDate")
    if (inherits(time, "POSIXct"))
      expect_s3_class(actual$built$layout$panel_scales_x[[1L]], "ScaleContinuousDatetime")
  }
  unassigned <- .bstsStyleBuild("prediction", fit, .bstsStyleOptions(time0 = ""))
  expect_identical(unassigned$plot$labels$x, "Time")
})

test_that("BSTS plots retain interactive conversion with numeric and calendar axes", {
  # Rangeframes ask jaspGraphs for editable numeric axis metadata during
  # conversion. Calendar axes use capped guides to keep their native scales.
  times <- list(seq_len(8L), as.Date("2098-01-01") + seq_len(8L),
    as.POSIXct("2098-01-01 00:00:00", tz = "UTC") + seq_len(8L) * 3600)
  for (time in times) for (kind in .bstsStyleKinds) {
    actual <- .bstsStyleBuild(kind, .bstsStyleFixture(), .bstsStyleOptions(time))
    calendarAxis <- inherits(time, c("Date", "POSIXt")) && kind != "prediction"
    frames <- vapply(actual$plot$layers, function(layer)
      inherits(layer$geom, "GeomRangeFrame"), logical(1L))
    expect_equal(sum(frames), if (calendarAxis) 0L else 1L)
    if (calendarAxis) {
      expect_identical(actual$plot$theme$axis.line$colour, "black")
      expect_equal(actual$plot$theme$axis.line$linewidth, jaspGraphs::getGraphOption("axisTickWidth"))
    }
    converted <- withCallingHandlers(
      jaspGraphs::convertGgplotToPlotly(actual$plot, returnJSON = FALSE),
      warning = function(warning) {
        # Existing upstream ribbon conversion emits this coercion warning.
        if (identical(conditionMessage(warning), "Coercing LHS to a list"))
          invokeRestart("muffleWarning")
      })
    expect_s3_class(converted, "plotly")
    expect_s3_class(converted, "htmlwidget")
    expect_gt(length(converted$x$data), 0L)
    expect_true(any(vapply(converted$x$data, function(trace)
      length(trace$x) > 0L && length(trace$y) > 0L, logical(1L))))
  }
})

test_that("BSTS component panels retain independent y ranges and gain usable height", {
  single <- .bstsStyleBuild("component", .bstsStyleFixture(1L), .bstsStyleOptions())
  multiple <- .bstsStyleBuild("component", .bstsStyleFixture(3L), .bstsStyleOptions())
  expect_gt(multiple$record$height, single$record$height)
  expect_equal(multiple$record$width, single$record$width)
  expect_length(multiple$built$layout$panel_scales_y, 3L)
  expect_length(multiple$built$layout$panel_scales_x, 1L)
  ranges <- lapply(multiple$built$layout$panel_scales_y, function(scale) scale$range$range)
  expect_length(unique(ranges), 3L)
  expect_true(all(vapply(multiple$built$layout$panel_params, function(panel)
    identical(panel$x.range, multiple$built$layout$panel_params[[1L]]$x.range), logical(1L))))
  expect_false(identical(multiple$plot$theme$axis.text.x$angle, -90))
})

test_that("BSTS neutral bands preserve missing response gaps", {
  missing <- c(1L, 4L, 8L)
  fit <- .bstsStyleFixture(missing = missing)
  for (kind in c("aggregated", "residual", "prediction")) {
    actual <- .bstsStyleBuild(kind, fit, .bstsStyleOptions())
    if (kind == "aggregated") {
      points <- .bstsStyleLayer(actual, "GeomPoint")
      expect_identical(which(is.na(points$y)), missing)
      expect_false(anyNA(.bstsStyleLayer(actual, "GeomLine")$y))
    } else {
      line <- .bstsStyleLayer(actual, "GeomLine")
      expect_identical(which(is.na(line$y)), missing)
      if (kind == "residual") {
        ribbon <- .bstsStyleLayer(actual, "GeomRibbon")
        expect_identical(which(is.na(ribbon$ymin)), missing)
        expect_identical(which(is.na(ribbon$ymax)), missing)
      }
    }
  }
})

test_that("BSTS threshold notes report the actual crossing outside the plot", {
  fit <- .bstsStyleFixture()
  for (time in list(2000 + seq_len(8L), as.Date("2098-01-01") + seq_len(8L))) {
    actual <- .bstsStyleBuild("threshold", fit, .bstsStyleOptions(time))
    expect_null(actual$plot$labels$caption)
    text <- actual$note$text
    expect_match(text, "<i>Note.</i> The lower bound", fixed = TRUE)
    total <- apply(fit$state.contributions[-1L, , , drop = FALSE], c(1, 3), sum)
    average <- colMeans(total)
    threshold <- mean(average[1:3]) + 2 * stats::sd(average[1:3])
    first <- which(apply(total, 2, stats::quantile, probs = .025) > threshold)[1L]
    expect_false(is.na(first))
    expect_match(text, as.character(time[first]), fixed = TRUE)
    expect_match(text, "95% state credible interval", fixed = TRUE)
    expect_equal(actual$note$position, 1.1)
  }
})

test_that("BSTS plot titles and axis labels pass through translation", {
  translate <- function(text) paste0("translated: ", text)
  for (kind in .bstsStyleKinds) {
    actual <- .bstsStyleBuild(kind, .bstsStyleFixture(), .bstsStyleOptions(), translate)
    expect_identical(actual$record$title, translate(.bstsStyleTitles[[kind]]))
    expect_identical(actual$plot$labels$y, translate(.bstsStyleYLabels[[kind]]))
    expect_identical(actual$plot$labels$x,
      translate(if (kind == "prediction") "Observation step" else "Time"))
  }
})

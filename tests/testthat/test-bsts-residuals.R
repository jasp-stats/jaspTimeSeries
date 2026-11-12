# Deterministic state draws exercise the actual bsts residual method and plot
# builder without making the regression test depend on a sampled model.
.bstsResidualTestFit <- function(missing = integer()) {
  response <- c(4, 7, 3, 9, 2, 8, 1, 6)
  residuals <- outer(c(-2, -1, 0, 1, 3), seq_along(response), function(draw, step) draw + step / 10)
  state <- sweep(residuals, 2, response, "+")
  response[missing] <- NA_real_
  list(
    fit = structure(list(
      family = "gaussian", niter = nrow(residuals),
      original.series = stats::ts(response),
      state.contributions = array(state, dim = c(nrow(state), 1L, ncol(state))),
      timestamp.info = list(timestamps.are.trivial = TRUE)
    ), class = "bsts"),
    residuals = residuals
  )
}

.bstsResidualTestPlot <- function(fit, burn, ci = .95) {
  builder <- getFromNamespace(".bstsResidualPlot", "jaspTimeSeries")
  scope <- new.env(parent = environment(builder))
  # A plain R collector, never a manually constructed native result object.
  scope$createJaspPlot <- function(...) new.env(parent = emptyenv())
  environment(builder) <- scope
  plots <- new.env(parent = emptyenv())
  time <- seq(2001, by = .25, length.out = length(fit$original.series))
  builder(plots, fit, list(residualPlot = TRUE, burn = burn, time = time,
                          aggregatedStateAndErrorPlotsCiLevel = ci), ready = TRUE)
  list(plot = plots$bstsResidualPlot$plotObject, time = time)
}

test_that("BSTS residual intervals preserve internal and endpoint missing observations", {
  for (missing in list(c(3L, 6L), c(1L, 8L), c(1L, 4L, 8L))) {
    fixture <- .bstsResidualTestFit(missing)
    before <- serialize(fixture$fit, NULL)
    observed <- setdiff(seq_len(8L), missing)
    for (burn in c(0L, 2L, 4L)) {
      actual <- .bstsResidualTestPlot(fixture$fit, burn)
      built <- ggplot2::ggplot_build(actual$plot)
      ribbon <- built$data[[1L]]
      line <- built$data[[2L]]
      expect_equal(line$x, actual$time)
      expect_equal(ribbon$x, actual$time)
      expect_identical(which(is.na(line$y)), missing)
      expect_identical(which(is.na(ribbon$ymin)), missing)
      expect_identical(which(is.na(ribbon$ymax)), missing)
      # Missing time columns stay present. Observed summaries are the same
      # posterior summaries as before, including when just one draw remains.
      retained <- fixture$residuals[seq_len(5L) > burn, observed, drop = FALSE]
      expect_equal(line$y[observed], colMeans(retained), tolerance = 1e-12)
      expect_equal(ribbon$ymin[observed], unname(apply(retained, 2, stats::quantile, probs = .025)), tolerance = 1e-12)
      expect_equal(ribbon$ymax[observed], unname(apply(retained, 2, stats::quantile, probs = .975)), tolerance = 1e-12)
      expect_identical(serialize(fixture$fit, NULL), before)
    }
  }
})

test_that("BSTS complete-data residual summaries retain their values and interval level", {
  fixture <- .bstsResidualTestFit()
  for (ci in c(.8, .95)) {
    for (burn in c(0L, 2L, 4L)) {
      actual <- .bstsResidualTestPlot(fixture$fit, burn, ci)
      built <- ggplot2::ggplot_build(actual$plot)
      retained <- fixture$residuals[seq_len(5L) > burn, , drop = FALSE]
      expect_equal(built$data[[2L]]$x, actual$time)
      expect_equal(built$data[[2L]]$y, colMeans(retained), tolerance = 1e-12)
      expect_equal(built$data[[1L]]$ymin, unname(apply(retained, 2, stats::quantile, probs = (1 - ci) / 2)), tolerance = 1e-12)
      expect_equal(built$data[[1L]]$ymax, unname(apply(retained, 2, stats::quantile, probs = 1 - (1 - ci) / 2)), tolerance = 1e-12)
    }
  }
})

test_that("ARIMA residuals align to the original series across fitting modes and frequencies", {
  residualsForRows <- getFromNamespace(".tsResiduals", "jaspTimeSeries")
  k <- 1:80
  y <- 10 + .6 * sin(k / 5) - .3 * cos(k / 9) + sin(k / 3) + ((k * 7) %% 11) / 20
  for (frequency in c(1L, 12L)) for (leading in c(0L, 2L, 14L)) {
    values <- y
    values[c(seq_len(leading), 37L, 79L, 80L)] <- NA_real_
    input <- stats::ts(values, start = c(2001L, if (frequency == 12L) 3L else 1L), frequency = frequency)
    fits <- list(
      auto = forecast::auto.arima(input, seasonal = frequency > 1L),
      custom = forecast::Arima(input, order = c(1L, 0L, 0L))
    )
    for (mode in names(fits)) {
      fit <- fits[[mode]]
      before <- serialize(fit, NULL)
      actual <- residualsForRows(fit)
      # Ordinary subtraction intersects ts ranges. Taking their union first
      # provides an independent oracle that retains the leading missing rows.
      aligned <- stats::ts.union(observed = fit$x, fitted = stats::fitted(fit))
      expected <- as.numeric(aligned[, "observed"] - aligned[, "fitted"])
      info <- paste(mode, "frequency", frequency, "leading missing", leading)
      expect_type(actual, "double")
      expect_length(actual, length(input))
      expect_equal(actual, expected, tolerance = 1e-10, info = info)
      expect_identical(which(is.na(actual)), which(is.na(values)), info = info)
      if (mode == "custom" || leading == 0L)
        expect_equal(actual, as.numeric(stats::residuals(fit)), tolerance = 1e-10, info = info)
      else
        expect_equal(actual[-seq_len(leading)], as.numeric(stats::residuals(fit)), tolerance = 1e-10, info = info)
      expect_identical(serialize(fit, NULL), before, info = info)
    }
  }
})

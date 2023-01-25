#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

DescriptivesTimeSeries <- function(jaspResults, dataset, options) {
    ready <- options$dependent != ""

    if (ready)
      dataset <- .tsReadDataDescriptives(jaspResults, dataset, options)

    .tsTimeSeriesPlotDescriptives(jaspResults, dataset, options, ready, position = 1, dependencies = c("timeSeriesPlot", "timeSeriesPlotType", "dependent", "timeSeriesPlotDistribution"))

    .tsStateSpacePlotDescriptives(jaspResults, dataset, options, ready, position = 2, dependencies  = c("stateSpacePlot", "stateSpacePlotLag", "stateSpacePlotRegressionType", "stateSpacePlotRegressionLine", "stateSpacePlotRegressionCi", "stateSpacePlotRegressionCiLevel", "dependent"))

    .tsACFDescriptives(jaspResults, dataset, options, ready, position = 3, dependencies = c("dependent", "acf", "acfCi", "acfCiLevel", "acfCiType", "acfMaxLag"))

    .tsPACFDescriptives(jaspResults, dataset, options, ready, position = 4, dependencies = c("dependent", "pacf", "pacfCi", "pacfCiLevel", "pacfCiType", "pacfMaxLag"))

    .tsPowerSpectralDensityDescriptives(jaspResults, dataset, options, ready, position = 5, dependencies = c("powerSpectralDensity", "powerSpectralDensityDetrend", "powerSpectralDensityDemean", "powerSpectralDensitySmoother", "powerSpectralDensitySmootherKernel", "powerSpectralDensitySmootherKernelM1", "powerSpectralDensitySmootherKernelM2", "powerSpectralDensityTaper", "powerSpectralDensityScaling", "noScaling", "log", "log10", "dependent"))
}

.tsReadDataDescriptives <- function(jaspResults, dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    dataset <- .readDataSetToEnd(columns.as.numeric = options$dependent)
    yName <- options$dependent[1]
    y     <- dataset[, yName]
    t     <- 1:nrow(dataset)

    dat <- data.frame(y, t)
    return(dat)
  }
}

.tsTimeSeriesPlotDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$timeSeriesPlot)
    return()

  if (is.null(jaspResults[["timeSeriesPlot"]])) {
    plot <- createJaspPlot(title = "Time Series Plot", width = 660)
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["timeSeriesPlot"]] <- plot

    if (!ready)
      return()

    .tsFillTimeSeriesPlot(plot, dataset, options, type = options$timeSeriesPlotType, distribution = options$timeSeriesPlotDistribution)

  }
}

.tsFillTimeSeriesPlot <- function(timeSeriesPlot, dataset, options, type, distribution, yName = NULL) {
  if (is.null(yName)) yName <- options$dependent[1]

  dat <- dataset

  p <- jaspGraphs::JASPScatterPlot(dat$t, dat$y,
    yName = yName, xName = "t",
    addSmooth = FALSE, plotAbove = "none",
    plotRight = distribution
  )

  if (type != "points")  p$subplots$mainPlot$layers[[1]] <- jaspGraphs::geom_line()
  if (type == "both")    p$subplots$mainPlot <- p$subplots$mainPlot + jaspGraphs::geom_point()

  timeSeriesPlot$plotObject <- p
}

.tsStateSpacePlotDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$stateSpacePlot)
    return()

  if (is.null(jaspResults[["stateSpacePlot"]])) {
    plot <- createJaspPlot(title = "State Space Plot")
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["stateSpacePlot"]] <- plot

    if (!ready)
      return()

    .tsFillStateSpacePlot(plot, dataset, options)
  }
}

.tsFillStateSpacePlot <- function(stateSpacePlot, dataset, options) {
  yLag  <- c(rep(NA, options$stateSpacePlotLag), dataset$y[1:(length(dataset$y) - options$stateSpacePlotLag)])

  yName <- decodeColNames(options$dependent[1])
  xName <- as.expression(bquote(.(yName)[t-.(options$stateSpacePlotLag)]))
  yName <- as.expression(bquote(.(yName)[t]))

  dat <- data.frame(y = dataset$y, yLag)
  dat <- na.omit(dat)

  # forceLinearSmooth <- options$stateSpacePlotRegressionType == "linear"
  # Does not work with bquote
  p <- jaspGraphs::JASPScatterPlot(
    dat$yLag, dat$y,
    xName = xName, 
    yName = yName,
    addSmooth = options$stateSpacePlotRegressionLine,
    addSmoothCI = options$stateSpacePlotRegressionCi,
    smoothCIValue = options$stateSpacePlotRegressionCiLevel,
    forceLinearSmooth = options$stateSpacePlotRegressionType == "linear",
    plotAbove = "none", plotRight = "none"
  )

  stateSpacePlot$plotObject <- p

  return()
}

.tsACFDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies){
  if (!options$acf)
    return()

  if (is.null(jaspResults[["acfPlot"]])) {
    plot <- createJaspPlot(title = "Autocorrelation Function")
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["acfPlot"]] <- plot

    if (!ready) {
      return()
    }

    .tsFillACF(plot,
      type = "ACF", dataset, options,
      ci = options$acfCi,
      ciValue = options$acfCiLevel,
      ciType = options$acfCiType
    )
  }
}

.tsPACFDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$acf)
    return()

  if (is.null(jaspResults[["pacfPlot"]])) {
    plot <- createJaspPlot(title = "Partial Autocorrelation Function")
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["pacfPlot"]] <- plot

    if (!ready) {
      return()
    }

    .tsFillACF(plot,
      type = "PACF", dataset, options, ci = options$pacfCi,
      ciValue = options$pacfCiLevel,
      ciType = options$pacfCiType
    )
  }
}

.tsBartlettSE <- function(r, N, ci = 0.95) {
  z <- qnorm((1 + ci) / 2)

  lag <- length(r)
  df  <- data.frame(r = r, se = numeric(lag))
  for(i in 1:lag) {
    df$se[i] <- z * sqrt((1 / N) * (1 + 2 * sum(r[1:i] ^ 2)))
  }

  return(df)
}

.tsFillACF <- function(plot, type, dataset, options, ci, ciValue, ciType) {
  y <- na.omit(dataset$y)
  lag <- options$acfMax
  if (type == "ACF") {
    ac <- stats::acf(y, plot = FALSE, lag.max = options$acfMax)
    dat <- data.frame(acf = ac$acf[-1], lag = ac$lag[-1]) # remove lag 0
  }
  if (type == "PACF") {
    ac <- stats::pacf(y, plot = FALSE, lag.max = options$pacfMax)
    ciType <- "normal"
    dat <- data.frame(acf = ac$acf, lag = ac$lag)
  }

  yRange <- dat$acf
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$lag)
  xBreaks <- xBreaks[!xBreaks %% 1] # keep only integers
  xMin <- min(xBreaks)
  xMax <- max(xBreaks)

  p <- ggplot2::ggplot()
  if (ci) {
    if (ciType == "normal") {
      clim <- qnorm((1 + ciValue) / 2) / sqrt(ac$n.used)
      dat$upper <- rep(clim, nrow(dat))
      dat$lower <- -dat$upper
    } else {
      clim <- .tsBartlettSE(dat$acf, ac$n.used, ciValue)
      dat$upper <- clim$se
      dat$lower <- -dat$upper
    }

    yRange    <- c(yRange, dat$upper, dat$lower)

    p <- p +
      ggplot2::geom_ribbon(
        data = dat,
        ggplot2::aes(x = lag, ymin = lower, ymax = upper), alpha = 0.15
      )
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)

  p <- p +
    ggplot2::scale_x_continuous(name = "Lag", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = type, breaks = yBreaks, limits = range(yBreaks))

  p <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = acf), size = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = xMin, xend = xMax, y = 0, yend = 0), alpha = 0.2) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
  return()
}

.tsPowerSpectralDensityDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies){
  if (!options$powerSpectralDensity)
    return()

  if (is.null(jaspResults[["powerSpectralDensity"]])) {
    plot <- createJaspPlot(title = "Power Spectral Density Plot")
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["powerSpectralDensity"]] <- plot

    if (!ready)
      return()

    .tsFillPowerSpectralDensity(plot, dataset, options)
  }
}

.tsFillPowerSpectralDensity <- function(powerSpectralDensity, dataset, options) {
  y <- na.omit(dataset$y)

  k <- NULL

  # crashes when modified daniell has zeros..
  if (options$powerSpectralDensitySmoother)
    k <- stats::kernel(options$powerSpectralDensitySmootherKernel, c(options$powerSpectralDensitySmootherKernelM1, options$powerSpectralDensitySmootherKernelM2))

  yPSD <- stats::spec.pgram(y,
    kernel = k,
    taper = options$powerSpectralDensityTaper,
    demean = options$powerSpectralDensityDemean,
    detrend = options$powerSpectralDensityDetrend,
    plot = FALSE
  )

  dat <- data.frame(x = yPSD$freq, y = yPSD$spec)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$y)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) + jaspGraphs::geom_line() +
    ggplot2::scale_x_continuous(name = "Frequency", breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Spectrum", breaks = yBreaks)

  if (options$powerSpectralDensityScaling != "noScaling") {
    logTrans <- options$powerSpectralDensityScaling
    logFunction <- function(x) exp(x)
    logLabels <- scales::math_format(e ^ .x)
    if (logTrans == "log10") {
      logFunction <- function(x) 10 ^ x
      logLabels <- scales::math_format(10 ^ .x)
    }

    p <- p + ggplot2::scale_y_continuous(
      trans = logTrans,
      breaks = scales::trans_breaks(logTrans, logFunction),
      labels = scales::trans_format(logTrans, logLabels)
    )
  }

  p <- p +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  powerSpectralDensity$plotObject <- p

  return()
}
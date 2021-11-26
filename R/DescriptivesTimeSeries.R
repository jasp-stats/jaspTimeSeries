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
    # ready <- (length(options$dependentVariable) > 0)
    ready <- options$dependentVariable != ""

    if (ready)
      dataset <- .tsReadData(jaspResults, dataset, options)

    .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1)
    
    .tsStateSpacePlot(jaspResults, dataset, options, ready, position = 2)

    .tsACF(jaspResults, dataset, options, ready, position = 3)

    .tsPowerSpectralDensity(jaspResults, dataset, options, ready, position = 4)
}

.tsReadData <- function(jaspResults, dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = options$dependentVariable))
}

.tsTimeSeriesPlot <- function(jaspResults, dataset, options, ready, position) {
  if (!options$timeSeriesPlot)
    return()
  
  if (is.null(jaspResults[["timeSeriesPlot"]])) {
    plot <- createJaspPlot(title = "Time Series Plot", width = 480)
    plot$dependOn(c("timeSeriesPlot", "tsType", "dependentVariable"))
    plot$position <- position

    jaspResults[["timeSeriesPlot"]] <- plot

    if (!ready)
      return()

    .tsFillTimeSeriesPlot(plot, dataset, options, type = options$tsType)
  }
}

.tsFillTimeSeriesPlot <- function(timeSeriesPlot, dataset, options, type) {
  yName <- options$dependentVariable[1]
  y     <- dataset[, yName]
  t     <- 1:nrow(dataset)

  dat <- data.frame(y, t)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(1, t))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = t, y = y)) +
    ggplot2::scale_x_continuous(name = gettext("t"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
  if (type != "points") p <- p + jaspGraphs::geom_line()
  if (type != "line") p <- p + jaspGraphs::geom_point()
  p <- jaspGraphs::themeJasp(p)

  timeSeriesPlot$plotObject <- p
}

.tsStateSpacePlot <- function(jaspResults, dataset, options, ready, position) {
  if (!options$stateSpacePlot)
    return()

  if (is.null(jaspResults[["stateSpacePlot"]])) {
    plot <- createJaspPlot(title = "State Space Plot")
    plot$dependOn(c("stateSpacePlot", "lag", "regressionType", "addSmooth", "addSmoothCI", "addSmoothCIValue", "dependentVariable"))
    plot$position <- position

    jaspResults[["stateSpacePlot"]] <- plot

    if (!ready)
      return()

    .tsFillStateSpacePlot(plot, dataset, options)
  }
}

.tsFillStateSpacePlot <- function(stateSpacePlot, dataset, options) {
  yName <- options$dependentVariable[1]
  y     <- dataset[, yName]
  yLag  <- c(rep(NA, options$lag), y[1:(length(y) - options$lag)])

  yName <- decodeColNames(yName)
  xName <- as.expression(bquote(.(yName)[t-.(options$lag)]))
  yName <- as.expression(bquote(.(yName)[t]))

  dat <- data.frame(y, yLag)
  dat <- na.omit(dat)

  # xBreaks <- jaspGraphs::getPrettyAxisBreaks(yLag)
  # yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  # forceLinearSmooth <- options$regressionType == "linear"
  # Does not work with bquote
  p <- jaspGraphs::JASPScatterPlot(dat$yLag, dat$y,
                                   xName = xName, 
                                   yName = yName,
                                   addSmooth = options$addSmooth,
                                   addSmoothCI = options$addSmoothCI,
                                   smoothCIValue = options$addSmoothCIValue,
                                   forceLinearSmooth = options$regressionType == "linear",
                                   plotAbove = "none", plotRight = "none"
                                   )

  # p <- ggplot2::ggplot(dat, ggplot2::aes(x = yLag, y = y)) + jaspGraphs::geom_point() +
  #   ggplot2::scale_x_continuous(name = bquote(.(decodeColNames(yName))[t-1]), 
  #                               breaks = xBreaks, limits = range(xBreaks)) +
  #   ggplot2::scale_y_continuous(name = bquote(.(decodeColNames(yName))[t]), 
  #                               breaks = yBreaks, limits = range(yBreaks))

  # if (options$addSmooth) {
  #   p <- p + ggplot2::geom_smooth(se = options$addSmoothCI, level = options$addSmoothCIValue,
  #                                 method = if (forceLinearSmooth) "lm" else "loess")
  # }
  # p <- jaspGraphs::themeJasp(p)

  stateSpacePlot$plotObject <- p
  
  return()
}

.tsACF <- function(jaspResults, dataset, options, ready, position){
  if (!options$acfPlot)
    return()

  if (is.null(jaspResults[["acfPlot"]])) {
    plot <- createJaspPlot(title = "Autocorrelation Function Plot")
    plot$dependOn(c("acfPlot", "addLinesCI", "addLinesCIValue", "dependentVariable"))
    plot$position <- position

    jaspResults[["acfPlot"]] <- plot

    if (!ready)
      return()

    .tsFillACF(plot, dataset, options)
  }
}

.tsFillACF <- function(acfPlot, dataset, options) {
  y <- dataset[, options$dependentVariable[1]]
  y <- na.omit(y)

  yACF <- acf(y, plot = F)

  dat <- data.frame(ACF = yACF$acf, Lag = yACF$lag)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$Lag)
  yRange  <- dat$ACF

  p <- ggplot2::ggplot()
  if (options$addLinesCI) {
    ci        <- options$addLinesCIValue
    clim      <- qnorm((1 + ci) / 2) / sqrt(yACF$n.used)
    yRange    <- c(yRange, clim, -clim)
    dfSegment <- data.frame(x = min(xBreaks), xend = max(xBreaks), y = c(clim, -clim))

    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = "dashed", color = "blue", data = dfSegment)
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)

  p <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = Lag, ymin = 0, ymax = ACF)) +
    ggplot2::scale_x_continuous(name = "Lag", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = "ACF", breaks = yBreaks, limits = range(yBreaks))

  p <- jaspGraphs::themeJasp(p)

  acfPlot$plotObject <- p
  
  return()
}

.tsPowerSpectralDensity <- function(jaspResults, dataset, options, ready, position){
  if (!options$powerSpectralDensity)
    return()

  if (is.null(jaspResults[["powerSpectralDensity"]])) {
    plot <- createJaspPlot(title = "Power Spectral Density Plot")
    plot$dependOn(c("powerSpectralDensity",
                    "detrend", "demean", 
                    "smoothing", "kernel", "m1", "m2", 
                    "taper",
                    "scaling", "noScaling", "log", "log10",
                    "dependentVariable"))
    plot$position <- position

    jaspResults[["powerSpectralDensity"]] <- plot

    if (!ready)
      return()

    .tsFillPowerSpectralDensity(plot, dataset, options)
  }
}

.tsFillPowerSpectralDensity <- function(powerSpectralDensity, dataset, options) {
  y <- dataset[, options$dependentVariable[1]]
  y <- na.omit(y)

  k <- NULL

  # crashes when modified daniell has zeros..
  if (options$smoothing)
    k <- kernel(options$kernel, c(options$m1, options$m2))
  
  yPSD <- spec.pgram(y, 
                     kernel = k,
                     taper = options$taper,
                     demean = options$demean,
                     detrend = options$detrend,
                     plot = F)

  dat <- data.frame(x = yPSD$freq, y = yPSD$spec)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$y)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) + jaspGraphs::geom_line() +
    ggplot2::scale_x_continuous(name = "Frequency", breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Spectrum", breaks = yBreaks)

  if (options$scaling != "noScaling") {
    logTrans <- options$scaling
    logFunction <- function(x) exp(x)
    logLabels <- scales::math_format(e ^ .x)
    if (logTrans == "log10") {
      logFunction <- function(x) 10 ^ x
      logLabels <- scales::math_format(10 ^ .x)
    }

    p <- p + ggplot2::scale_y_continuous(trans = logTrans,
                                         breaks = scales::trans_breaks(logTrans, logFunction),
                                         labels = scales::trans_format(logTrans, logLabels))
  }
  
  # if(options$scaling == "log10")
  #   p <- p + scale_y_continuous(trans = "log10",
  #                               breaks = trans_breaks("log10", function(x) 10 ^ x),
  #                               labels = trans_format("log10", math_format(10 ^ .x)))

  p <- jaspGraphs::themeJasp(p)

  powerSpectralDensity$plotObject <- p
  
  return()
}
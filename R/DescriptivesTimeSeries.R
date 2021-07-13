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

# When making changes to this file always mention @sophieberkhout as a 
# reviewer in the Pull Request

DescriptivesTimeSeries <- function(jaspResults, dataset, options) {
    ready <- (length(options$dependentVariable) > 0)

    if (ready)
      dataset <- .tsReadData(jaspResults, dataset, options)

    if (options$timeSeriesPlot)
      .tsTimeSeriesPlot(jaspResults, dataset, options, ready)
    
    if(options$stateSpacePlot)
      .tsStateSpacePlot(jaspResults, dataset, options, ready)

    if(options$acf)
      .tsACF(jaspResults, dataset, options, ready)
}

.tsReadData <- function(jaspResults, dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = options$dependentVariable))
}

.tsTimeSeriesPlot <- function(jaspResults, dataset, options, ready) {
  timeSeriesPlot <- createJaspPlot(title = "Time Series Plot", width = 480)
  timeSeriesPlot$dependOn(c("dependentVariable", "timeSeriesPlot"))

  jaspResults[["timeSeriesPlot"]] <- timeSeriesPlot

  if (!ready)
    return()

  .tsFillTimeSeriesPlot(timeSeriesPlot, dataset, options)

  return()
}

.tsFillTimeSeriesPlot <- function(timeSeriesPlot, dataset, options) {
  yName <-  options$dependentVariable[1]
  y     <- dataset[, yName]
  t     <- 1:nrow(dataset)

  dat <- data.frame(y, t)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(1, t))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  plot <- ggplot2::ggplot(dat, ggplot2::aes(x = t, y = y)) + ggplot2::geom_line() +
    ggplot2::scale_x_continuous(name = gettext("t"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
  plot <- jaspGraphs::themeJasp(plot)

  timeSeriesPlot$plotObject <- plot

  return()
}

.tsStateSpacePlot <- function(jaspResults, dataset, options, ready) {
  stateSpacePlot <- createJaspPlot(title = "State Space Plot")
  stateSpacePlot$dependOn(c("dependentVariable", "stateSpacePlot"))

  jaspResults[["stateSpacePlot"]] <- stateSpacePlot

  if (!ready)
    return()

  .tsFillStateSpacePlot(stateSpacePlot, dataset, options)

  return()
}

.tsFillStateSpacePlot <- function(stateSpacePlot, dataset, options) {
  yName <- options$dependentVariable[1]
  y     <- dataset[, yName]
  yLag  <- c(NA, y[-length(y)])

  dat <- data.frame(y, yLag)
  dat <- na.omit(dat)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(yLag)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  forceLinearSmooth <- options$regressionType == "linear"
  # Does not work with bquote
  # plot <- jaspGraphs::JASPScatterPlot(dat$yLag, dat$y,
  #                                     xName = bquote(.(decodeColNames(yName))[t-1]), 
  #                                     yName = bquote(.(decodeColNames(yName))[t]),
  #                                     plotAbove = "none", plotRight = "none"
  #                                     )

  plot <- ggplot2::ggplot(dat, ggplot2::aes(x = yLag, y = y)) + jaspGraphs::geom_point() +
    ggplot2::scale_x_continuous(name = bquote(.(decodeColNames(yName))[t-1]), 
                                breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = bquote(.(decodeColNames(yName))[t]), 
                                breaks = yBreaks, limits = range(yBreaks))

  if(options$addSmooth){
    plot <- plot + ggplot2::geom_smooth(se = options$addSmoothCI, level = options$addSmoothCIValue,
                                        method = if (forceLinearSmooth) "lm" else "loess")
  }
  plot <- jaspGraphs::themeJasp(plot)

  stateSpacePlot$plotObject <- plot
  
  return()
}

.tsACF <- function(jaspResults, dataset, options, ready){
  acfPlot <- createJaspPlot(title = "Autocorrelation Function")
  acfPlot$dependOn(c("dependentVariable", "acf"))

  jaspResults[["acfPlot"]] <- acfPlot

  if (!ready)
    return()

  .tsFillACF(acfPlot, dataset, options)

  return()
}

.tsFillACF <- function(acfPlot, dataset, options) {
  y <- dataset[, options$dependentVariable[1]]
  y <- na.omit(y)

  yACF <- acf(y, plot = F)

  dat <- data.frame(ACF = yACF$acf, Lag = yACF$lag)

  yRange <- dat$ACF

  plot <- ggplot2::ggplot()
  if(options$addLinesCI){
    ci        <- options$addLinesCIValue
    clim      <- qnorm((1 + ci)/2)/sqrt(yACF$n.used)
    yRange    <- c(yRange, clim, -clim)
    dfSegment <- data.frame(x = min(xBreaks), xend = max(xBreaks), y = c(clim, -clim))

    plot <- plot +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = "dashed", color = "blue", data = dfSegment)
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$Lag)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)

  plot <- plot +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = Lag, ymin = 0, ymax = ACF)) +
    ggplot2::scale_x_continuous(name = "Lag", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = "ACF", breaks = yBreaks, limits = range(yBreaks))

  plot <- jaspGraphs::themeJasp(plot)

  acfPlot$plotObject <- plot
  
  return()
}
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
    ready <- (length(options$dependentVariable) > 0)

    if (ready)
      dataset <- .tsReadData(jaspResults, dataset, options)

    .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1)
    
    .tsStateSpacePlot(jaspResults, dataset, options, ready, position = 2)

    .tsACF(jaspResults, dataset, options, ready, position = 3)
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
    plot$dependOn(c("timeSeriesPlot"))
    plot$position <- position

    jaspResults[["timeSeriesPlot"]] <- plot

    if (!ready)
      return()

    .tsFillTimeSeriesPlot(plot, dataset, options)
  }
}

.tsFillTimeSeriesPlot <- function(timeSeriesPlot, dataset, options) {
  yName <- options$dependentVariable[1]
  y     <- dataset[, yName]
  t     <- 1:nrow(dataset)

  dat <- data.frame(y, t)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(1, t))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = t, y = y)) + ggplot2::geom_line() +
    ggplot2::scale_x_continuous(name = gettext("t"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
  p <- jaspGraphs::themeJasp(p)

  timeSeriesPlot$plotObject <- p
}

.tsStateSpacePlot <- function(jaspResults, dataset, options, ready, position) {
  if (!options$stateSpacePlot)
    return()

  if (is.null(jaspResults[["stateSpacePlot"]])) {
    plot <- createJaspPlot(title = "State Space Plot")
    plot$dependOn(c("stateSpacePlot", "addSmooth", "addSmoothCI", "addSmoothCIValue"))
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
  yLag  <- c(NA, y[-length(y)])

  dat <- data.frame(y, yLag)
  dat <- na.omit(dat)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(yLag)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  forceLinearSmooth <- options$regressionType == "linear"
  # Does not work with bquote
  # p <- jaspGraphs::JASPScatterPlot(dat$yLag, dat$y,
  #                                  xName = bquote(.(decodeColNames(yName))[t-1]), 
  #                                  Name = bquote(.(decodeColNames(yName))[t]),
  #                                  plotAbove = "none", plotRight = "none"
  #                                  )

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = yLag, y = y)) + jaspGraphs::geom_point() +
    ggplot2::scale_x_continuous(name = bquote(.(decodeColNames(yName))[t-1]), 
                                breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = bquote(.(decodeColNames(yName))[t]), 
                                breaks = yBreaks, limits = range(yBreaks))

  if (options$addSmooth) {
    p <- p + ggplot2::geom_smooth(se = options$addSmoothCI, level = options$addSmoothCIValue,
                                  method = if (forceLinearSmooth) "lm" else "loess")
  }
  p <- jaspGraphs::themeJasp(p)

  stateSpacePlot$plotObject <- p
  
  return()
}

.tsACF <- function(jaspResults, dataset, options, ready, position){
  if (!options$acfPlot)
    return()

  if (is.null(jaspResults[["acfPlot"]])) {
    plot <- createJaspPlot(title = "Autocorrelation Function Plot")
    plot$dependOn(c("acfPlot", "addLinesCI", "addLinesCIValue"))
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
    clim      <- qnorm((1 + ci)/2)/sqrt(yACF$n.used)
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
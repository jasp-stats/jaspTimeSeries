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
      dataset <- .tsReadDataDescriptives(jaspResults, dataset, options)

    .tsTimeSeriesPlotDescriptives(jaspResults, dataset, options, ready, position = 1, dependencies = c("timeSeriesPlot", "tsType", "dependentVariable", "distribution"))
    
    .tsStateSpacePlotDescriptives(jaspResults, dataset, options, ready, position = 2, dependencies  = c("stateSpacePlot", "lag", "regressionType", "addSmooth", "addSmoothCI", "addSmoothCIValue", "dependentVariable"))

    .tsACFDescriptives(jaspResults, dataset, options, ready, position = 3, dependencies = c("acfPlots", "acfCI", "acfCIValue", "acfMax", "dependentVariable"))

    .tsPowerSpectralDensityDescriptives(jaspResults, dataset, options, ready, position = 4, dependencies = c("powerSpectralDensity", "detrend", "demean", "smoothing", "kernel", "m1", "m2", "taper", "scaling", "noScaling", "log", "log10", "dependentVariable"))
}

.tsReadDataDescriptives <- function(jaspResults, dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    dataset <- .readDataSetToEnd(columns.as.numeric = options$dependentVariable)
    yName <- options$dependentVariable[1]
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

    .tsFillTimeSeriesPlot(plot, dataset, options, type = options$tsType, distribution = options$distribution)
  }
}

.tsFillTimeSeriesPlot <- function(timeSeriesPlot, dataset, options, type, distribution, yName = NULL) {
  if (is.null(yName)) yName <- options$dependentVariable[1]
  # y     <- dataset[, yName]
  # t     <- 1:nrow(dataset)

  # dat <- data.frame(y, t)
  dat <- dataset

  # xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(1, dat$t))
  # yBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$y)

  # p <- ggplot2::ggplot(dat, ggplot2::aes(x = t, y = y)) +
  #   ggplot2::scale_x_continuous(name = gettext("t"), breaks = xBreaks, limits = range(xBreaks)) +
  #   ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
  # if (type != "points") p <- p + jaspGraphs::geom_line()
  # if (type != "line") p <- p + jaspGraphs::geom_point()
  # p <- jaspGraphs::themeJasp(p)

  p <- jaspGraphs::JASPScatterPlot(dat$t, dat$y, yName = yName, xName = "t",
                                   addSmooth = F, plotAbove = "none", plotRight = distribution)
  if (type != "points")  p$subplots$mainPlot$layers[[1]] <- jaspGraphs::geom_line()
  if (type == "both")    p$subplots$mainPlot <- p$subplots$mainPlot + jaspGraphs::geom_point()


  # if (forecast)
  # f <- forecast::forecast(fit)
  # f <- as.data.frame(f)
  # names(f) <- c("y", "lb80", "ub80", "lb95", "ub95")
  # f$t <- as.numeric(row.names(f))
  # obs <- data.frame(t = dat$t, y = dat$y)
  # fcs <- data.frame(t = f$t, y = f$y)
  # new <- rbind(obs, fc)
  # cols <- rep(c("black", "blue"), c(nrow(obs), nrow(fcs)))
  # p <- ggplot()
  # p <- p + 
  #   geom_ribbon(aes(ymin = lb95, ymax = ub95, x = t), f, alpha = 0.1) +
  #   geom_ribbon(aes(ymin = lb80, ymax = ub80, x = t), f, alpha = 0.2)
  # p <- p + geom_line(aes(x = t, y = y, group = 1), data = new, color = cols)



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
  # yName <- options$dependentVariable[1]
  # y     <- dataset[, yName]
  yLag  <- c(rep(NA, options$lag), dataset$y[1:(length(dataset$y) - options$lag)])

  yName <- decodeColNames(options$dependentVariable[1])
  xName <- as.expression(bquote(.(yName)[t-.(options$lag)]))
  yName <- as.expression(bquote(.(yName)[t]))

  dat <- data.frame(y = dataset$y, yLag)
  dat <- na.omit(dat)

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

  stateSpacePlot$plotObject <- p
  
  return()
}

.tsACFDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies){
  if (!is.null(jaspResults[["acfContainer"]]))
    return()

  acfContainer <- createJaspContainer(title = gettext("Autocorrelation Function Plots"))
  acfContainer$dependOn(dependencies)
  jaspResults[["acfContainer"]] <- acfContainer
  jaspResults[["acfContainer"]]$position <- position

  if (!ready) {
    return()
  }

  if (options$acfPlots) {
    acfPlot <- createJaspPlot(title = "Autocorrelation Function")
    acfPlot$dependOn(dependencies)
    acfPlot$position <- 1
    acfContainer[["acfPlot"]] <- acfPlot

    .tsFillACF(acfPlot, type = "ACF", dataset, options, ci = options$acfCI, ciValue = options$acfCIValue)

    pacfPlot <- createJaspPlot(title = "Partial Autocorrelation Function")
    pacfPlot$dependOn(dependencies)
    pacfPlot$position <- 2
    acfContainer[["pacfPlot"]] <- pacfPlot

    .tsFillACF(pacfPlot, type = "PACF", dataset, options, ci = options$acfCI, ciValue = options$acfCIValue)
  }
}

.tsAC <- function(x, lag) {
  iN <- 1 / length(x)
  
  xMean    <- mean(x)
  xC       <- x - xMean
  
  c0 <- iN * sum(xC ^ 2)
  
  c1 <- numeric(lag)
  names(c1) <- paste0("lag", 1:lag)
  for(i in 1:lag) {
    xLaggedC <- lagged(x, i) - xMean
    c1[i]    <- iN * sum(xC * xLaggedC, na.rm = T)
  }
  
  ac <- c1 / c0
  return(ac)
}

.tsBartlettCI <- function(r, N, ci = 0.95) {
  z <- qnorm((1 + ci) / 2)
  
  lag <- length(r)
  df  <- data.frame(r = r, se = numeric(lag), lb = numeric(lag), ub = numeric(lag))
  for(i in 1:lag) {
    df$se[i] <- sqrt((1 / N) * (1 + 2 * sum(r[1:i] ^ 2)))
  }
  
  b <- z * df$se
  
  df$lb <- df$r - b
  df$ub <- df$r + b
  return(df)
}

.tsFillACF <- function(plot, type, dataset, options, ci, ciValue) {
  # y <- dataset[, options$dependentVariable[1]]
  y <- na.omit(dataset$y)

  if (type == "ACF")  ac <- acf(y, plot = F, lag.max = options$acfMax)
  if (type == "PACF") ac <- pacf(y, plot = F, lag.max = options$acfMax)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(ac$lag)
  xBreaks <- xBreaks[!xBreaks%%1] # keep only integers
  yRange <- ac$acf
  # if (type == "both")
  #   xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(yACF$lag, yPACF$lag))
  # dfSegment <- data.frame(x = min(xBreaks), xend = max(xBreaks))
  xMin <- min(xBreaks)
  xMax <- max(xBreaks)

  p <- ggplot2::ggplot()
  if (ci) {
    clim      <- qnorm((1 + ciValue) / 2) / sqrt(ac$n.used)
    # dfSegment <- data.frame(x = min(xBreaks), xend = max(xBreaks), y = c(clim, -clim))
    yClim <- c(clim, -clim)
    yRange    <- c(yRange, clim, -clim)

    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = xMin, xend = xMax, y = yClim, yend = yClim),
                            linetype = "dashed", color = "blue")
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)

  dat <- data.frame(acf = ac$acf, lag = ac$lag)
  # if (type == "PACF") dat <- data.frame(acf = yPACF$acf, lag = yPACF$lag)

  p <- p +
  #   ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = acf)) +
    ggplot2::scale_x_continuous(name = "Lag", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = type, breaks = yBreaks, limits = range(yBreaks))

  p <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = acf), size = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = xMin, xend = xMax, y = 0, yend = 0), alpha = 0.2) +
    # ggplot2::labs(x = "Lag", y = type) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
    # ggplot2::scale_x_continuous(name = "Lag", breaks = xBreaks, limits = range(xBreaks)) +
    # ggplot2::scale_y_continuous(name = type, breaks = yBreaks, limits = range(yBreaks))

  # p <- jaspGraphs::themeJaspRaw(p)

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
  # y <- dataset[, options$dependentVariable[1]]
  y <- na.omit(dataset$y)

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
    # ggplot2::labs(x = "Frequency", y = "Spectrum")
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
  
  p <- p + 
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()


  powerSpectralDensity$plotObject <- p
  
  return()
}
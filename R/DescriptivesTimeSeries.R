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

    .tsDescriptivesTable(jaspResults, dataset, options, ready, position = 1, dependencies = c("dependent", "descriptivesTableTransposed"))

    .tsTimeSeriesPlotDescriptives(jaspResults, dataset, options, ready, position = 2, dependencies = c("timeSeriesPlot", "timeSeriesPlotType", "dependent", "timeSeriesPlotDistribution"))

    .tslagPlotDescriptives(jaspResults, dataset, options, ready, position = 3, dependencies  = c("lagPlot", "lagPlotLag", "lagPlotRegressionType", "lagPlotRegressionLine", "lagPlotRegressionCi", "lagPlotRegressionCiLevel", "dependent"))

    .tsACFDescriptives(jaspResults, dataset, options, ready, position = 4, dependencies = c("dependent", "acf", "acfCi", "acfCiLevel", "acfCiType", "acfZeroLag", "acfMaxLag"))

    .tsPACFDescriptives(jaspResults, dataset, options, ready, position = 5, dependencies = c("dependent", "pacf", "pacfCi", "pacfCiLevel", "pacfCiType", "pacfMaxLag"))

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

.tslagPlotDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$lagPlot)
    return()

  if (is.null(jaspResults[["lagPlot"]])) {
    plot <- createJaspPlot(title = "State Space Plot")
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["lagPlot"]] <- plot

    if (!ready)
      return()

    .tsFillLagPlot(plot, dataset, options)
  }
}

.tsFillLagPlot <- function(lagPlot, dataset, options) {
  yLag  <- c(rep(NA, options$lagPlotLag), dataset$y[1:(length(dataset$y) - options$lagPlotLag)])

  yName <- decodeColNames(options$dependent[1])
  xName <- as.expression(bquote(.(yName)[t-.(options$lagPlotLag)]))
  yName <- as.expression(bquote(.(yName)[t]))

  dat <- data.frame(y = dataset$y, yLag)
  dat <- na.omit(dat)

  breaks <- jaspGraphs::getPrettyAxisBreaks(c(dat$y, dat$yLag))

  p <- jaspGraphs::JASPScatterPlot(
    dat$yLag, dat$y,
    xName = xName, 
    yName = yName,
    addSmooth = options$lagPlotRegressionLine,
    addSmoothCI = options$lagPlotRegressionCi,
    smoothCIValue = options$lagPlotRegressionCiLevel,
    forceLinearSmooth = options$lagPlotRegressionType == "linear",
    plotAbove = "none", plotRight = "none"
  )

  p$subplots$mainPlot <- p$subplots$mainPlot +
    ggplot2::scale_x_continuous(breaks = breaks, limits = range(breaks)) +
    ggplot2::scale_y_continuous(breaks = breaks, limits = range(breaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  lagPlot$plotObject <- p

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
      firstLag = options$acfZeroLag,
      maxLag = options$acfMaxLag,
      ci = options$acfCi,
      ciValue = options$acfCiLevel,
      ciType = options$acfCiType
    )
  }
}

.tsPACFDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$pacf)
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
      type = "PACF", dataset, options,
      maxLag = options$pacfMaxLag,
      ci = options$pacfCi,
      ciValue = options$pacfCiLevel,
      ciType = options$pacfCiType
    )
  }
}

.tsAcfBartlett <- function(r, N, ci = 0.95) {
  z <- qnorm((1 + ci) / 2)

  lag <- length(r)
  df  <- data.frame(r = r, se = numeric(lag))
  
  for (i in 1:lag) {
      df$se[i] <- z * sqrt((1 / N) * (1 + 2 * sum(r[1:i] ^ 2)))
  }

  return(df)
}

.tsFillACF <- function(plot, type, dataset, options, firstLag = F, maxLag, ci, ciValue, ciType) {
  y <- na.omit(dataset$y)

  if (type == "ACF") {
    ac <- stats::acf(y, plot = FALSE, lag.max = maxLag)
  }
  if (type == "PACF") {
    ac <- stats::pacf(y, plot = FALSE, lag.max = maxLag)
    ciType <- "whiteNoise"
  }

  dat <- data.frame(acf = ac$acf, lag = ac$lag)
  if(type == "ACF" & !firstLag)
    dat <- dat[-1, ] # remove lag 0

  yRange <- dat$acf
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$lag)
  xBreaks <- xBreaks[!xBreaks %% 1] # keep only integers
  xMin <- min(xBreaks)
  xMax <- max(xBreaks)

  p <- ggplot2::ggplot()
  if (ci) {
    if (ciType == "whiteNoise") {
      clim <- qnorm((1 + ciValue) / 2) / sqrt(ac$n.used)
      dat$upper <- rep(clim, nrow(dat))
      dat$lower <- -dat$upper
    } else {
      clim <- .tsAcfBartlett(dat$acf, ac$n.used, ciValue)
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
    ggplot2::geom_segment(ggplot2::aes(x = xMin, xend = xMax, y = 0, yend = 0), alpha = 0.5) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
  return()
}

.tsDescriptivesTable <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["descriptivesTable"]])) return()

  table <- createJaspTable("Descriptive Statistics")
  table$dependOn(dependencies)
  table$position <- position
  table$showSpecifiedColumnsOnly <- TRUE
  table$transpose <- !options[["descriptivesTableTransposed"]] # the table is transposed by default

  table$addColumnInfo(name = "variable",  title = " ",                        type = "string")
  table$addColumnInfo(name = "valid",     title = gettext("Valid"),           type = "integer")
  table$addColumnInfo(name = "missing",   title = gettext("Missing"),         type = "integer")
  table$addColumnInfo(name = "mean",      title = gettext("Mean"),            type = "number")
  table$addColumnInfo(name = "sd",        title = gettext("Std. Deviation"),  type = "number")
  table$addColumnInfo(name = "var",       title = gettext("Variance"),        type = "number")
  table$addColumnInfo(name = "range",     title = gettext("Range"),           type = "number")
  table$addColumnInfo(name = "min",       title = gettext("Minimum"),         type = "number")
  table$addColumnInfo(name = "max",       title = gettext("Maximum"),         type = "number")
  table$addColumnInfo(name = "start",     title = gettext("Start"),           type = "number")
  table$addColumnInfo(name = "end",       title = gettext("End"),             type = "number")
  table$addColumnInfo(name = "ar",        title = gettext("Lag 1 Autocorrelation"),  type = "number")

  jaspResults[["descriptivesTable"]] <- table

  if (ready) {   
    na.omitted <- na.omit(dataset$y)
    yName <- options$dependent[1]
    
    nY <- length(na.omitted)
    minY <- min(na.omitted)
    maxY <- max(na.omitted)

    yLag  <- c(NA, dataset$y[1:(length(dataset$y) - 1)])
    corY <- cor(dataset$y, yLag, use = "complete.obs")

    rows <- data.frame(variable = yName,
                      valid = nY,
                      missing = nrow(dataset) - nY,
                      mean = mean(na.omitted),
                      sd = sd(na.omitted),
                      var = var(na.omitted),
                      range = maxY - minY,
                      min = minY,
                      max = maxY,
                      start = na.omitted[1],
                      end = na.omitted[nY],
                      ar = corY)
    table$addRows(rows)
  }
}
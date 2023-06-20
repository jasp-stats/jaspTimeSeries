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

    dataset <- .tsReadData(jaspResults, dataset, options, ready)

    .tsDescriptivesTable(jaspResults, dataset, options, ready, position = 1, dependencies = c("dependent", "time", "descriptivesTableTransposed"))

    .tsTimeSeriesPlotDescriptives(jaspResults, dataset, options, ready, position = 2, dependencies = c("dependent", "time", "timeSeriesPlot", "timeSeriesPlotType", "timeSeriesPlotDistribution"))

    .tslagPlotDescriptives(jaspResults, dataset, options, ready, position = 3, dependencies  = c("dependent", "time", "lagPlot", "lagPlotLag", "lagPlotRegressionType", "lagPlotRegressionLine", "lagPlotRegressionCi", "lagPlotRegressionCiLevel"))

    .tsACFDescriptives(jaspResults, dataset, options, ready, position = 4, dependencies = c("dependent", "time", "acf", "acfCi", "acfCiLevel", "acfCiType", "acfZeroLag", "acfMaxLag"))

    .tsPACFDescriptives(jaspResults, dataset, options, ready, position = 5, dependencies = c("dependent", "time", "pacf", "pacfCi", "pacfCiLevel", "pacfCiType", "pacfMaxLag"))

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

.tslagPlotDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$lagPlot)
    return()

  if (is.null(jaspResults[["lagPlot"]])) {
    plot <- createJaspPlot(title = "Lag Plot")
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

.tsDescriptivesTable <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["descriptivesTable"]])) return()

  table <- createJaspTable(gettext("Descriptive Statistics"))
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
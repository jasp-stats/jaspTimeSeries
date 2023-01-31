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

DataTransformationTimeSeries <- function(jaspResults, dataset, options) {
    ready <- options$dependent != ""

    if (ready) {
      dataset <- .tsReadDataTransformation(jaspResults, dataset, options)
      transformedDataset <- .tsTransformData(jaspResults, dataset, options)
    }
    
    if (options$adfTest | options$ppTest | options$kpssLevel | options$kpssTrend) {
      .tsCreateTableStationarityTests(jaspResults, transformedDataset, options, ready, position = 2, dependencies = c(.tsTransformationDependencies, "adfTest", "ppTest", "kpssLevel", "kpssTrend"))
    }

    .tsSaveTransformation(transformedDataset, options, jaspResults, ready)

    .tsTimeSeriesPlotTransformation(jaspResults, transformedDataset, options, ready, position = 1, dependencies = c(.tsTransformationDependencies, "timeSeriesPlot", "timeSeriesPlotType", "timeSeriesPlotDistribution"))

    .tsACFTransformation(jaspResults, transformedDataset, options, ready, position = 3, dependencies = c(.tsTransformationDependencies, "acf", "acfCi", "acfCiLevel", "acfCiType", "acfMaxLag"))

    .tsPACFTransformation(jaspResults, transformedDataset, options, ready, position = 4, dependencies = c(.tsTransformationDependencies, "pacf", "pacfCi", "pacfCiLevel", "pacfCiType", "pacfMaxLag"))
}

.tsTransformationDependencies <- c("dependent", "transformation", "poly")

.tsReadDataTransformation <- function(jaspResults, dataset, options) {
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

.tsTransformData <- function(jaspResults, dataset, options) {
  transformedDataset <- dataset
  if (options$transformation == "center") {
    transformedDataset$y <- dataset$y - mean(dataset$y)
  }

  if (options$transformation == "detrend") {
    transformedDataset$y <- residuals(lm(y ~ poly(t, options$poly), data = dataset))
  }

  return(transformedDataset)
}

.tsSaveTransformation <- function(transformedDataset, options, jaspResults, ready) {
  if (options[["transformationSavedToData"]] && is.null(jaspResults[["transformationColumn"]]) && options[["transformationColumn"]] != "" && ready) {
    transformationColumn <- rep(NA, max(as.numeric(rownames(transformedDataset))))
    transformationColumn[as.numeric(rownames(transformedDataset))] <- transformedDataset$y
    jaspResults[["transformationColumn"]] <- createJaspColumn(columnName = options[["transformationColumn"]])
    jaspResults[["transformationColumn"]]$dependOn(options = c("transformationColumn", "transformationSavedToData", "transformation", "poly", "dependentVariable"))
    jaspResults[["transformationColumn"]]$setScale(transformationColumn)
  }
}

.tsCreateTableStationarityTests <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["stationaryTable"]]) & (options$adfTest | options$ppTest | options$kpssLevel | options$kpssTrend)) {
    return()
  }

  stationaryTable <- createJaspTable("Stationarity Tests")
  stationaryTable$dependOn(dependencies)
  stationaryTable$position <- position
  stationaryTable$showSpecifiedColumnsOnly <- TRUE

  stationaryTable$addColumnInfo(name = "test", title = gettext("Test"), type = "string")
  stationaryTable$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  stationaryTable$addColumnInfo(name = "lag", title = gettext("Truncation lag parameter"), type = "integer")
  stationaryTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  stationaryTable$addColumnInfo(name = "null", title = sprintf("H\u2080"), type = "string")

  jaspResults[["stationaryTable"]] <- stationaryTable

  # Check if ready
  if (!ready) {
    rows <- data.frame(
      test = ".",
      statistic = ".",
      lag = ".",
      p = ".",
      null = "."
    )
    row.names(rows) <- paste0("row", 1)
    stationaryTable$addRows(rows)
    return()
  }

  smallerA <- smallerP <- smallerKl <- smallerKt <- greaterA <- greaterP <- greaterKl <- greaterKt <- F
  if (options$adfTest) {
    # alternative stationary
    fit <- tseries::adf.test(dataset$y)
    smallerA <- fit$p.value == 0.01
    greaterA <- fit$p.value == 0.99
    dfA <- .stationarityRows(fit, "Augmented Dickey-Fuller t", gettext("Non-stationary")) # h0 a unit root
    # p value 0.01 - 0.99
    stationaryTable$addRows(dfA)
    stationaryTable$setRowName(rowIndex = 1, newName = "adf")
  }
  if (options$ppTest) {
    # type = c("Z(alpha)", "Z(t_alpha)")
    # alternative stationary
    # rho normalized bias test (regression coefficient) vs. tau studentized test
    ppType <- "Z(\u03B1)"
    fit <- tseries::pp.test(dataset$y)
    smallerP <- fit$p.value == 0.01
    greaterP <- fit$p.value == 0.99
    dfP <- .stationarityRows(fit, sprintf("Phillips-Perron %s", ppType), gettext("Non-stationary"))
    stationaryTable$addRows(dfP)
    idxP <- ifelse(options$adfTest, 2, 1)
    stationaryTable$setRowName(rowIndex = idxP, newName = "pp")
    # p value 0.01 - 0.99
  }
  if (options$kpssLevel) {
    # null is stationary
    # null = c("Level", "Trend")
    fit <- tseries::kpss.test(dataset$y, null = "Level")
    smallerKl <- fit$p.value == 0.01
    greaterKl <- fit$p.value == 0.1
    dfKl <- .stationarityRows(
      fit,
      sprintf("Kwiatkowski-Phillips-Schmidt-Shin Level %s", "\u03B7"),
      gettext("Level stationary")
    ) # mean stationarity
    stationaryTable$addRows(dfKl)
    idxKl <- ifelse(options$adfTest & options$ppTest, 3, ifelse(options$adfTest | options$ppTest, 2, 1))
    stationaryTable$setRowName(rowIndex = idxKl, newName = "kpssLevel")
    # p value 0.1 - 0.01
  }
  if (options$kpssTrend) {
    fit <- tseries::kpss.test(dataset$y, null = "Trend")
    smallerKt <- fit$p.value == 0.01
    greaterKt <- fit$p.value == 0.1
    dfKt <- .stationarityRows(
      fit,
      sprintf("Kwiatkowski-Phillips-Schmidt-Shin Trend %s", "\u03B7"),
      gettext("Trend stationary")
    ) # linear trend stationarity

    stationaryTable$addRows(dfKt)
    otherRows <- sum(options$adfTest, options$ppTest, options$kpssLevel)
    idxKt <- ifelse(otherRows == 3, 4,
      ifelse(otherRows == 2, 3,
        ifelse(otherRows == 1, 2, 1)
      )
    )
    stationaryTable$setRowName(rowIndex = idxKt, newName = "kpssTrend")
    # p value 0.1 - 0.01
  }

  if (smallerA | smallerP | smallerKl | smallerKt) {
    stationaryTable$addFootnote(gettext("The p-value is actually less than p-value shown (see Help file)."),
      colNames = "p", rowNames = .stationaryFootnoteRows(smallerA, smallerP, smallerKl, smallerKt, options)
    )
  }

  if (greaterA | greaterP | greaterKl | greaterKt) {
    stationaryTable$addFootnote(gettext("The p-value is actually greater than p-value shown (see Help file)."),
      colNames = "p", rowNames = .stationaryFootnoteRows(greaterA, greaterP, greaterKl, greaterKt, options)
    )
  }
}

.stationaryFootnoteRows <- function(A, P, Kl, Kt, options) {
  footRow <- NULL
  if (options$adfTest & A) footRow <- c(footRow, "adf")
  if (options$ppTest & P) footRow <- c(footRow, "pp")
  if (options$kpssLevel & Kl) footRow <- c(footRow, "kpssLevel")
  if (options$kpssTrend & Kt) footRow <- c(footRow, "kpssTrend")
  return(footRow)
}

.stationarityRows <- function(fit, test, null) {
  # p <- fit$p.value
  # sgn <- ""
  # if (p == 0.01) sgn <- "<"
  # if (p == 0.99 | (kpss & p == 0.1)) sgn <- ">"
  # p <- paste(sgn, substring(sprintf("%.3f", p), 2))
  df <- data.frame(test = test, statistic = fit$statistic, lag = fit$parameter, p = fit$p.value, null = null)
  return(df)
}

# .tsPlotsOriginalData <- function(jaspResults, dataset, options, ready, position, dependencies) {

#   tsContainer <- createJaspContainer(title = gettext("Original Data"))
#   tsContainer$dependOn(dependencies)
#   jaspResults[["tsContainer"]] <- tsContainer
#   jaspResults[["tsContainer"]]$position <- position

#   if (!ready | !options[["originalDataPlots"]]) {
#     return()
#   }

#   if (is.null(tsContainer[["originalDataTimeSeriesPlot"]])) {
#     originalDataTimeSeriesPlot <- createJaspPlot(title = "Time Series Plot", width = 660)
#     originalDataTimeSeriesPlot$dependOn(c("originalDataTsType", "originalDataDistribution"))
#     originalDataTimeSeriesPlot$position <- 1
#     tsContainer[["originalDataTimeSeriesPlot"]] <- originalDataTimeSeriesPlot

#     if (!ready) {
#       return()
#     }

#     .tsFillTimeSeriesPlot(originalDataTimeSeriesPlot, dataset, options, type = options$timeSeriesPlotType, distribution = options$timeSeriesPlotDistribution)
#   }

#   if (is.null(tsContainer[["acfPlot"]])) {
#     acfPlot <- createJaspPlot(title = "Autocorrelation Function Plot")
#     acfPlot$dependOn(c("acfCi", "acfCiLevel", "acfCiType", "acfMaxLag"))
#     acfPlot$position <- 2
#     tsContainer[["acfPlot"]] <- acfPlot

#     if (!ready) {
#       return()
#     }

#     .tsFillACF(acfPlot, type = "ACF", dataset, options, maxLag = options$acfMaxLag, ci = options$acfCi, ciValue = options$acfCiLevel, ciType = options$acfCiType)
#   }

#   if (is.null(tsContainer[["pacfPlot"]])) {
#     pacfPlot <- createJaspPlot(title = "Partial Autocorrelation Function Plot")
#     pacfPlot$dependOn(c("pacfCi", "pacfCiLevel", "pacfCiType", "pacfMaxLag"))
#     pacfPlot$position <- 2
#     tsContainer[["pacfPlot"]] <- pacfPlot

#     if (!ready) {
#       return()
#     }

#     .tsFillACF(pacfPlot, type = "PACF", dataset, options, maxLag = options$pacfMaxLag, ci = options$pacfCi, ciValue = options$acfCiLevel, ciType = options$acfCiType)
#   }
# }

.tsTimeSeriesPlotTransformation <- function(jaspResults, dataset, options, ready, position, dependencies) {
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

.tsACFTransformation <- function(jaspResults, dataset, options, ready, position, dependencies){
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
      maxLag = options$acfMaxLag,
      ci = options$acfCi,
      ciValue = options$acfCiLevel,
      ciType = options$acfCiType
    )
  }
}

.tsPACFTransformation <- function(jaspResults, dataset, options, ready, position, dependencies) {
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
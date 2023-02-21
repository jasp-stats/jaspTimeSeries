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
    
    if (options$adfTest | options$ppTestRegressionCoefficient | options$ppTestStudentized | options$kpssLevel | options$kpssTrend) {
      .tsCreateTableStationarityTests(jaspResults, transformedDataset, options, ready, position = 2, dependencies = c(.tsTransformationDependencies, "adfTest", "ppTestRegressionCoefficient", "ppTestStudentized", "kpssLevel", "kpssTrend"))
    }

    .tsSaveTransformation(transformedDataset, options, jaspResults, ready)

    .tsTimeSeriesPlotTransformation(jaspResults, transformedDataset, options, ready, position = 1, dependencies = c(.tsTransformationDependencies, "timeSeriesPlot", "timeSeriesPlotType", "timeSeriesPlotDistribution"))

    .tsACFTransformation(jaspResults, transformedDataset, options, ready, position = 3, dependencies = c(.tsTransformationDependencies, "acf", "acfCi", "acfCiLevel", "acfCiType", "acfFirstLag", "acfMaxLag"))

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
  if (!is.null(jaspResults[["stationaryTable"]]) & (options$adfTest | options$ppTestRegressionCoefficient | options$ppTestStudentized | options$kpssLevel | options$kpssTrend)) {
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

  test <- c(
    "Augmented Dickey-Fuller t",
    paste("Phillips-Perron", c("regression coefficient \u03C1", "studentized \u03C4")),
    paste("Kwiatkowski-Phillips-Schmidt-Shin", c(gettext("Level"), gettext("Trend")), "\u03B7")
  )

  null <- c(
    rep(gettext("Non-stationary"), 3),
    gettext("Level stationary"),
    gettext("Trend stationary")
  )

  idx <- c(options$adfTest, options$ppTestRegressionCoefficient, options$ppTestStudentized, options$kpssLevel, options$kpssTrend)
  rows <- 1:5
  rowsIdx <- rows[idx]

  if (!ready) {
    rows <- data.frame(
      test = test[idx],
      statistic = ".",
      lag = ".",
      p = ".",
      null = null[idx]
    )
    row.names(rows) <- paste0("row", rowsIdx)
    stationaryTable$addRows(rows)
    return()
  }

  df <- data.frame(test, statistic = numeric(5), lag = numeric(5), p = numeric(5), null)
  smallerA <- smallerP <- smallerKl <- smallerKt <- greaterA <- greaterP <- greaterKl <- greaterKt <- F
  if (options$adfTest) {
    fit <- tseries::adf.test(dataset$y)
    df[1, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[1, ])
    stationaryTable$setRowName(rowIndex = 1, newName = "adf")
    smallerA <- fit$p.value == 0.01
    greaterA <- fit$p.value == 0.99   # p value 0.01 - 0.99
  }
  if (options$ppTestRegressionCoefficient) {
    # type = c("Z(alpha)", "Z(t_alpha)")
    # rho normalized bias test (regression coefficient) vs. tau studentized test
    # ppType <- "Z(\u03B1)"
    fit <- tseries::pp.test(dataset$y, type = "Z(alpha)")
    df[2, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[2, ])
    stationaryTable$setRowName(rowIndex = which(rowsIdx == 2), newName = "ppRegression")
    smallerPr <- fit$p.value == 0.01
    greaterPr <- fit$p.value == 0.99   # p value 0.01 - 0.99
  }
  if (options$ppTestStudentized) {
    fit <- tseries::pp.test(dataset$y, type = "Z(t_alpha)")
    df[3, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[3, ])
    stationaryTable$setRowName(rowIndex = which(rowsIdx == 3), newName = "ppStudentized")
    smallerPs <- fit$p.value == 0.01
    greaterPs <- fit$p.value == 0.99   # p value 0.01 - 0.99
  }
  if (options$kpssLevel) {
    fit <- tseries::kpss.test(dataset$y, null = "Level")
    df[4, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[4, ])
    stationaryTable$setRowName(rowIndex = which(rowsIdx == 4), newName = "kpssLevel")
    smallerKl <- fit$p.value == 0.01
    greaterKl <- fit$p.value == 0.1    # p value 0.1 - 0.01
  }
  if (options$kpssTrend) {
    fit <- tseries::kpss.test(dataset$y, null = "Trend")
    df[5, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[5, ])
    stationaryTable$setRowName(rowIndex = which(rowsIdx == 5), newName = "kpssTrend")
    smallerKt <- fit$p.value == 0.01
    greaterKt <- fit$p.value == 0.1    # p value 0.1 - 0.01
  }

  if (smallerA | smallerPr | smallerPs | smallerKl | smallerKt) {
    stationaryTable$addFootnote(gettext("The p-value is actually less than p-value shown (see Help file)."),
      colNames = "p", rowNames = .stationaryFootnoteRows(smallerA, smallerPr, smallerPs, smallerKl, smallerKt, options)
    )
  }

  if (greaterA | greaterPr | greaterPs | greaterKl | greaterKt) {
    stationaryTable$addFootnote(gettext("The p-value is actually greater than p-value shown (see Help file)."),
      colNames = "p", rowNames = .stationaryFootnoteRows(greaterA, greaterPr, greaterPs, greaterKl, greaterKt, options)
    )
  }
}

.stationaryFootnoteRows <- function(A, Pr, Ps, Kl, Kt, options) {
  footRow <- NULL
  if (options$adfTest & A) footRow <- c(footRow, "adf")
  if (options$ppTestRegressionCoefficient & Pr) footRow <- c(footRow, "ppRegression")
  if (options$ppTestStudentized & Ps) footRow <- c(footRow, "ppStudentized")
  if (options$kpssLevel & Kl) footRow <- c(footRow, "kpssLevel")
  if (options$kpssTrend & Kt) footRow <- c(footRow, "kpssTrend")
  return(footRow)
}

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
      firstLag = options$acfFirstLag,
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
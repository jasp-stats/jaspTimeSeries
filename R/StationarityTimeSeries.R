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

StationarityTimeSeries <- function(jaspResults, dataset, options) {
  ready <- options$dependent != ""

  dataset <- .tsReadData(jaspResults, dataset, options, ready)
  transformedDataset <- .tsTransformData(jaspResults, dataset, options, ready)

  .tsErrorHandler(dataset, ready)

  .tsCreateTableStationarityTests(jaspResults, transformedDataset, options, ready, position = 2, dependencies = c(.tsTransformationDependencies, "adfTest", "ppTestRegressionCoefficient", "ppTestStudentized", "kpssLevel", "kpssTrend"))

  .tsSaveTransformation(transformedDataset, options, jaspResults, ready, dependencies = c(.tsTransformationDependencies, "transformationColumn", "transformationSavedToData"))

  .tsTimeSeriesPlotTransformation(jaspResults, transformedDataset, options, ready, position = 1, dependencies = c(.tsTransformationDependencies, "timeSeriesPlot", "timeSeriesPlotType", "timeSeriesPlotDistribution"))

  .tsACFTransformation(jaspResults, transformedDataset, options, ready, position = 3, dependencies = c(.tsTransformationDependencies, "acf", "acfCi", "acfCiLevel", "acfCiType", "acfZeroLag", "acfMaxLag"))

  .tsPACFTransformation(jaspResults, transformedDataset, options, ready, position = 4, dependencies = c(.tsTransformationDependencies, "pacf", "pacfCi", "pacfCiLevel", "pacfCiType", "pacfMaxLag"))

  .tsCreateTablePolynomials(jaspResults, transformedDataset, options, ready, position = 5, dependencies = .tsTransformationDependencies)
}

.tsTransformationDependencies <- c(
  "dependent", "time", "detrend", "detrendPoly", "log", "logBase", "root",
  "rootIndex", "boxCox", "boxCoxLambdaSpecification", "boxCoxLambda",
  "difference", "differenceLag", "differenceOrder", "polynomialSpecification",
  "polynomialSpecificationAutoIc", "polynomialSpecificationAutoMax"
)

.tsTransformData <- function(jaspResults, dataset, options, ready) {
  if (!ready) {
    return()
  }

  transformedDataset <- dataset

  # apply log
  if (options$log) {
    if (any(transformedDataset$y <= 0)) {
      .quitAnalysis(gettext("Data cannot be zero or negative for log transformation."))
    }

    if (options$logBase == "10") transformedDataset$y <- log10(transformedDataset$y)
    if (options$logBase == "e") transformedDataset$y <- log(transformedDataset$y)
  }

  # apply root
  if (options$root) {
    if (any(transformedDataset$y < 0)) {
      .quitAnalysis(gettext("Data cannot be negative for root transformation."))
    }

    if (options$rootIndex == "square") transformedDataset$y <- sqrt(transformedDataset$y)
    if (options$rootIndex == "cube") transformedDataset$y <- transformedDataset$y^(1 / 3)
  }

  # apply Box-Cox transformation
  if (options$boxCox) {
    if (options$boxCoxLambdaSpecification == "auto") lambda <- "auto"
    if (options$boxCoxLambdaSpecification == "custom") lambda <- options$boxCoxLambda

    transformedDataset$y <- forecast::BoxCox(transformedDataset$y, lambda = lambda)
  }

  # apply detrend using linear regression and save only residuals
  if (options$detrend) {
    if (options$polynomialSpecification == "custom") {
      transformedDataset$y <- residuals(lm(y ~ poly(as.integer(t), options$detrendPoly), data = transformedDataset))
    }
    if (options$polynomialSpecification == "auto") {
      .tsComputePolyResults(dataset, options, jaspResults, ready)
      lmFit <- jaspResults[["polyResult"]]$object
      best <- which.min(unlist(lmFit[options$polynomialSpecificationAutoIc, ]))
      transformedDataset$y <- unlist(lmFit["residuals", best])
    }
  }

  # apply differencing
  if (options$difference) {
    differencedY <- diff(
      transformedDataset$y,
      lag = options$differenceLag,
      order = options$differenceOrder
    )

    # add NA's to start of the transformed dependent variable
    # because differencing makes the variable shorter (n - lag)
    transformedDataset$y <- c(
      rep(NA, nrow(transformedDataset) - length(differencedY)),
      differencedY
    )
  }

  return(transformedDataset)
}

.tsCreateTablePolynomials <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["polyTable"]]) || !(options$detrend && options$polynomialSpecification == "auto")) {
    return()
  }

  table <- createJaspTable(gettext("Detrend using linear regression"))
  table$dependOn(dependencies)
  table$position <- position
  table$addColumnInfo(name = "degree", title = gettext("Degree"), type = "integer")
  table$addColumnInfo(name = "aic", title = gettext("AIC"), type = "number")
  table$addColumnInfo(name = "bic", title = gettext("BIC"), type = "number")

  jaspResults[["polyTable"]] <- table

  # Check if ready
  if (!ready) {
    rows <- data.frame(
      degree = ".",
      aic = ".",
      bic = "."
    )
    row.names(rows) <- paste0("row", 1)
    table$addRows(rows)
    return()
  }

  .tsComputePolyResults(dataset, options, jaspResults, ready)
  lmFit <- jaspResults[["polyResult"]]$object
  best <- lmFit["degree", which.min(unlist(lmFit[options$polynomialSpecificationAutoIc, ]))]

  rows <- data.frame(
    degree = unlist(lmFit["degree", ]),
    aic = unlist(lmFit["aic", ]),
    bic = unlist(lmFit["bic", ])
  )
  row.names(rows) <- paste0("row", 1:length(unlist(lmFit["degree", ])))
  table$addRows(rows)

  table$addFootnote(gettextf("A polynomial regression with a degree of %s was fitted.", best))
}

.tsFitPoly <- function(degree, x, t) {
  # fit polynomial regression and save residuals, degree, and ics
  if (degree == 0) {
    m <- lm(x ~ 1)
  } else {
    m <- lm(x ~ poly(t, degree = degree))
  }
  return(list(residuals = residuals(m), degree = degree, aic = AIC(m), bic = BIC(m)))
}

.tsComputePolyResults <- function(dataset, options, jaspResults, ready) {
  if (!is.null(jaspResults[["polyResult"]])) {
    return()
  }

  if (ready) {
    res <- sapply(0:(options$polynomialSpecificationAutoMax), .tsFitPoly, x = dataset$y, t = as.integer(dataset$t))

    jaspResults[["polyResult"]] <- createJaspState(res)
    jaspResults[["polyResult"]]$dependOn(c("polynomialSpecificationAutoMax"))
  }
}

.tsSaveTransformation <- function(transformedDataset, options, jaspResults, ready, dependencies) {
  # append transformed variable to spreadsheet
  if (options[["transformationSavedToData"]] && is.null(jaspResults[["transformationColumn"]]) && options[["transformationColumn"]] != "" && ready) {
    transformationColumn <- rep(NA, max(as.numeric(rownames(transformedDataset))))
    transformationColumn[as.numeric(rownames(transformedDataset))] <- transformedDataset$y
    jaspResults[["transformationColumn"]] <- createJaspColumn(columnName = options[["transformationColumn"]])
    jaspResults[["transformationColumn"]]$dependOn(dependencies)
    jaspResults[["transformationColumn"]]$setScale(transformationColumn)
  }
}

.tsCreateTableStationarityTests <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["stationaryTable"]]) || !(options$adfTest || options$ppTestRegressionCoefficient || options$ppTestStudentized || options$kpssLevel || options$kpssTrend)) {
    return()
  }

  stationaryTable <- createJaspTable(gettext("Stationarity Tests"))
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

  # each checkbox indicates a row in the table
  idx <- c(
    options$adfTest,
    options$ppTestRegressionCoefficient,
    options$ppTestStudentized,
    options$kpssLevel,
    options$kpssTrend
  )
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

  dataset <- dataset[complete.cases(dataset), ]

  df <- data.frame(test, statistic = numeric(5), lag = numeric(5), p = numeric(5), null)

  smallerA <- smallerPr <- smallerPs <- smallerKl <- smallerKt <- greaterA <- greaterPr <- greaterPs <- greaterKl <- greaterKt <- F
  if (options$adfTest) {
    fit <- try(tseries::adf.test(dataset$y))
    if (jaspBase::isTryError(fit)) .quitAnalysis(gettext("The ADF test failed."))
    df[1, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[1, ])
    stationaryTable$setRowName(rowIndex = 1, newName = "adf")
    smallerA <- fit$p.value == 0.01
    greaterA <- fit$p.value == 0.99 # p value 0.01 - 0.99
  }
  if (options$ppTestRegressionCoefficient) {
    # type = c("Z(alpha)", "Z(t_alpha)")
    # rho normalized bias test (regression coefficient) vs. tau studentized test
    fit <- try(tseries::pp.test(dataset$y, type = "Z(alpha)"))
    if (jaspBase::isTryError(fit)) .quitAnalysis(gettext("The PP regression coefficient test failed."))
    df[2, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[2, ])
    stationaryTable$setRowName(rowIndex = which(rowsIdx == 2), newName = "ppRegression")
    smallerPr <- fit$p.value == 0.01
    greaterPr <- fit$p.value == 0.99 # p value 0.01 - 0.99
  }
  if (options$ppTestStudentized) {
    fit <- try(tseries::pp.test(dataset$y, type = "Z(t_alpha)"))
    if (jaspBase::isTryError(fit)) .quitAnalysis(gettext("The PP studentized test failed."))
    df[3, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[3, ])
    stationaryTable$setRowName(rowIndex = which(rowsIdx == 3), newName = "ppStudentized")
    smallerPs <- fit$p.value == 0.01
    greaterPs <- fit$p.value == 0.99 # p value 0.01 - 0.99
  }
  if (options$kpssLevel) {
    fit <- try(tseries::kpss.test(dataset$y, null = "Level"))
    if (jaspBase::isTryError(fit)) .quitAnalysis(gettext("The KPSS test for level stationarity failed."))
    df[4, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[4, ])
    stationaryTable$setRowName(rowIndex = which(rowsIdx == 4), newName = "kpssLevel")
    smallerKl <- fit$p.value == 0.01
    greaterKl <- fit$p.value == 0.1 # p value 0.1 - 0.01
  }
  if (options$kpssTrend) {
    fit <- try(tseries::kpss.test(dataset$y, null = "Trend"))
    if (jaspBase::isTryError(fit)) .quitAnalysis(gettext("The KPSS test for trend stationarity failed."))
    df[5, c("statistic", "lag", "p")] <- c(fit$statistic, fit$parameter, fit$p.value)
    stationaryTable$addRows(df[5, ])
    stationaryTable$setRowName(rowIndex = which(rowsIdx == 5), newName = "kpssTrend")
    smallerKt <- fit$p.value == 0.01
    greaterKt <- fit$p.value == 0.1 # p value 0.1 - 0.01
  }

  # these tests do not have exact computations for the p-value
  # but use a table of critical valuesy, so sometimes the p-value
  # falls outside of table and then a note is added
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
  # add footnote only for relevant p-values
  footRow <- NULL
  if (options$adfTest & A) footRow <- c(footRow, "adf")
  if (options$ppTestRegressionCoefficient & Pr) footRow <- c(footRow, "ppRegression")
  if (options$ppTestStudentized & Ps) footRow <- c(footRow, "ppStudentized")
  if (options$kpssLevel & Kl) footRow <- c(footRow, "kpssLevel")
  if (options$kpssTrend & Kt) footRow <- c(footRow, "kpssTrend")
  return(footRow)
}

.tsTimeSeriesPlotTransformation <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$timeSeriesPlot) {
    return()
  }

  if (is.null(jaspResults[["timeSeriesPlot"]])) {
    plot <- createJaspPlot(title = "Time Series Plot", width = 660)
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["timeSeriesPlot"]] <- plot

    if (!ready) {
      return()
    }

    .tsFillTimeSeriesPlot(plot, dataset, options, type = options$timeSeriesPlotType, distribution = options$timeSeriesPlotDistribution)
  }
}

.tsACFTransformation <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$acf) {
    return()
  }

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
      zeroLag = options$acfZeroLag,
      maxLag = options$acfMaxLag,
      ci = options$acfCi,
      ciValue = options$acfCiLevel,
      ciType = options$acfCiType
    )
  }
}

.tsPACFTransformation <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$pacf) {
    return()
  }

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

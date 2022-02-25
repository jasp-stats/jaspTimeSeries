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

ARIMATimeSeries <- function(jaspResults, dataset, options) {
    # ready <- (length(options$dependentVariable) > 0)
    ready <- options$dependentVariable != ""

    if (ready) {
      rawData <- .tsReadData(jaspResults, dataset, options)
      dataset <- .tsPrepareData(jaspResults, rawData, options)
    }
    
    fit <- .tsResults(jaspResults, dataset, options, ready)

    .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1, dependencies = c("transformation", "timeSeriesPlot", "tsType", "dependentVariable"))

    .tsCreateTableCoefficients(jaspResults, fit, dataset, options, ready, position = 2)
    # .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1)
    
    # .tsACF(jaspResults, dataset, options, ready, position = 3)
}

.tsDependencies <- c("model", "ic", "best", "manual",
                     "method", "p", "d", "q",
                     "dependentVariable", "center", "detrend", "transformation")

.tsReadData <- function(jaspResults, dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = options$dependentVariable))
}

.tsPrepareData <- function(jaspResults, dataset, options) {
  yName <- options$dependentVariable[1]
  y     <- dataset[, yName]
  t     <- 1:nrow(dataset)

  if (options$transformation == "center") y <- y - mean(y)

  if (options$transformation == "detrend") {
    lmFit <- lm(y ~ poly(t, options$poly))
    y <- lmFit$residuals
  }
  # add covariates at some point
  return(data.frame(y, t))
}

.tsTimeSeriesPlot <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$timeSeriesPlot)
    return()
  
  if (is.null(jaspResults[["timeSeriesPlot"]])) {
    plot <- createJaspPlot(title = "Time Series Plot", width = 480)
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["timeSeriesPlot"]] <- plot

    if (!ready)
      return()

    .tsFillTimeSeriesPlot(plot, dataset, options, type = options$tsType)
  }
}

.tsResults <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return()

  # yName <- options$dependentVariable[1]
  y     <- ts(dataset$y)
  
  # if(options$best) fit <- auto.arima(y, ic = options$ic)

  if (options$model == "manual") {
    # p   <- ifelse(options$ar, options$p, 0)
    # d   <- ifelse(options$i,  options$d, 0)
    # q   <- ifelse(options$ma, options$q, 0)
    fit <- arima(y, order = c(options$p, options$d, options$q))
        # fit <- forecast::Arima(y, order = c(options$p, options$d, options$q), include.constant = T)
  }

  if (options$model == "best") fit <- forecast::auto.arima(y)

  if (length(fit$coef) == 0)
    .quitAnalysis("No parameters are estimated.")


  return(fit)
}

.tsCreateTableCoefficients <- function(jaspResults, fit, dataset, options, ready, position) {
  if (!is.null(jaspResults[["coefTable"]])) return()

  coefTable <- createJaspTable("Coefficients")
  coefTable$dependOn(.tsDependencies)
  coefTable$position <- position
  coefTable$showSpecifiedColumnsOnly <- TRUE

  coefTable$addColumnInfo(name = "coefficients",   title = "",                         type = "string")
  coefTable$addColumnInfo(name = "estimate",       title = gettext("Estimate"),        type = "number")
  coefTable$addColumnInfo(name = "SE",             title = gettext("Standard Error"),  type = "number")

  # coefTable$setExpectedSize(2)

  jaspResults[["coefTable"]] <- coefTable

  # Check if ready
  if(!ready) {
    rows <- data.frame(coefficients = ".",
                       estimate = ".",
                       SE = ".")
    row.names(rows) <- paste0("row", 1)
    coefTable$addRows(rows)
    return()
  }

  # fit$arma
  # p q P Q f d D
  # f is frequency (or period) of time series
  # arimaorder function
  p <- fit$arma[1]
  d <- fit$arma[6]
  q <- fit$arma[2]

  estimate  <- fit$coef
  SE        <- sqrt(diag(fit$var.coef))

  coefficients <- character()
  group <- logical()

  if(d <= 1 & (p + q) != length(estimate)) {
    group <- T
    coefficients <- gettext("Constant")
    # I want the intercept to be the first row...
    estimate  <- c(estimate[length(estimate)], estimate[-length(estimate)])
    SE        <- c(SE[length(SE)], SE[-length(SE)])
  }

  if(p >= 1) {
    ar <- sprintf("AR(%d)", 1:p)
    coefficients <- c(coefficients, ar)
    group <- c(group, T, rep(F, p - 1))
  }

  if(q >= 1) {
    ma <- sprintf("MA(%d)", 1:q)
    coefficients <- c(coefficients, ma)
    group <- c(group, T, rep(F, q - 1))
  }

  rows <- data.frame(coefficients = coefficients,
                     estimate = estimate,
                     SE = SE,
                     .isNewGroup = group)
  row.names(rows) <- paste0("row", 1:length(coefficients))
  coefTable$addRows(rows)

  coefTable$addFootnote(gettextf("ARIMA(%s, %s, %s)", p, d, q))
}
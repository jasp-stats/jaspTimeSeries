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

ARMATimeSeries <- function(jaspResults, dataset, options) {
    # ready <- (length(options$dependentVariable) > 0)
    ready <- options$dependentVariable != ""

    if (ready)
      dataset <- .tsReadData(jaspResults, dataset, options)
    
    fit <- .tsResults(jaspResults, dataset, options, ready)

    .tsCreateTableCoefficients(jaspResults, fit, dataset, options, position = 1, ready)
    # .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1)
    
    # .tsACF(jaspResults, dataset, options, ready, position = 3)
}

.tsDependencies <- c("model", "best", "ic", "manual",
                     "method", "ar", "i", "ma", "p", "d", "q",
                     "dependentVariable", "center", "detrend")

.tsReadData <- function(jaspResults, dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = options$dependentVariable))
}

.tsResults <- function(jaspResults, dataset, options, ready) {
  if(!ready)
    return()

  yName <- options$dependentVariable[1]
  y     <- ts(dataset[, yName])
  
  # if(options$best) fit <- auto.arima(y, ic = options$ic)

  if(options$manual) {
    p   <- ifelse(options$ar, options$p, 0)
    d   <- ifelse(options$i,  options$d, 0)
    q   <- ifelse(options$ma, options$q, 0)
    fit <- arima(y, order = c(p, d, q))
  }

  return(fit)
}

.tsCreateTableCoefficients <- function(jaspResults, fit, dataset, options, position, ready) {
  if (!is.null(jaspResults[["coef 	fTable"]])) return()

  coeffTable <- createJaspTable("Coefficients")
  coeffTable$dependOn(.tsDependencies)
  coeffTable$position <- position
  coeffTable$showSpecifiedColumnsOnly <- TRUE

  coeffTable$addColumnInfo(name = "coefficients",   title = "",                         type = "string")
  coeffTable$addColumnInfo(name = "lag",            title = gettext("Lag"),             type = "integer")
  coeffTable$addColumnInfo(name = "estimate",       title = gettext("Estimate"),        type = "number")
  coeffTable$addColumnInfo(name = "SE",             title = gettext("Standard Error"),  type = "number")

  # coeffTable$setExpectedSize(2)

  jaspResults[["coeffTable"]] <- coeffTable
  
  coefficients <- gettext("Intercept")
  lagAR <- NA
  group <- T
  if(options$lagAR >= 1) {
    coefficients <- c(coefficients, options$dependentVariable[1])
    if(options$lagAR > 1) {
    coefficients <- c(coefficients, rep(NA, options$lagAR - 1))
    }
    lagAR <- c(lagAR, 1:options$lagAR)
    group <- c(group, T, rep(F, options$lagAR - 1))
  }

  # Check if ready
  if(!ready) {
    rows <- data.frame(coefficients = coefficients,
                       lag = "",
                       estimate = ".",
                       SE = ".")
    row.names(rows) <- paste0("row", 1)
    coeffTable$addRows(rows)
    return()
  }

  estimate  <- fit$coef
  SE        <- sqrt(diag(fit$var.coef))

  # I want the intercept to be the first row...
  estimate  <- c(estimate[length(estimate)], estimate[-length(estimate)])
  SE        <- c(SE[length(SE)], SE[-length(SE)])

  rows <- data.frame(coefficients = coefficients,
                     lag = lagAR, 
                     estimate = estimate,
                     SE = SE,
                     .isNewGroup = group)
  row.names(rows) <- paste0("row", 1:length(coefficients))
  coeffTable$addRows(rows)
}
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

    .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1, dependencies = c("transformation", "timeSeriesPlot", "tsType", "dependentVariable", "distribution"))

    .tsCreateTableCoefficients(jaspResults, fit, dataset, options, ready, position = 2)

    if (options$adfTest | options$ppTest | options$kpssTest)
      .tsCreateTableStationarityTests(jaspResults, fit, dataset, options, ready, position = 3)
    
    .tsResidualDiagnostics(jaspResults, fit, dataset, options, ready, position = 4, dependencies = .tsDependencies)
    # .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1)
    
    # .tsACF(jaspResults, dataset, options, ready, position = 3)

    if (options$forecastTimeSeries)
      .tsForecastPlot(jaspResults, fit, dataset, options, ready, position = 5, dependencies = c(.tsDependencies, "forecastTimeSeries", "addObserved", "forecastType", "nForecasts"))
}

# data dependencies

.tsDependencies <- c("model", "ic", "best", "manual", "addConstant",
                     "p", "d", "q",
                     "dependentVariable", "center", "detrend", "transformation",
                     "addSeasonal", "period", "m", "P", "D", "Q")

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
    y <- ts(y)
    y <- residuals(forecast::tslm(y ~ trend))
    # lmFit <- lm(y ~ poly(t, options$poly))
    # y <- lmFit$residuals
  }
  # add covariates at some point
  return(data.frame(y, t))
}

.tsTimeSeriesPlot <- function(jaspResults, dataset, options, ready, position, dependencies) {
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

.tsCreateTableStationarityTests <- function(jaspResults, fit, dataset, options, ready, position) {
  # makeTable <- options$adfTest | options$ppTest | options$kpssTest
  if (!is.null(jaspResults[["stationaryTable"]]) & (options$adfTest | options$ppTest | options$kpssTest)) return()

  stationaryTable <- createJaspTable("Stationarity Tests")
  stationaryTable$dependOn(c("adfTest", "ppTest", "kpssTest", "kpssNull"))
  stationaryTable$position <- position
  stationaryTable$showSpecifiedColumnsOnly <- TRUE

  stationaryTable$addColumnInfo(name = "test",      title = gettext("Test"),                      type = "string")
  stationaryTable$addColumnInfo(name = "statistic", title = gettext("Statistic"),                 type = "number")
  # stationaryTable$addColumnInfo(name = "estimate",  title = gettext("Estimate"),                  type = "number")
  stationaryTable$addColumnInfo(name = "lag",       title = gettext("Truncation lag parameter"),  type = "integer")
  # stationaryTable$addColumnInfo(name = "p",         title = gettext("p"),                         type = "pvalue",  format = "p:.0101")
  stationaryTable$addColumnInfo(name = "p",         title = gettext("p"),                         type = "pvalue")
  stationaryTable$addColumnInfo(name = "null",      title = sprintf("H\u2080"),                   type = "string")
  # stationaryTable$setExpectedSize(2)

  jaspResults[["stationaryTable"]] <- stationaryTable

  # Check if ready
  if(!ready) {
    rows <- data.frame(test = ".",
                       statistic = ".",
                      #  estimate = ".",
                       lag = ".",
                       p = ".",
                       null = ".")
    row.names(rows) <- paste0("row", 1)
    stationaryTable$addRows(rows)
    return()
  }

  # statistic <- lag <- p <- vector()
  
  smallerA <- smallerP <- smallerK <- greaterA <- greaterP <- greaterK <- F
  # rowNames <- NULL
  # dfA <- dfP <- dfK <- NULL
  if (options$adfTest) {
    # a <- tseries::adf.test(dataset$y)
    # alternative stationary
    # rowNames <- c(rowNames, "adf")
    # rowNames <- c(rowNames, "row1")
    fit <- tseries::adf.test(dataset$y)
    smallerA <- fit$p.value == 0.01
    greaterA <- fit$p.value == 0.99
    dfA <- .stationarityRows(fit, "Augmented Dickey-Fuller t", gettext("Non-stationary"))
    # p value 0.01 - 0.99
    stationaryTable$addRows(dfA)
    stationaryTable$setRowName(rowIndex = 1, newName = "adf")
  }
  if (options$ppTest) {
    # type = c("Z(alpha)", "Z(t_alpha)")
    # alternative stationary
    # rho normalized bias test (regression coefficient) vs. tau studentized test
    # rowNames <- c(rowNames, "pp")
    # rowNames <- c(rowNames, "row2")
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
  if (options$kpssTest) {
    # null is stationary
    # null = c("Level", "Trend")
    # rowNames <- c(rowNames, "kpss")
    # rowNames <- c(rowNames, "row3")
    fit <- tseries::kpss.test(dataset$y, null = options$kpssNull)
    smallerK <- fit$p.value == 0.01
    greaterK <- fit$p.value == 0.1
    dfK <- .stationarityRows(fit,
                             sprintf("Kwiatkowski-Phillips-Schmidt-Shin %s %s", options$kpssNull, "\u03B7"),
                             gettextf("%s stationary", options$kpssNull))
                             
    stationaryTable$addRows(dfK)
    idxK <- ifelse(options$adfTest & options$ppTest, 3, ifelse(options$adfTest | options$ppTest, 2, 1))
    stationaryTable$setRowName(rowIndex = idxK, newName = "kpss")

                              
    # p value 0.1 - 0.01
  }
  # p <- tseries::pp.test(dataset$y)
  # k <- tseries::kpss.test(dataset$y)

  # DF t, PP Z(alpha hat) and Z(t_alpha hat), KPSS level/trend eta
  # test <- c("Augmented Dickey-Fuller t", "Phillips-Perron", "Kwiatkowski-Phillips-Schmidt-Shin")
  # statistic <- c(a$statistic, p$statistic, k$statistic)
  # estimate <- c(a$statistic, p$statistic, k$statistic)
  # lag <- c(a$parameter, p$parameter, k$parameter)
  # p <- c(a$p.value, p$p.value, k$p.value)

  # rows <- data.frame(test = test,
  #                    statistic = statistic,
  #                   #  estimate = estimate,
  #                    lag = lag,
  #                    p = p)
  # if (options$adfTest | options$ppTest | options$kpssTest) {
  #   rows <- rbind(dfA, dfP, dfK)
  #   row.names(rows) <-  rowNames
  #   stationaryTable$addRows(rows)
  #   # stationaryTable$addRows(rows, rowNames = rowNames)

  #   # stationaryTable$addRows(rows, rowNames = c("row1", "row2", "row3"))
  #   # stationaryTable$$addFootnote(gettext("The p-values are interpolated from tables of critical values, and are not precise if the computed statistic is outside the table (see Help file)."))
  # }

  # Depends on which rows are in the table
  # if (smallerA | smallerK | smallerP) {
  #   footRow <- NULL
  #   if (options$adfTest & smallerA) footRow <- c(footRow, "adf")
  #   if (options$ppTest & smallerP) footRow <- c(footRow, "pp")
  #   if (options$kpssTest & smallerK) footRow <- c(footRow, "kpss")
  #   # if (options$adfTest & smallerA) footRow <- c(footRow, "row1")
  #   # if (options$ppTest & smallerP) footRow <- c(footRow, "row2")
  #   # if (options$kpssTest & smallerK) footRow <- c(footRow, "row3")
  #   stationaryTable$addFootnote(gettextf("The p-value is actually smaller than p-value shown (see Help file)."), colNames = "p", rowNames = footRow)
  # }

  # stationaryTable$addFootnote(gettextf("The p-value is actually greater than shown p-value (see Help file)."), rowNames = , colNames = "p")

  # txt <- gettextf("The p-value is actually %s than p-value shown (see Help file).", greaterOrSmaller)
  if (smallerA | smallerK | smallerP)
    stationaryTable$addFootnote(gettext("The p-value is actually smaller than p-value shown (see Help file)."), 
                                colNames = "p", rowNames = .stationaryFootnoteRows(smallerA, smallerP, smallerK, options))

  if (greaterA | greaterP | greaterK)                            
    stationaryTable$addFootnote(gettext("The p-value is actually greater than p-value shown (see Help file)."), 
                                colNames = "p", rowNames = .stationaryFootnoteRows(greaterA, greaterP, greaterK, options))

}

.stationaryFootnoteRows <- function(A, P, K, options) {
  footRow <- NULL
  if (options$adfTest & A)  footRow <- c(footRow, "adf")
  if (options$ppTest & P)   footRow <- c(footRow, "pp")
  if (options$kpssTest & K) footRow <- c(footRow, "kpss")
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

.tsResults <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return()

  if (options$addSeasonal) {
    P <- options$P
    D <- options$D
    Q <- options$Q
    if (options$period == "specifyPeriod")  m <- options$m
    if (options$period == "findPeriod")     m <- forecast::findfrequency(dataset$y)
  } else {
    m <- 1
    P <- D <- Q <- 0
    # seasonOrder <- list(order = c(0, 0, 0), period = m)
  }
  # yName <- options$dependentVariable[1]
  y <- ts(dataset$y, frequency = m)
  seasonOrder <- list(order = c(P, D, Q), period = m)

  # if(options$best) fit <- auto.arima(y, ic = options$ic)

  if (options$model == "manual") {
    # p   <- ifelse(options$ar, options$p, 0)
    # d   <- ifelse(options$i,  options$d, 0)
    # q   <- ifelse(options$ma, options$q, 0)
    # fit <- arima(y, order = c(options$p, options$d, options$q))
      fit <- forecast::Arima(y, include.constant = options$addConstant,
                             order = c(options$p, options$d, options$q),
                             seasonal = seasonOrder)
  }

  if (options$model == "best") fit <- forecast::auto.arima(y, allowdrift = options$addConstant, allowmean = options$addConstant)

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
  P <- fit$arma[3]
  D <- fit$arma[7]
  Q <- fit$arma[4]
  m <- fit$arma[5]

  estimate  <- fit$coef
  SE        <- sqrt(diag(fit$var.coef))

  coefficients <- character()
  group <- logical()

  if(options$addConstant & d < 2 & D < 2 & (p + q) != length(estimate)) {
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

  if(P >= 1) {
    sar <- sprintf("seasonal AR(%d)", 1:P)
    coefficients <- c(coefficients, sar)
    group <- c(group, T, rep(F, P - 1))
  }

  if(Q >= 1) {
    sma <- sprintf("seasonal MA(%d)", 1:Q)
    coefficients <- c(coefficients, sma)
    group <- c(group, T, rep(F, Q - 1))
  }

  rows <- data.frame(coefficients = coefficients,
                     estimate = estimate,
                     SE = SE,
                     .isNewGroup = group)
  row.names(rows) <- paste0("row", 1:length(coefficients))
  coefTable$addRows(rows)

  coefTable$addFootnote(gettextf("An ARIMA(%s, %s, %s)(%s, %s, %s)[%s] model was fitted.", p, d, q, P, D, Q, m))
}

.tsResidualDiagnostics <- function(jaspResults, fit, dataset, options, ready, position, dependencies){
  # if (!options[["residualPlots"]])
  #   return()

  residContainer <- createJaspContainer(title = gettext("Residual Diagnostics Plots"))
  residContainer$dependOn(c(dependencies, "residualPlots"))
  jaspResults[["residContainer"]] <- residContainer
  jaspResults[["residContainer"]]$position <- position

  if (!ready | !options[["residualPlots"]]) {
    return()
  }

  dataset$y <- residuals(fit)
  # stdres <- rs/sqrt(fitit$sigma2)

  if (is.null(residContainer[["residualTimeSeriesPlot"]]) & options$residualTs) {
    residualTimeSeriesPlot <- createJaspPlot(title = "Time Series Plot", width = 660)
    residualTimeSeriesPlot$dependOn(c("residualTs", "residualTsType", "residualsDistribution"))
    residualTimeSeriesPlot$position <- 1
    residContainer[["residualTimeSeriesPlot"]] <- residualTimeSeriesPlot

    if (!ready)
      return()

    .tsFillTimeSeriesPlot(residualTimeSeriesPlot, dataset, options, type = options$residualTsType, distribution = options$residualsDistribution, yName = "Standardized Residuals")
  }

  if (is.null(residContainer[["residualACFPlot"]]) & options$residualACF) {
    residualACFPlot <- createJaspPlot(title = "Autocorrelation Function Plot")
    residualACFPlot$dependOn(c("residualACF", "residualAcfCI", "residualAcfCIValue", "acfMax"))
    residualACFPlot$position <- 2
    residContainer[["residualACFPlot"]] <- residualACFPlot

    if (!ready)
      return()

    .tsFillACF(residualACFPlot, type = "ACF", dataset, options, ci = options$residualAcfCI, ciValue = options$residualAcfCIValue)
  }

  if (is.null(residContainer[["residualQQPlot"]]) & options$residualQQ) {
    residualQQPlot <- createJaspPlot(title = "Q-Q Plot")
    residualQQPlot$dependOn("residualQQ")
    residualQQPlot$position <- 4
    residContainer[["residualQQPlot"]] <- residualQQPlot

    if (!ready)
      return()

    residualQQPlot$plotObject <- jaspGraphs::plotQQnorm(dataset$y, ablineColor = "darkred")
    # this plot from jaspGraphs still uses jaspTheme()
  }

  if (is.null(residContainer[["ljungPlot"]]) & options$residualLB) {
    ljungPlot <- createJaspPlot(title = "Ljung-Box Plot")
    ljungPlot$dependOn(c("residualLB", "acfMax"))
    ljungPlot$position <- 3
    residContainer[["ljungPlot"]] <- ljungPlot

    if (!ready)
      return()

    .tsFillLjungBoxPlot(ljungPlot, dataset, options)
  }
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

.tsFillLjungBoxPlot <- function(ljungPlot, dataset, options) {
  pValues <- numeric(options$acfMax)
  nLags <- 1:options$acfMax
  for (i in nLags) {
    pValues[i] <- stats::Box.test(dataset$y, i, type = "Ljung-Box")$p.value
  }

  p <- jaspGraphs::JASPScatterPlot(nLags, pValues, addSmooth = F, 
                                   plotAbove = "none", plotRight = "none", 
                                   xName = "Lag", yName = expression(italic(p)-value)) 
  p$subplots$mainPlot <- 
    p$subplots$mainPlot + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = length(nLags), y = 0.05, yend = 0.05), 
                                     linetype = "dashed", color = "blue") +
                          ggplot2::ylim(c(0, 1))

  ljungPlot$plotObject <- p
}

.tsForecastPlot <- function(jaspResults, fit, dataset, options, ready, position, dependencies) {
  if (!options$forecastTimeSeries)
    return()
  
  if (is.null(jaspResults[["forecastPlot"]])) {
    plot <- createJaspPlot(title = "Forecast Time Series Plot", width = 660)
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["forecastPlot"]] <- plot

    if (!ready)
      return()

    .tsFillforecastPlot(plot, fit, dataset, options)
  }
}

.tsFillforecastPlot <- function(plot, fit, dataset, options) {
  yName <- options$dependentVariable[1]

  dat <- dataset

  pred <- forecast::forecast(fit, h = options$nForecasts)
  pred <- as.data.frame(pred)
  names(pred) <- c("y", "lb80", "ub80", "lb95", "ub95")
  pred$t <- (nrow(dat) + 1):(nrow(dat)+nrow(pred))
  obs <- data.frame(t = dat$t, y = dat$y)
  fcs <- data.frame(t = pred$t, y = pred$y)
  both <- rbind(obs, fcs)
  cols <- rep(c("black", "blue"), c(nrow(obs), nrow(fcs)))
  idx <- (nrow(obs) + 1):nrow(both)
  if (options$addObserved)
    idx <- 1:nrow(both)

  p <- ggplot2::ggplot()
  p <- p + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lb95, ymax = ub95, x = t), pred, alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lb80, ymax = ub80, x = t), pred, alpha = 0.2) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (options$forecastType != "points") p <- p + jaspGraphs::geom_line(ggplot2::aes(x = t, y = y), data = both[idx, ], color = cols[idx])
  if (options$forecastType != "line") p <- p + jaspGraphs::geom_point(ggplot2::aes(x = t, y = y), data = both[idx, ], color = cols[idx])

  plot$plotObject <- p
}
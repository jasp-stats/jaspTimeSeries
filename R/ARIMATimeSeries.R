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
    
    .tsResidualDiagnostics(jaspResults, fit, dataset, options, ready, position = 3, dependencies = .tsDependencies)
    # .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1)
    
    # .tsACF(jaspResults, dataset, options, ready, position = 3)
}

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
  if (!is.null(jaspResults[["residContainer"]]))
    return()

  residContainer <- createJaspContainer(title = gettext("Residual Diagnostics Plots"))
  residContainer$dependOn(dependencies)
  jaspResults[["residContainer"]] <- residContainer
  jaspResults[["residContainer"]]$position <- position

  if (!ready) {
    return()
  }

  dataset$y <- residuals(fit)

  # if (options$residualTimeSeries) {
    residualTimeSeriesPlot <- createJaspPlot(title = "Time Series Plot", width = 480)
    residualTimeSeriesPlot$dependOn(c("residualTimeSeries", "residualTsType", "residualPoints"))
    residualTimeSeriesPlot$position <- 1
    residContainer[["residualTimeSeriesPlot"]] <- residualTimeSeriesPlot

    if (options$residualTimeSeries) .tsFillTimeSeriesPlot(residualTimeSeriesPlot, dataset, options, type = options$residualTsType, yName = "Standardized Residuals")
  # }
  if (options$residualACF) {
    residualACFPlot <- createJaspPlot(title = "Autocorrelation Function Plot")
    residualACFPlot$dependOn(c("residualACF", "residualAcfCI", "residualAcfCIValue"))
    residualACFPlot$position <- 2
    residContainer[["residualACFPlot"]] <- residualACFPlot

    .tsFillACF(residualACFPlot, type = "ACF", dataset, options, ci = options$residualAcfCI, ciValue = options$residualAcfCIValue)
  }
  if (options$residualDistribution) {
    residualDistribution <- createJaspPlot(title = "Histogram Plot")
    residualDistribution$dependOn(c("residualDistribution", "distPlotDensity", "distPlotRug", "binWidthType", "numberOfBins"))
    residualDistribution$position <- 3
    residContainer[["residualDistribution"]] <- residualDistribution

    .tsFillHist(residualDistribution, dataset, options, 
                rugs = options$distPlotRug, displayDensity = options$distPlotDensity, 
                binWidthType = options$binWidthType, numberOfBins = options$numberOfBins)
  }
  if (options$residualQQ) {
    residualQQPlot <- createJaspPlot(title = "Q-Q Plot")
    residualQQPlot$dependOn("residualQQ")
    residualQQPlot$position <- 4
    residContainer[["residualQQPlot"]] <- residualQQPlot

    residualQQPlot$plotObject <- jaspGraphs::plotQQnorm(dataset$y, ablineColor = "darkred")
    # .tsFillResidualQQPlot(residualQQPlot, dataset, options)
  }
}

.tsFillHist <- function(residualDistribution, dataset, options, rugs, displayDensity, binWidthType, numberOfBins) {

  lwd <- 1
  variable <- dataset$y

  if (binWidthType == "doane") {  # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6*(length(variable) - 2)) / ((length(variable) + 1)*(length(variable) + 3)))
    g1 <- mean(abs(variable)^3)
    k <- 1 + log2(length(variable)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k
  } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
    binWidthType <- 10000
  } else if (binWidthType == "manual") {
    binWidthType <- numberOfBins
  }

  h <- hist(variable, plot = FALSE, breaks = binWidthType)
  # h <- hist(variable, plot = FALSE)

  if (!displayDensity)
    yhigh <- max(h$counts)
  else {
    dens <- density(variable)
    yhigh <- max(max(h$density), max(dens$y))
  }
  ylow <- 0
  xticks <- base::pretty(c(variable, h$breaks), min.n = 3)

  if (!displayDensity) {
    p <-
      jaspGraphs::drawAxis(
        xName = "Residuals", yName = gettext("Counts"), xBreaks = xticks,
        yBreaks = base::pretty(c(0, h$counts)), force = TRUE, xLabels = xticks
      )
  } else {
    p <-
      jaspGraphs::drawAxis(
        xName = "Residuals", yName = gettext("Density"), xBreaks = xticks,
        yBreaks = c(0,  1.05 * yhigh), force = TRUE, yLabels = NULL,
        xLabels = xticks
      )
  }

  if (displayDensity) {
    p <- p +
      ggplot2::geom_histogram(
        data    = data.frame(variable),
        mapping = ggplot2::aes(x = variable, y = ..density..),
        breaks  = h[["breaks"]],
        fill    = "grey",
        col     = "black",
        size    = .7
      ) +
      ggplot2::geom_line(
        data    = data.frame(x = dens$x, y = dens$y),
        mapping = ggplot2::aes(x = x, y = y),
        lwd     = lwd,
        col     = "black"
      )
  } else {
    p <- p +
      ggplot2::geom_histogram(
        data     = data.frame(variable),
        mapping  = ggplot2::aes(x = variable, y = ..count..),
        breaks   = h[["breaks"]],
        fill     = "grey",
        col      = "black",
        size     = .7
      )
  }

  if (rugs)
    p <- p + ggplot2::geom_rug(data = data.frame(variable), mapping = ggplot2::aes(x = variable), sides = "b")

  # JASP theme
  p <- jaspGraphs::themeJasp(p,
                             axisTickWidth = .7,
                             bty = list(type = "n", ldwX = .7, lwdY = 1))
  # TODO: Fix jaspgraphs axis width X vs Y. See @vandenman.

  if (displayDensity)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  residualDistribution$plotObject <- p
  
  return()
}
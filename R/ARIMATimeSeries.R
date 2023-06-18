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
    ready <- options$dependent != ""

    if (ready) {
      rawData <- .tsReadData(jaspResults, dataset, options)
      dataset <- .tsPrepareData(jaspResults, rawData, options)
    }
    
    fit <- .tsResults(jaspResults, dataset, options, ready)

    .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1, dependencies = c("timeSeriesPlot", "timeSeriesPlotType", "dependent", "timeSeriesPlotDistribution"))

    .tsCreateTableModel(jaspResults, fit, dataset, options, ready, position = 3, dependencies = .tsDependencies)

    .tsCreateTableCoefficients(jaspResults, fit, dataset, options, ready, position = 4, dependencies = .tsDependencies)
    
    .tsResidualDiagnostics(jaspResults, fit, dataset, options, ready, position = 5, dependencies = .tsDependencies)

    .tsSaveResiduals(dataset, fit, options, jaspResults, ready, dependencies = c("residualColumn", "residualSavedToData", .tsDependencies))

    if (options$forecastTable)
      .tsCreateTableForecasts(jaspResults, fit, dataset, options, ready, position = 7, dependencies = c(.tsDependencies, "forecast", "forecastLength"))

    if (options$forecastTimeSeries)
      .tsForecastPlot(jaspResults, fit, dataset, options, ready, position = 6, dependencies = c(.tsDependencies, "forecast", "forecastTimeSeries", "forecastTimeSeriesObserved", "forecastTimeSeriesType", "forecastLength"))

    if (options$saveforecast != "")
      .tsSaveForecasts(jaspResults, fit, dataset, options)
}

# data dependencies
.tsDependencies <- c(
  "dependent", "time", "covariates",
  "modelSpecification", "modelSpecificationAutoIc", "auto", "manual", "intercept",
  "p", "d", "q",
  "seasonal", "periodSpecification", "m", "P", "D", "Q"
)

.tsReadData <- function(jaspResults, dataset, options) {
  if (is.null(dataset)) {
    y <- options$dependent[1]
    # t     <- 1:nrow(dataset)

    covariates <- NULL
    if (length(options[["covariates"]]) > 0) {
      covariates <- unlist(options[["covariates"]])
    }
  }  
  return(.readDataSetToEnd(columns.as.numeric = c(y, covariates)))
}

.tsPrepareData <- function(jaspResults, dataset, options) {
  yName <- options$dependent[1]
  y     <- dataset[, yName]
  t     <- 1:nrow(dataset)

  df <- data.frame(y, t)

  if (length(options[["covariates"]]) > 0) {
    covariateNames <- options$covariates
    covariates <- dataset[, covariateNames]
    df <- cbind(df, covariates)
  }

  return(df)
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

    .tsFillTimeSeriesPlot(plot, dataset, options, type = options$timeSeriesPlotType, distribution = options$timeSeriesPlotDistribution)
  }
}

.tsResults <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return()

  if (options$seasonal) {
    P <- options$P
    D <- options$D
    Q <- options$Q
    if (options$periodSpecification == "manual")    m <- options$m
    if (options$periodSpecification == "dominant")  m <- forecast::findfrequency(dataset$y)
  } else {
    m <- 1
    P <- D <- Q <- 0
  }
  y <- ts(dataset$y, frequency = m)
  seasonOrder <- list(order = c(P, D, Q), period = m)

  xreg <- NULL
  if (length(options[["covariates"]]) > 0) {
    covariates <- dataset[, which(names(dataset) != "y" & names(dataset) != "t")]
    xreg <- as.matrix(covariates)
  }

  if (options$modelSpecification == "manual") {
      fit <- forecast::Arima(
        y,
        include.constant = options$intercept,
        order = c(options$p, options$d, options$q),
        xreg = xreg,
        seasonal = seasonOrder
      )
  }

  if (options$modelSpecification == "auto") {
    fit <- forecast::auto.arima(
      y,
      allowdrift = options$intercept,
      allowmean = options$intercept,
      ic = options$modelSpecificationAutoIc,
      xreg = xreg,
      seasonal = options$seasonal
    )
  }

  if (length(fit$coef) == 0)
    .quitAnalysis("No parameters are estimated.")


  return(fit)
}

.tsCreateTableModel <- function(jaspResults, fit, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["modelTable"]])) return()

  table <- createJaspTable("Model Summary")
  table$dependOn(dependencies)
  table$position <- position
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "sigma",   title = "\u03C3\u00B2",             type = "number")
  table$addColumnInfo(name = "ll",      title = gettext("Log-Likelihood"),  type = "number")
  table$addColumnInfo(name = "aicc",    title = gettext("AICc"),            type = "number")
  table$addColumnInfo(name = "aic",     title = gettext("AIC"),             type = "number")
  table$addColumnInfo(name = "bic",     title = gettext("BIC"),             type = "number")

  jaspResults[["modelTable"]] <- table

  # Check if ready
  if(!ready) {
    rows <- data.frame(sigma = ".",
                       ll = ".",
                       aicc = ".",
                       aic = ".", 
                       bic = ".")
    row.names(rows) <- paste0("row", 1)
    table$addRows(rows)
    return()
  }


  rows <- data.frame(sigma = fit$sigma2,
                     ll = fit$loglik,
                     aicc = fit$aicc,
                     aic = fit$aic,
                     bic = fit$bic)
  row.names(rows) <- paste0("row", 1)
  table$addRows(rows)
}
 
.tsCreateTableCoefficients <- function(jaspResults, fit, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["coefTable"]])) return()

  coefTable <- createJaspTable("Coefficients")
  coefTable$dependOn(dependencies)
  coefTable$position <- position
  coefTable$showSpecifiedColumnsOnly <- TRUE

  coefTable$addColumnInfo(name = "coefficients",  title = "",                         type = "string")
  coefTable$addColumnInfo(name = "estimate",      title = gettext("Estimate"),        type = "number")
  coefTable$addColumnInfo(name = "SE",            title = gettext("Standard Error"),  type = "number")
  coefTable$addColumnInfo(name = "t",             title = gettext("t"), type = "number")
  coefTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  overtitle <- gettextf("%s%% CI", 100 * .95)
  coefTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  coefTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  jaspResults[["coefTable"]] <- coefTable

  # Check if ready
  if(!ready) {
    rows <- data.frame(
      coefficients = ".",
      estimate = ".",
      SE = ".",
      t = ".",
      p = ".",
      lower = ".",
      upper = "."
    )
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
  SE <- sqrt(diag(fit$var.coef))
  t <- fit$coef / SE
  df <- fit$nobs - length(estimate)
  p.val <- 2 * (1 - stats::pt(abs(t), df))
  me <- stats::qt(.95 / 2 + 0.5, df = df) * SE
  lower <- estimate - me
  upper <- estimate + me
  coefficients <- character()
  group <- logical()

  # there is no constant if d or D > 1
  if (options$intercept && d < 2 && D < 2 && (p + q) != length(estimate)) {
    group <- TRUE
    coefficients <- gettext("Constant")
    # I want the intercept to be the first row...
    int <- which(names(estimate) == "intercept")
    nint <- which(names(estimate) != "intercept")
    idx <- c(int, nint)
    estimate  <- estimate[idx]
    SE        <- SE[idx]
    t <- t[idx]
    p.val <- p.val[idx]
    lower <- lower[idx]
    upper <- upper[idx]
  }

  if (p >= 1) {
    ar <- sprintf("AR(%d)", 1:p)
    coefficients <- c(coefficients, ar)
    group <- c(group, T, rep(F, p - 1))
  }

  if (q >= 1) {
    ma <- sprintf("MA(%d)", 1:q)
    coefficients <- c(coefficients, ma)
    group <- c(group, T, rep(F, q - 1))
  }

  if (P >= 1) {
    sar <- sprintf("seasonal AR(%d)", 1:P)
    coefficients <- c(coefficients, sar)
    group <- c(group, T, rep(F, P - 1))
  }

  if (Q >= 1) {
    sma <- sprintf("seasonal MA(%d)", 1:Q)
    coefficients <- c(coefficients, sma)
    group <- c(group, T, rep(F, Q - 1))
  }

  if (length(options[["covariates"]]) > 0) {
    xreg <- options$covariates
    coefficients <- c(coefficients, xreg)
    group <- c(group, T, rep(F, length(xreg) - 1))
  }

  rows <- data.frame(coefficients = coefficients,
                     estimate = estimate,
                     SE = SE,
                     t = t,
                     p = p.val,
                     lower = lower,
                     upper = upper,
                     .isNewGroup = group)
  row.names(rows) <- paste0("row", 1:length(coefficients))
  coefTable$addRows(rows)

  if (P >= 1 | D >= 1 | Q >= 1) {
    coefTable$addFootnote(gettextf("An ARIMA(%s, %s, %s)(%s, %s, %s)[%s] model was fitted.", p, d, q, P, D, Q, m))
  } else {
    coefTable$addFootnote(gettextf("An ARIMA(%s, %s, %s) model was fitted.", p, d, q))
  }
}

.tsResidualDiagnostics <- function(jaspResults, fit, dataset, options, ready, position, dependencies){
  residContainer <- createJaspContainer(title = gettext("Residual Diagnostics Plots"))
  residContainer$dependOn(c(dependencies, "residualPlots"))
  jaspResults[["residContainer"]] <- residContainer
  jaspResults[["residContainer"]]$position <- position

  if (!ready | !options[["residualPlots"]]) {
    return()
  }

  dataset$y <- residuals(fit)

  if (is.null(residContainer[["residualTimeSeriesPlot"]]) & options$residualTimeSeries) {
    residualTimeSeriesPlot <- createJaspPlot(title = "Time Series Plot", width = 660)
    residualTimeSeriesPlot$dependOn(c("residualTimeSeries", "residualTimeSeriesType", "residualTimeSeriesDistribution"))
    residualTimeSeriesPlot$position <- 1
    residContainer[["residualTimeSeriesPlot"]] <- residualTimeSeriesPlot

    if (!ready)
      return()

    .tsFillTimeSeriesPlot(residualTimeSeriesPlot, dataset, options, type = options$residualTimeSeriesType, distribution = options$residualTimeSeriesDistribution, yName = "Standardized Residuals")
  }

  if (is.null(residContainer[["residualACFPlot"]]) & options$residualAcf) {
    residualACFPlot <- createJaspPlot(title = "Autocorrelation Function Plot")
    residualACFPlot$dependOn(c("residualAcf", "residualAcfCi", "residualAcfCiLevel", "residualAcfZeroLag", "residualMaxLag"))
    residualACFPlot$position <- 2
    residContainer[["residualACFPlot"]] <- residualACFPlot

    if (!ready)
      return()

    .tsFillACF(residualACFPlot, type = "ACF", dataset, options, 
      firstLag = options$residualAcfZeroLag, maxLag = options$residualMaxLag,
      ci = options$residualAcfCi, ciValue = options$residualAcfCiLevel,
      ciType = "normal"
    )
  }

  if (is.null(residContainer[["residualQQPlot"]]) & options$residualQQ) {
    residualQQPlot <- createJaspPlot(title = "Q-Q Plot")
    residualQQPlot$dependOn("residualQQ")
    residualQQPlot$position <- 4
    residContainer[["residualQQPlot"]] <- residualQQPlot

    if (!ready)
      return()

    residualQQPlot$plotObject <- jaspGraphs::plotQQnorm(dataset$y, ablineColor = "darkred")
  }

  if (is.null(residContainer[["ljungPlot"]]) & options$residualLjungBox) {
    ljungPlot <- createJaspPlot(title = "Ljung-Box Plot")
    ljungPlot$dependOn(c("residualLjungBox", "residualMaxLag", "ljungBoxSignificanceLevel"))
    ljungPlot$position <- 3
    residContainer[["ljungPlot"]] <- ljungPlot

    if (!ready)
      return()

    .tsFillLjungBoxPlot(ljungPlot, dataset, options)
  }
}

.tsSaveResiduals <- function(dataset, fit, options, jaspResults, ready, dependencies) {
  if (options[["residualSavedToData"]] && is.null(jaspResults[["residualColumn"]]) && options[["residualColumn"]] != "" && ready) {
    residualColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    residualColumn[as.numeric(rownames(dataset))] <- fit$residuals
    jaspResults[["residualColumn"]] <- createJaspColumn(columnName = options[["residualColumn"]])
    jaspResults[["residualColumn"]]$dependOn(options = dependencies)
    jaspResults[["residualColumn"]]$setScale(residualColumn)
  }
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

.tsFillLjungBoxPlot <- function(ljungPlot, dataset, options) {
  pValues <- numeric(options$residualMaxLag)
  nLags <- 1:options$residualMaxLag
  for (i in nLags) {
    pValues[i] <- stats::Box.test(dataset$y, i, type = "Ljung-Box")$p.value
  }

  df <- data.frame(x = nLags, y = pValues)

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(nLags)

  sig <- options$ljungBoxSignificanceLevel

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(name = "Lag", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = "p-value", breaks = yBreaks, limits = range(yBreaks))

  p <- p + 
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = length(nLags), y = sig, yend = sig), alpha = 0.5) +
    jaspGraphs::geom_point(ggplot2::aes(x = x, y = y), data = df) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

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

.tsForecasts <- function(fit, dataset, options) {
  pred <- data.frame(t = (nrow(dataset) + 1):(nrow(dataset) + options$forecastLength))
  pred <- cbind(
    pred,
    as.data.frame(forecast::forecast(fit, h = options$forecastLength))
  )
  yName <- options$dependent[1]
  names(pred) <- c("t", "y", "lower80", "upper80", "lower95", "upper95")
  return(pred)
}

.tsFillforecastPlot <- function(plot, fit, dataset, options) {
  yName <- options$dependent[1]

  pred <- .tsForecasts(fit, dataset, options)
  obs <- data.frame(t = dataset$t, y = dataset$y)
  fcs <- data.frame(t = pred$t, y = pred$y)
  both <- rbind(obs, fcs)
  cols <- rep(c("black", "blue"), c(nrow(obs), nrow(fcs)))
  idx <- (nrow(obs) + 1):nrow(both)
  if (options$forecastTimeSeriesObserved)
    idx <- 1:nrow(both)

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(both[idx, "y"], pred$lower95, pred$upper95))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(both[idx, "t"])

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(name = "t", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))

  p <- p + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower95, ymax = upper95, x = t), pred, alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower80, ymax = upper80, x = t), pred, alpha = 0.2) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (options$forecastTimeSeriesType != "points") p <- p + jaspGraphs::geom_line(ggplot2::aes(x = t, y = y), data = both[idx, ], color = cols[idx])
  if (options$forecastTimeSeriesType != "line") p <- p + jaspGraphs::geom_point(ggplot2::aes(x = t, y = y), data = both[idx, ], color = cols[idx])

  plot$plotObject <- p
}

.tsSaveForecasts <- function(jaspResults, fit, dataset, options) {
  yName <- options$dependent[1]
  forecasts <- .tsForecasts(fit, dataset, options)
  names(forecasts) <- c("t", yName, "lower80", "upper80", "lower95", "upper95")
  utils::write.csv(forecasts, file = options$saveforecast, row.names = FALSE)
}

.tsCreateTableForecasts <- function(jaspResults, fit, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["forecastTable"]])) return()
  
  table <- createJaspTable("Forecasts")
  table$dependOn(dependencies)
  table$position <- position
  yName <- options$dependent[1]
  table$addColumnInfo(name = "t",  title = "t",            type = "string")
  table$addColumnInfo(name = "y",  title = yName,          type = "number")
  overtitle80 <- gettextf("%s%% CI", 100 * .80)
  table$addColumnInfo(name = "lower80", title = gettext("Lower"), type = "number", overtitle = overtitle80)
  table$addColumnInfo(name = "upper80", title = gettext("Upper"), type = "number", overtitle = overtitle80)
  overtitle95 <- gettextf("%s%% CI", 100 * .95)
  table$addColumnInfo(name = "lower95", title = gettext("Lower"), type = "number", overtitle = overtitle95)
  table$addColumnInfo(name = "upper95", title = gettext("Upper"), type = "number", overtitle = overtitle95)

  jaspResults[["forecastTable"]] <- table

  # Check if ready
  if(!ready) {
    rows <- data.frame(
      t = ".",
      y = ".",
      lower80 = ".",
      upper80 = ".",
      lower95 = ".",
      upper95 = "."
    )
    row.names(rows) <- paste0("row", 1)
    table$addRows(rows)
    return()
  }

  forecasts <- .tsForecasts(fit, dataset, options)

  rows <- data.frame(t = forecasts$t,
                     y = forecasts$y,
                     lower80 = forecasts$lower80,
                     upper80 = forecasts$upper80,
                     lower95 = forecasts$lower95,
                     upper95 = forecasts$upper95)
  row.names(rows) <- paste0("row", 1:nrow(forecasts))
  table$addRows(rows)
}

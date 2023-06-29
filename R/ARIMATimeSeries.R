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

  datasetRaw <- .tsReadData(jaspResults, dataset, options, ready, covariates = TRUE)

  datasetFiltered <- .tsDataFilterHandler(datasetRaw, options, ready)

  dataset <- .tsDataWithMissingRowsHandler(datasetFiltered, options, ready)

  .tsArimaResults(jaspResults, dataset, options, ready, dependencies = c(.tsArimaDependencies(), .tsDataDependencies()))
  fit <- jaspResults[["arimaResult"]]$object

  .tsErrorHandler(dataset, ready)

  .tsTimeSeriesPlot(jaspResults, dataset, options, ready, position = 1, dependencies = c(.tsDataDependencies(), "timeSeriesPlot", "timeSeriesPlotType", "timeSeriesPlotDistribution"))

  .tsCreateTableModel(jaspResults, fit, dataset, options, ready, position = 3, dependencies = c(.tsArimaDependencies(), .tsDataDependencies()))

  .tsCreateTableCoefficients(jaspResults, fit, dataset, options, ready, position = 4, dependencies = c(.tsArimaDependencies(), .tsDataDependencies()))

  .tsResidualDiagnostics(jaspResults, fit, dataset, options, ready, position = 5, dependencies = c(.tsArimaDependencies(), .tsDataDependencies()))

  .tsSaveResiduals(dataset, datasetRaw, fit, options, jaspResults, ready, dependencies = c(.tsArimaDependencies(), .tsDataDependencies(), "residualColumn", "residualSavedToData"))

  .tsCreateTableForecasts(jaspResults, fit, dataset, datasetRaw, options, ready, position = 7, dependencies = c(.tsArimaDependencies(), .tsDataDependencies(), "forecast", "forecastLength"))

  .tsForecastPlot(jaspResults, fit, dataset, datasetRaw, options, ready, position = 6, dependencies = c(.tsArimaDependencies(), .tsDataDependencies(), "forecast", "forecastTimeSeries", "forecastTimeSeriesObserved", "forecastTimeSeriesType", "forecastLength"))

  .tsSaveForecasts(jaspResults, fit, dataset, datasetRaw, options, ready)
}

# dependencies
.tsDataDependencies <- function() {
  return(c(
    "dependent", "time", "covariates",
    "filter", "filterBy", "rowStart", "rowEnd", "timeStart", "timeEnd", "dateStart", "dateEnd"
  ))
}

.tsArimaDependencies <- function() {
  return(c(
    "modelSpecification", "modelSpecificationAutoIc", "intercept",
    "p", "d", "q",
    "seasonal", "periodSpecification", "m", "P", "D", "Q"
  ))
}

.tsTimeSeriesPlot <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!options$timeSeriesPlot) {
    return()
  }

  if (is.null(jaspResults[["timeSeriesPlot"]])) {
    plot <- createJaspPlot(title = gettext("Time Series Plot"), width = 660)
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["timeSeriesPlot"]] <- plot

    if (!ready) {
      return()
    }

    .tsFillTimeSeriesPlot(plot, dataset, options, type = options$timeSeriesPlotType, distribution = options$timeSeriesPlotDistribution)
  }
}

.tsArimaResults <- function(jaspResults, dataset, options, ready, dependencies) {
  if (!is.null(jaspResults[["arimaResult"]])) {
    return()
  }

  if (ready) {
    if (options$seasonal) {
      P <- options$P
      D <- options$D
      Q <- options$Q
      if (options$periodSpecification == "custom") m <- options$m
      if (options$periodSpecification == "dominant") m <- forecast::findfrequency(dataset$y)
    } else {
      m <- 1
      P <- D <- Q <- 0
    }
    y <- ts(dataset$y, frequency = m)
    seasonOrder <- list(order = c(P, D, Q), period = m)

    xreg <- NULL
    if (length(options[["covariates"]]) > 0) {
      covariates <- dataset[, grepl("xreg", names(dataset))]
      xreg <- as.matrix(covariates)
    }

    if (options$modelSpecification == "custom") {
      fit <- try(forecast::Arima(
        y,
        include.constant = options$intercept,
        order = c(options$p, options$d, options$q),
        xreg = xreg,
        seasonal = seasonOrder
      ))
    }

    if (options$modelSpecification == "auto") {
      fit <- try(forecast::auto.arima(
        y,
        allowdrift = options$intercept,
        allowmean = options$intercept,
        ic = options$modelSpecificationAutoIc,
        xreg = xreg,
        seasonal = options$seasonal
      ))
    }

    if (jaspBase::isTryError(fit)) .quitAnalysis(gettext("The ARIMA model could not be fit."))
    if (length(fit$coef) == 0) {
      .quitAnalysis(gettext("No parameters are estimated."))
    }

    jaspResults[["arimaResult"]] <- createJaspState(fit)
    jaspResults[["arimaResult"]]$dependOn(dependencies)
  }
}

.tsCreateTableModel <- function(jaspResults, fit, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["modelTable"]])) {
    return()
  }

  table <- createJaspTable(gettext(gettext("Model Summary")))
  table$dependOn(dependencies)
  table$position <- position
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "sigma", title = "\u03C3\u00B2", type = "number")
  table$addColumnInfo(name = "ll", title = gettext("Log-Likelihood"), type = "number")
  table$addColumnInfo(name = "aicc", title = gettext("AICc"), type = "number")
  table$addColumnInfo(name = "aic", title = gettext("AIC"), type = "number")
  table$addColumnInfo(name = "bic", title = gettext("BIC"), type = "number")

  jaspResults[["modelTable"]] <- table

  # Check if ready
  if (!ready) {
    rows <- data.frame(
      sigma = ".",
      ll = ".",
      aicc = ".",
      aic = ".",
      bic = "."
    )
    row.names(rows) <- paste0("row", 1)
    table$addRows(rows)
    return()
  }


  rows <- data.frame(
    sigma = fit$sigma2,
    ll = fit$loglik,
    aicc = fit$aicc,
    aic = fit$aic,
    bic = fit$bic
  )
  row.names(rows) <- paste0("row", 1)
  table$addRows(rows)
}

.tsCreateTableCoefficients <- function(jaspResults, fit, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["coefTable"]])) {
    return()
  }

  coefTable <- createJaspTable(gettext("Coefficients"))
  coefTable$dependOn(dependencies)
  coefTable$position <- position
  coefTable$showSpecifiedColumnsOnly <- TRUE

  coefTable$addColumnInfo(name = "coefficients", title = "", type = "string")
  coefTable$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  coefTable$addColumnInfo(name = "SE", title = gettext("Standard Error"), type = "number")
  coefTable$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  coefTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  overtitle <- gettextf("%s%% CI", 100 * .95)
  coefTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  coefTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  jaspResults[["coefTable"]] <- coefTable

  # Check if ready
  if (!ready) {
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

  # fit$arma order
  # p q P Q f d D
  # f is frequency (or period) of time series
  p <- fit$arma[1]
  d <- fit$arma[6]
  q <- fit$arma[2]
  P <- fit$arma[3]
  D <- fit$arma[7]
  Q <- fit$arma[4]
  m <- fit$arma[5]

  estimate <- fit$coef
  SE <- sqrt(diag(fit$var.coef))
  t <- fit$coef / SE
  df <- fit$nobs - length(estimate)
  p.val <- 2 * (1 - stats::pt(abs(t), df))
  me <- stats::qt(.95 / 2 + 0.5, df = df) * SE
  lower <- estimate - me
  upper <- estimate + me
  coefficients <- character()
  group <- logical()

  # if intercept and/or drift is in included
  hasIntercept <- any(names(fit$coef) == "intercept")
  hasDrift <- any(names(fit$coef) == "drift")
  if (hasIntercept | hasDrift) {
    if (options$intercept && hasIntercept) {
      group <- TRUE
      coefficients <- gettext("Intercept")

      # I want the intercept to be the first row...
      idxInt <- which(names(estimate) == "intercept")
      idxNotInt <- which(names(estimate) != "intercept")
      idx <- c(idxInt, idxNotInt)
    }

    if (hasDrift) {
      coefficients <- c(coefficients, "Drift")
      group <- c(group, TRUE)

      # Drift should be on the second row if intercept, otherwise first row
      idxDrift <- which(names(estimate) == "drift")
      idxNotDrift <- which(names(estimate) != "drift")
      idx <- c(idxDrift, idxNotDrift)

      if (hasIntercept) {
        idxNotDrift <- which(names(estimate) != "drift" & names(estimate) != "intercept")
        idx <- c(idxInt, idxDrift, idxNotDrift)
      }
    }
    estimate <- estimate[idx]
    SE <- SE[idx]
    t <- t[idx]
    p.val <- p.val[idx]
    lower <- lower[idx]
    upper <- upper[idx]
  }

  # add coefficients to table
  if (p >= 1) {
    ar <- gettextf("AR(%d$1)", 1:p)
    coefficients <- c(coefficients, ar)
    group <- c(group, TRUE, rep(FALSE, p - 1))
  }

  if (q >= 1) {
    ma <- gettextf("MA(%d$1)", 1:q)
    coefficients <- c(coefficients, ma)
    group <- c(group, TRUE, rep(FALSE, q - 1))
  }

  if (P >= 1) {
    sar <- gettextf("seasonal AR(%d$1)", 1:P)
    coefficients <- c(coefficients, sar)
    group <- c(group, TRUE, rep(FALSE, P - 1))
  }

  if (Q >= 1) {
    sma <- gettextf("seasonal MA(%d$1)", 1:Q)
    coefficients <- c(coefficients, sma)
    group <- c(group, TRUE, rep(FALSE, Q - 1))
  }

  if (length(options[["covariates"]]) > 0) {
    xreg <- options$covariates
    coefficients <- c(coefficients, xreg)
    group <- c(group, TRUE, rep(FALSE, length(xreg) - 1))
  }

  rows <- data.frame(
    coefficients = coefficients,
    estimate = estimate,
    SE = SE,
    t = t,
    p = p.val,
    lower = lower,
    upper = upper,
    .isNewGroup = group
  )
  row.names(rows) <- paste0("row", 1:length(coefficients))
  coefTable$addRows(rows)

  if (P >= 1 | D >= 1 | Q >= 1) {
    coefTable$addFootnote(gettextf("An ARIMA(%s$1, %s$2, %s$3)(%s$4, %s$5, %s$6)[%s$7] model was fitted.", p, d, q, P, D, Q, m))
  } else {
    coefTable$addFootnote(gettextf("An ARIMA(%s$1, %s$2, %s$3) model was fitted.", p, d, q))
  }
}

.tsResidualDiagnostics <- function(jaspResults, fit, dataset, options, ready, position, dependencies) {
  residContainer <- createJaspContainer(title = gettext("Residual Diagnostics Plots"))
  residContainer$dependOn(dependencies)
  jaspResults[["residContainer"]] <- residContainer
  jaspResults[["residContainer"]]$position <- position

  if (!ready) {
    return()
  }

  dataset$y <- residuals(fit)

  if (is.null(residContainer[["residualTimeSeriesPlot"]]) && options$residualTimeSeries) {
    residualTimeSeriesPlot <- createJaspPlot(title = gettext("Time Series Plot"), width = 660)
    residualTimeSeriesPlot$dependOn(c("residualTimeSeries", "residualTimeSeriesType", "residualTimeSeriesDistribution"))
    residualTimeSeriesPlot$position <- 1
    residContainer[["residualTimeSeriesPlot"]] <- residualTimeSeriesPlot

    if (!ready) {
      return()
    }

    .tsFillTimeSeriesPlot(residualTimeSeriesPlot, dataset, options, type = options$residualTimeSeriesType, distribution = options$residualTimeSeriesDistribution, yName = "Standardized Residuals")
  }

  if (is.null(residContainer[["residualACFPlot"]]) && options$residualAcf) {
    residualACFPlot <- createJaspPlot(title = gettext("Autocorrelation Function Plot"))
    residualACFPlot$dependOn(c("residualAcf", "residualAcfCi", "residualAcfCiLevel", "residualAcfZeroLag", "residualMaxLag"))
    residualACFPlot$position <- 2
    residContainer[["residualACFPlot"]] <- residualACFPlot

    if (!ready) {
      return()
    }

    .tsFillACF(residualACFPlot,
      type = "ACF", dataset, options,
      zeroLag = options$residualAcfZeroLag, maxLag = options$residualMaxLag,
      ci = options$residualAcfCi, ciValue = options$residualAcfCiLevel,
      ciType = "whiteNoise"
    )
  }

  if (is.null(residContainer[["residualQQPlot"]]) && options$residualQQ) {
    residualQQPlot <- createJaspPlot(title = gettext("Q-Q Plot"))
    residualQQPlot$dependOn("residualQQ")
    residualQQPlot$position <- 4
    residContainer[["residualQQPlot"]] <- residualQQPlot

    if (!ready) {
      return()
    }

    residualQQPlot$plotObject <- jaspGraphs::plotQQnorm(dataset$y, ablineColor = "darkred")
  }

  if (is.null(residContainer[["ljungPlot"]]) && options$residualLjungBox) {
    ljungPlot <- createJaspPlot(title = gettext("Ljung-Box Plot"))
    ljungPlot$dependOn(c("residualLjungBox", "residualMaxLag", "ljungBoxSignificanceLevel"))
    ljungPlot$position <- 3
    residContainer[["ljungPlot"]] <- ljungPlot

    if (!ready) {
      return()
    }

    .tsFillLjungBoxPlot(ljungPlot, dataset, options)
  }
}

.tsSaveResiduals <- function(dataset, datasetRaw, fit, options, jaspResults, ready, dependencies) {
  # append residuals to spreadsheet
  if (options[["residualSavedToData"]] && is.null(jaspResults[["residualColumn"]]) && options[["residualColumn"]] != "" && ready) {
    residualColumn <- rep(NA, max(as.numeric(rownames(datasetRaw))))
    matchT <- match(as.POSIXct(datasetRaw$t, tz = "UTC"), dataset$t)
    residualColumn[as.numeric(rownames(datasetRaw))] <- fit$residuals[matchT]
    jaspResults[["residualColumn"]] <- createJaspColumn(columnName = options[["residualColumn"]])
    jaspResults[["residualColumn"]]$dependOn(options = dependencies)
    jaspResults[["residualColumn"]]$setScale(residualColumn)
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
    ggplot2::scale_x_continuous(name = gettext("Lag"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("p-value"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = length(nLags), y = sig, yend = sig), alpha = 0.5) +
    jaspGraphs::geom_point(ggplot2::aes(x = x, y = y), data = df) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  ljungPlot$plotObject <- p
}

.tsForecasts <- function(fit, dataset, datasetRaw, options, jaspResults, ready) {
  if (!is.null(jaspResults[["forecastResult"]])) {
    return()
  }

  if (ready && options$forecastLength > 0) {
    # test if time is a date or a numeric variable
    tryDate <- try(as.POSIXct(dataset$t, tz = "UTC"))

    # get the last non-NA observation
    lastObsY <- max(which(!is.na(dataset$y)))
    lastT <- dataset$t[lastObsY]
    if (jaspBase::isTryError(tryDate)) {
      tPred <- data.frame(t = (lastObsY + 1):(lastObsY + options$forecastLength))
    } else {
      increment <- .tsGuessInterval(dataset)
      # get future dates for forecasts
      tPred <- seq.POSIXt(lastT, by = increment, length.out = (options$forecastLength + 1))
      tPred <- tPred[-1] # remove last value
    }

    xreg <- NULL
    if (length(options[["covariates"]]) > 0) {
      nDependent <- fit$nobs
      covariates <- datasetRaw[, grepl("xreg", names(datasetRaw))]
      # get idx last observation in raw dataset
      # if a filter is used, there may be covariates outside of the filter
      # to use for forecasting
      rawT <- try(as.POSIXct(datasetRaw$t, tz = "UTC"))
      if (jaspBase::isTryError(rawT)) rawT <- datasetRaw$t
      lastRaw <- match(lastT, rawT)
      firstForecast <- lastRaw + 1
      lastForecast <- lastRaw + options$forecastLength
      rangeForecast <- firstForecast:lastForecast
      if (firstForecast > nrow(datasetRaw)) .quitAnalysis(gettext("When 'Covariates' are used in the model, predictions cannot be carried out unless the covariates are also observed for the predicted period."))
      if (length(rangeForecast) > length(firstForecast:nrow(datasetRaw))) {
        .quitAnalysis(
          gettextf(
            "Not enough observations in the covariate%s$1. The maximum number of forecasts is %s$2.",
            ifelse(is.vector(covariates), "", "s"), length(firstForecast:nrow(dataset))
          )
        )
      }
      covariatesForecast <- covariates[rangeForecast]
      xreg <- as.matrix(covariatesForecast)
    }

    tPred <- data.frame(t = tPred)
    pred <- try(as.data.frame(forecast::forecast(fit, h = options$forecastLength, xreg = xreg)))
    if (jaspBase::isTryError(pred)) .quitAnalysis(gettext("Forecasting failed."))
    pred <- cbind(tPred, pred)

    yName <- options$dependent[1]
    names(pred) <- c("t", "y", "lower80", "upper80", "lower95", "upper95")

    jaspResults[["forecastResult"]] <- createJaspState(pred)
    jaspResults[["forecastResult"]]$dependOn(c(.tsDataDependencies(), .tsArimaDependencies(), "forecastLength"))
  }
}

.tsForecastPlot <- function(jaspResults, fit, dataset, datasetRaw, options, ready, position, dependencies) {
  if (!options$forecastTimeSeries) {
    return()
  }

  if (is.null(jaspResults[["forecastPlot"]])) {
    plot <- createJaspPlot(title = gettext("Forecast Time Series Plot"), width = 660)
    plot$dependOn(c(dependencies, "forecastLength"))
    plot$position <- position

    jaspResults[["forecastPlot"]] <- plot

    if (!ready || options$forecastLength == 0) {
      return()
    }

    .tsFillforecastPlot(plot, fit, dataset, datasetRaw, options, jaspResults, ready)
  }
}

.tsFillforecastPlot <- function(plot, fit, dataset, datasetRaw, options, jaspResults, ready) {
  yName <- options$dependent[1]

  .tsForecasts(fit, dataset, datasetRaw, options, jaspResults, ready)
  pred <- jaspResults[["forecastResult"]]$object

  # get the last non-NA observation
  lastObsY <- max(which(!is.na(dataset$y)))

  # get the observations and the forecasts
  obs <- data.frame(t = dataset$t, y = dataset$y)
  obs <- obs[1:lastObsY, ]
  fcs <- data.frame(t = pred$t, y = pred$y)
  df <- rbind(obs, fcs)
  cols <- rep(c("black", "blue"), c(nrow(obs), nrow(fcs)))
  # plot only forecasts or also observations
  idx <- (nrow(obs) + 1):nrow(df)
  if (options$forecastTimeSeriesObserved) {
    idx <- 1:nrow(df)
  }

  cols <- cols[idx]
  df <- df[idx, ]
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(df$y, pred$lower95, pred$upper95))
  tryDate <- try(as.POSIXct(df$t, tz = "UTC"))

  if (jaspBase::isTryError(tryDate)) {
    df$t <- as.numeric(df$t)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$t)
    xScale <- ggplot2::scale_x_continuous("t", breaks = xBreaks, limits = range(xBreaks))
  } else {
    df$t <- as.POSIXct(df$t, tz = "UTC")
    xBreaks <- pretty(df$t)
    xLabels <- attr(xBreaks, "labels")
    xScale <- ggplot2::scale_x_datetime("t", breaks = xBreaks, labels = xLabels, limits = range(xBreaks))
  }

  geomPoint <- if (options$forecastTimeSeriesType == "points" || options$forecastTimeSeriesType == "both") {
    jaspGraphs::geom_point(color = cols)
  } else {
    NULL
  }
  geomLine <- if (options$forecastTimeSeriesType == "line" || options$forecastTimeSeriesType == "both") {
    jaspGraphs::geom_line(color = cols)
  } else {
    NULL
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = y)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower95, ymax = upper95, x = t), pred, alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower80, ymax = upper80, x = t), pred, alpha = 0.2) +
    geomLine +
    geomPoint +
    xScale +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.tsSaveForecasts <- function(jaspResults, fit, dataset, datasetRaw, options, ready) {
  # save forecasts in a seperate .csv file
  # it is not possible to append forecasts to the spreadsheet
  # because that would require adding rows instead of columns
  if (options$forecastSave != "") {
    yName <- decodeColNames(options$dependent[1])

    .tsForecasts(fit, dataset, datasetRaw, options, jaspResults, ready)
    pred <- jaspResults[["forecastResult"]]$object
    names(pred) <- c("t", yName, "lower80", "upper80", "lower95", "upper95")
    utils::write.csv(pred, file = options$forecastSave, row.names = FALSE)
  }
}

.tsCreateTableForecasts <- function(jaspResults, fit, dataset, datasetRaw, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["forecastTable"]]) || !options$forecastTable) {
    return()
  }

  table <- createJaspTable(gettext("Forecasts"))
  table$dependOn(c(dependencies, "forecastLength", "forecastTable"))
  table$position <- position

  yName <- options$dependent[1]
  table$addColumnInfo(name = "t", title = "t", type = "string")
  table$addColumnInfo(name = "y", title = yName, type = "number")
  overtitle80 <- gettextf("%s%% CI", 100 * .80)
  table$addColumnInfo(name = "lower80", title = gettext("Lower"), type = "number", overtitle = overtitle80)
  table$addColumnInfo(name = "upper80", title = gettext("Upper"), type = "number", overtitle = overtitle80)
  overtitle95 <- gettextf("%s%% CI", 100 * .95)
  table$addColumnInfo(name = "lower95", title = gettext("Lower"), type = "number", overtitle = overtitle95)
  table$addColumnInfo(name = "upper95", title = gettext("Upper"), type = "number", overtitle = overtitle95)

  jaspResults[["forecastTable"]] <- table

  # Check if ready
  if (!ready || options$forecastLength == 0) {
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

  .tsForecasts(fit, dataset, datasetRaw, options, jaspResults, ready)
  pred <- jaspResults[["forecastResult"]]$object

  rows <- data.frame(
    t = as.character(pred$t),
    y = pred$y,
    lower80 = pred$lower80,
    upper80 = pred$upper80,
    lower95 = pred$lower95,
    upper95 = pred$upper95
  )
  row.names(rows) <- paste0("row", 1:nrow(pred))
  table$addRows(rows)
}

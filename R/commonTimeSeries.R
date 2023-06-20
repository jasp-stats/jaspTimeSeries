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

.tsReadData <- function(jaspResults, dataset, options, ready, time = TRUE, covariates = FALSE) {
  if (!is.null(dataset))
    return(dataset)
  
  if (ready) {
    yDataset <- .readDataSetToEnd(columns.as.numeric = options$dependent)
    yName <- options$dependent[1]
    y     <- yDataset[, yName]
    dat   <- data.frame(y)
    if (time) {
      if (options$time == "") {
        t <- 1:nrow(yDataset)
      } else {
        tDataset <- .readDataSetToEnd(columns = options$time)
        tName <- options$time[1]
        t <- tDataset[, tName]
      }
      dat <- cbind(dat, t)
    }
    if (covariates) {
      if (length(options[["covariates"]]) > 0) {
        cDataset <- .readDataSetToEnd(columns.as.numeric = options$covariates)
        covariateNames <- options$covariates
        covariates <- as.data.frame(cDataset[, covariateNames])
        names(covariates) <- paste0("xreg", 1:length(covariateNames))
        dat <- cbind(dat, covariates)
      }
    }
    dat <- .tsDataWithMissingRowsHandler(dat)
    return(dat)
  }
}

.tsDataWithMissingRowsHandler <- function(dataset) {
  # Sometimes time series data sets do not have NA's for missing data,
  # but skip rows with a column indicating the time / date
  # so e.g., when third measurement is missing at t = 3,
  # the data set goes from t = 2 on the second row, to t = 4 on the third.
  # This function makes sure these data are seen as missings with NA's.
  tryDate <- try(as.POSIXct(dataset$t, tz = "UTC"))

  if (jaspBase::isTryError(tryDate)) {
    maxT <- max(dataset$t)
    newT <- 1:maxT
  } else {
    dataset$t <- as.POSIXct(dataset$t, tz = "UTC")
    datZoo  <- zoo::zoo(dataset$y, dataset$t)
    datTs   <- as.ts(datZoo)
    newT    <- as.POSIXct(zoo::index(datTs), origin = "1970/01/01")
  }
  dfNewT  <- data.frame(t = newT)
  dat     <- merge(dfNewT, dataset, all.x = TRUE)
  return(dat)
}

.tsFillTimeSeriesPlot <- function(timeSeriesPlot, dataset, options, type, distribution, yName = NULL) {
  if (is.null(yName)) yName <- options$dependent[1]

  dat <- dataset

  p <- .tsJASPScatterPlot(dat$t, dat$y,
    yName = yName, xName = "t",
    addSmooth = FALSE, plotAbove = "none",
    plotRight = distribution,
    type = type
  )

  timeSeriesPlot$plotObject <- p
}

.tsFillACF <- function(plot, type, dataset, options, firstLag = F, maxLag, ci, ciValue, ciType) {
  y <- na.omit(dataset$y)

  if (type == "ACF") {
    ac <- stats::acf(y, plot = FALSE, lag.max = maxLag)
  }
  if (type == "PACF") {
    ac <- stats::pacf(y, plot = FALSE, lag.max = maxLag)
    ciType <- "whiteNoise"
  }

  dat <- data.frame(acf = ac$acf, lag = ac$lag)
  if(type == "ACF" & !firstLag)
    dat <- dat[-1, ] # remove lag 0

  yRange <- dat$acf
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$lag)
  xBreaks <- xBreaks[!xBreaks %% 1] # keep only integers
  xMin <- min(xBreaks)
  xMax <- max(xBreaks)

  p <- ggplot2::ggplot()
  if (ci) {
    if (ciType == "whiteNoise") {
      clim <- qnorm((1 + ciValue) / 2) / sqrt(ac$n.used)
      dat$upper <- rep(clim, nrow(dat))
      dat$lower <- -dat$upper
    } else {
      clim <- .tsAcfBartlett(dat$acf, ac$n.used, ciValue)
      dat$upper <- clim$se
      dat$lower <- -dat$upper
    }

    yRange    <- c(yRange, dat$upper, dat$lower)

    p <- p +
      ggplot2::geom_ribbon(
        data = dat,
        ggplot2::aes(x = lag, ymin = lower, ymax = upper), alpha = 0.15
      )
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)

  p <- p +
    ggplot2::scale_x_continuous(name = "Lag", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = type, breaks = yBreaks, limits = range(yBreaks))

  p <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = acf), size = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = xMin, xend = xMax, y = 0, yend = 0), alpha = 0.5) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
  return()
}

.tsJASPScatterPlot <-  function(x, y, group = NULL, xName = NULL, yName = NULL,
                                addSmooth = TRUE, addSmoothCI = TRUE,
                                smoothCIValue = 0.95, forceLinearSmooth = FALSE,
                                plotAbove = c("density", "histogram", "none"),
                                plotRight = c("density", "histogram", "none"),
                                colorAreaUnderDensity = TRUE,
                                alphaAreaUnderDensity = .5,
                                showLegend = !is.null(group),
                                legendTitle = NULL,
                                emulateGgMarginal = FALSE,
                                type = "both",
                                ...) {

  # TODO: make actual error messages
  stopifnot(
    # is.numeric(x),
    is.numeric(y),
    is.null(group) || is.numeric(group)   || is.factor(group),
    is.null(xName) || is.character(xName) || is.expression(xName),
    is.null(yName) || is.character(yName) || is.expression(yName),
    is.logical(addSmooth),
    is.logical(emulateGgMarginal),
    is.logical(showLegend),
    length(x) == length(y) && (is.null(group) || length(x) == length(group))
  )
  plotRight <- match.arg(plotRight)

  if (emulateGgMarginal)
    colorAreaUnderDensity <- FALSE

  tryDate <- try(as.POSIXct(x, tz = "UTC"))

  if (jaspBase::isTryError(tryDate)) {
    x <- as.numeric(x)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(x)
    xScale <- ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks))
  } else {
    x <- as.POSIXct(x, tz = "UTC")
    xBreaks <- pretty(x)
    xLabels <- attr(xBreaks, "labels")
    xScale <- ggplot2::scale_x_datetime(breaks = xBreaks, labels = xLabels, limits = range(xBreaks))
  }

  df <- data.frame(x = x, y = y)
  mapping <- ggplot2::aes(x = .data$x, y = .data$y)

  geomPoint <- if (type == "points" || type == "both")
    jaspGraphs::geom_point() 
  else NULL
  geomLine <- if (type == "line" || type == "both")
    jaspGraphs::geom_line() 
  else NULL

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  dots <- list(...)
  if (showLegend)
    dots <- quantmod::setDefaults(dots, legend.position = "right")

  mainPlot <- ggplot2::ggplot(df, mapping) +
    geomLine +
    geomPoint +
    ggplot2::labs(x = xName, y = yName, color = legendTitle, fill = legendTitle) +
    xScale +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    do.call(jaspGraphs::themeJaspRaw, dots)

  if (emulateGgMarginal)
    mainPlot <- mainPlot + ggplot2::theme(plot.margin = unit(c(0, 0, 0.25, 0.25), "cm"))

  gb <- ggplot2::ggplot_build(mainPlot)
  scales <- gb$layout$get_scales(1L)
  x.range <- scales$x$get_limits()
  y.range <- scales$y$get_limits()

  rightPlot <- jaspGraphs:::JASPScatterSubPlot(y, group, plotRight, y.range, colorAreaUnderDensity, alphaAreaUnderDensity, flip = TRUE)

  plotList <- list(mainPlot = mainPlot, rightPlot = rightPlot)
  plotList <- plotList[lengths(plotList) > 0L]

  plot <- jaspGraphs:::jaspGraphsPlot$new(
    subplots     = plotList,
    plotFunction = jaspGraphs:::reDrawAlignedPlot,
    size         = 5,
    showLegend   = showLegend
  )
  return(plot)
}

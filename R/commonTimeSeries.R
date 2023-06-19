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

.tsFillTimeSeriesPlot <- function(timeSeriesPlot, dataset, options, type, distribution, yName = NULL) {
  if (is.null(yName)) yName <- options$dependent[1]

  dat <- dataset

  p <- jaspGraphs::JASPScatterPlot(dat$t, dat$y,
    yName = yName, xName = "t",
    addSmooth = FALSE, plotAbove = "none",
    plotRight = distribution
  )

  if (type != "points")  p$subplots$mainPlot$layers[[1]] <- jaspGraphs::geom_line()
  if (type == "both")    p$subplots$mainPlot <- p$subplots$mainPlot + jaspGraphs::geom_point()

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
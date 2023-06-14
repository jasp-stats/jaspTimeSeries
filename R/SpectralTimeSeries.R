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

SpectralTimeSeries <- function(jaspResults, dataset, options) {
    ready <- options$dependent != ""

    if (ready) {
      dataset <- .tsReadDataSpectral(jaspResults, dataset, options)
    }
    
    .tsPowerSpectralDensityDescriptives(jaspResults, dataset, options, ready, position = 6, dependencies = c("powerSpectralDensity", "powerSpectralDensityDetrend", "powerSpectralDensityDemean", "powerSpectralDensitySmoother", "powerSpectralDensitySmootherKernel", "term", "dimension", "powerSpectralDensityTaper", "powerSpectralDensityScaling", "noScaling", "log", "log10", "dependent"))
}

.tsReadDataSpectral <- function(jaspResults, dataset, options) {
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

.tsPowerSpectralDensityDescriptives <- function(jaspResults, dataset, options, ready, position, dependencies){
  # if (!ready)
  #   return()

  if (is.null(jaspResults[["powerSpectralDensity"]])) {
    plot <- createJaspPlot(title = "Power Spectral Density Plot")
    plot$dependOn(dependencies)
    plot$position <- position

    jaspResults[["powerSpectralDensity"]] <- plot

    if (!ready)
      return()

    .tsFillPowerSpectralDensity(plot, dataset, options)
  }
}

.tsGetKernellDimensions <- function(options) {
  dims <- NULL
  for (i in 1:length(options[["term"]])) {
    dims <- c(dims, options[["term"]][[i]]$dimension)
  }
  return(dims) # Vector of kernel dimensions in each term
}

.tsFillPowerSpectralDensity <- function(powerSpectralDensity, dataset, options) {
  y <- na.omit(dataset$y)

  k <- NULL

  dims <- .tsGetKernellDimensions(options)

  # crashes when modified daniell has zeros..
  if (options$powerSpectralDensitySmoother)
    k <- stats::kernel(options$powerSpectralDensitySmootherKernel, dims)

  yPSD <- stats::spec.pgram(y,
    kernel = k,
    taper = options$powerSpectralDensityTaper,
    demean = options$powerSpectralDensityDemean,
    detrend = options$powerSpectralDensityDetrend,
    plot = FALSE
  )

  dat <- data.frame(x = yPSD$freq, y = yPSD$spec)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$y)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) + jaspGraphs::geom_line() +
    ggplot2::scale_x_continuous(name = "Frequency", breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Spectrum", breaks = yBreaks)

  if (options$powerSpectralDensityScaling != "noScaling") {
    logTrans <- options$powerSpectralDensityScaling
    logFunction <- function(x) exp(x)
    logLabels <- scales::math_format(e ^ .x)
    if (logTrans == "log10") {
      logFunction <- function(x) 10 ^ x
      logLabels <- scales::math_format(10 ^ .x)
    }

    p <- p + ggplot2::scale_y_continuous(
      trans = logTrans,
      breaks = scales::trans_breaks(logTrans, logFunction),
      labels = scales::trans_format(logTrans, logLabels)
    )
  }

  p <- p +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  powerSpectralDensity$plotObject <- p

  return()
}



 
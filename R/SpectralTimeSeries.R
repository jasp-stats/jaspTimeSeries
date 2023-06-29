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

  datasetRaw <- .tsReadData(jaspResults, dataset, options, ready)

  datasetFiltered <- .tsDataFilterHandler(datasetRaw, options, ready)

  dataset <- .tsDataWithMissingRowsHandler(datasetFiltered, options, ready)

  .tsErrorHandler(dataset, ready)

  .tsPowerSpectralDensity(jaspResults, dataset, options, ready, position = 2, dependencies = .tsSpectralDependencies())
  .tsCreateTableBandWith(jaspResults, dataset, options, ready, position = 1, dependencies = .tsSpectralDependencies())
}

. <- function() {
  return(c(
    "dependent", "time",
    "kernel", "kernelMethod", "kernelTerm", "kernelDimension",
    "taper", "log", "detrend", "demean",
    "filter", "filterBy", "rowStart", "rowEnd", "timeStart", "timeEnd", "dateStart", "dateEnd"
  ))
}

.tsComputeSpectralResults <- function(dataset, options, jaspResults, ready) {
  if (!is.null(jaspResults[["spectralResult"]])) {
    return()
  }

  if (ready) {
    y <- ts(dataset$y)

    k <- NULL

    dims <- .tsGetKernellDimensions(options)

    if (options$kernel) {
      k <- stats::kernel(options$kernelMethod, dims)
    }

    res <- try(stats::spec.pgram(
      y,
      kernel    = k,
      taper     = options$taper,
      demean    = options$demean,
      detrend   = options$detrend,
      plot      = FALSE,
      na.action = na.exclude # should probably add other options at some point
    ))

    if (jaspBase::isTryError(res)) .quitAnalysis(gettext("The spectral analysis failed."))

    jaspResults[["spectralResult"]] <- createJaspState(res)
    jaspResults[["spectralResult"]]$dependOn(.tsSpectralDependencies())
  }
}

.tsPowerSpectralDensity <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (is.null(jaspResults[["powerSpectralDensity"]])) {
    plot <- createJaspPlot(title = gettext("Power Spectral Density Plot"))
    plot$dependOn(c(dependencies, "whiteNoise", "pinkNoise", "brownNoise"))
    plot$position <- position

    jaspResults[["powerSpectralDensity"]] <- plot

    if (!ready) {
      return()
    }

    .tsFillPowerSpectralDensity(plot, jaspResults, dataset, options, ready)
  }
}

.tsGetKernellDimensions <- function(options) {
  dims <- NULL
  for (i in 1:length(options[["kernelTerm"]])) {
    dims <- c(dims, options[["kernelTerm"]][[i]]$kernelDimension)
  }
  return(dims) # Vector of kernel dimensions in each term
}

.tsFillPowerSpectralDensity <- function(powerSpectralDensity, jaspResults, dataset, options, ready) {
  .tsComputeSpectralResults(dataset, options, jaspResults, ready)
  res <- jaspResults[["spectralResult"]]$object

  x <- res$freq
  y <- res$spec

  whiteNoise <- 1 / x^0
  pinkNoise <- 1 / x
  brownNoise <- 1 / x^2

  dat <- data.frame(x, y, whiteNoise, pinkNoise, brownNoise)

  dat <- as.data.frame(apply(dat, 2, log))

  xName <- "log(Frequency)"
  yName <- "log(Power)"

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$x)
  yRange <- dat$y
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y))

  if (options$whiteNoise) {
    yRange <- c(yRange, dat$whiteNoise)
    p <- p + jaspGraphs::geom_line(ggplot2::aes(x = x, y = whiteNoise), colour = "grey")
  }
  if (options$pinkNoise) {
    yRange <- c(yRange, dat$pinkNoise)
    p <- p + jaspGraphs::geom_line(ggplot2::aes(x = x, y = pinkNoise), colour = "pink")
  }
  if (options$brownNoise) {
    yRange <- c(yRange, dat$brownNoise)
    p <- p + jaspGraphs::geom_line(ggplot2::aes(x = x, y = brownNoise), colour = "brown")
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)


  p <- p + jaspGraphs::geom_line() +
    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  powerSpectralDensity$plotObject <- p

  return()
}

.tsCreateTableBandWith <- function(jaspResults, dataset, options, ready, position, dependencies) {
  if (!is.null(jaspResults[["bandWidthTable"]])) {
    return()
  }

  table <- createJaspTable(gettext("Spectral Density"))
  table$dependOn(dependencies)
  table$position <- position

  table$addColumnInfo(name = "bandwidth", title = gettext("Bandwidth"), type = "number")

  jaspResults[["bandWidthTable"]] <- table

  # Check if ready
  if (!ready) {
    rows <- data.frame(bandwidth = ".")
    row.names(rows) <- paste0("row", 1)
    table$addRows(rows)
    return()
  }

  .tsComputeSpectralResults(dataset, options, jaspResults, ready)
  res <- jaspResults[["spectralResult"]]$object

  rows <- data.frame(bandwidth = res$bandwidth)
  row.names(rows) <- paste0("row", 1)
  table$addRows(rows)
}

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

# When making changes to this file always mention @sophieberkhout as a 
# reviewer in the Pull Request

DescriptivesTimeSeries <- function(jaspResults, dataset, options) {
    ready <- (length(options$dependentVariable) > 0)

    if (ready)
      dataset <- .descriptivesTimeSeriesReadData(jaspResults, dataset, options)

    if (options$timeSeriesPlot)
      .timeSeriesPlotDescriptives(jaspResults, dataset, options, ready)
}

.descriptivesTimeSeriesReadData <- function(jaspResults, dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = options$dependentVariable))
}

.timeSeriesPlotDescriptives <- function(jaspResults, dataset, options, ready) {
  timeSeriesPlot <- createJaspPlot(title = "Time Series")
  timeSeriesPlot$dependOn(c("variables", "timeSeriesPlot"))

  jaspResults[["timeSeriesPlot"]] <- timeSeriesPlot

  if (!ready)
    return()

  .timeSeriesFillPlotDescriptives(timeSeriesPlot, dataset, options)

  return()
}

.timeSeriesFillPlotDescriptives <- function(timeSeriesPlot, dataset, options) {
  y <- dataset[, encodeColNames(options$dependentVariable[1])]
  t <- 1:nrow(dataset)
  # y <- rnorm(10)
  # t <- 1:10
  df <- data.frame(y, t)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(1, t))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  plot <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = y)) + ggplot2::geom_line() +
    ggplot2::scale_x_continuous(name = gettext("t"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = unlist(options$dependentVariable), breaks = yBreaks, limits = range(yBreaks))
  plot <- jaspGraphs::themeJasp(plot)

  timeSeriesPlot$plotObject <- plot

  return()
}

#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Spectral Analysis
#'
#' Spectral analysis allows the user to assess the frequency components and power distribution of a time series.
#'
#' @param dependent, A variable that is measured repeatedly over time.
#' @param filter, Selects an inclusive range of observations for analysis. Row number refers to original spreadsheet rows, even when Time changes the analysis order. Time index filters numeric Time values, including negative values and decimals. Date filters interpreted dates and times. Date filters use UTC. A date-only End includes the entire day; an End with a time uses that exact cutoff. Time or date filtering requires a usable Time variable; otherwise, select Row number or disable filtering.
#'    Defaults to \code{FALSE}.
#' @param kernel, Smooths the power spectral density using a kernel. The dimension determines the convolution of kernels. The taper specifies the proportion of data to which a split cosine bell taper is applied at the beginning and end of the series.
#'    Defaults to \code{FALSE}.
#' @param naAction, Applies only to this calculation. Listwise exclusion removes missing observations and treats the remaining observations as consecutive time steps, ignoring any internal gaps. Use the longest complete segment selects the longest consecutive run without missing values, using the earliest run when tied. When observations are omitted, a note reports the observations used and any effect on time spacing. The original data and saved values are unchanged.
#' \itemize{
#'   \item \code{"listwise"}
#'   \item \code{"longest"}
#' }
#' @param time, Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.
SpectralTimeSeries <- function(
          data = NULL,
          version = "1",
          brownNoise = FALSE,
          dateEnd = "",
          dateStart = "",
          demean = FALSE,
          dependent = list(types = list(), value = ""),
          detrend = FALSE,
          filter = FALSE,
          filterBy = "row",
          kernel = FALSE,
          kernelMethod = "daniell",
          kernelTerm = list(list(value = "#")),
          naAction = "listwise",
          pinkNoise = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          rowEnd = 100,
          rowStart = 1,
          taper = 0,
          time = list(types = list(), value = ""),
          timeEnd = 100,
          timeStart = 1,
          whiteNoise = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dependent", "kernelMethod", "kernelTerm", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspTimeSeries", "SpectralTimeSeries", "SpectralTimeSeries.qml", options, version, TRUE))
}
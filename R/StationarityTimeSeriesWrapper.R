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

#' Stationarity
#'
#' Stationarity allows the user to test a univariate time-series for stationarity and to transform the time-series if necessary.
#'
#' @param acf, Plots the autocorrelation for a specified number of lags. The confidence interval may be given assuming either a white noise process, or assuming for a lag q a moving average process of order q - 1
#'    Defaults to \code{FALSE}.
#' @param acfNaAction, Applies only to this calculation. Listwise exclusion removes missing observations and treats the remaining observations as consecutive time steps, ignoring any internal gaps. Use the longest complete segment selects the longest consecutive run without missing values, using the earliest run when tied. When observations are omitted, a note reports the observations used and any effect on time spacing. The original data and saved values are unchanged.
#' \itemize{
#'   \item \code{"listwise"}
#'   \item \code{"longest"}
#' }
#' @param adfTest, Computes an ADF test that accounts for a constant (drift) and a linear trend, where the null assumes that the time-series has a unit root. A default lag order is used based on T-1^(1/3). The p-values are interpolated from a table of critical values in Banerjee et al. (1993), when the statistic lies outside the range of critical values, a note is added to the table.
#'    Defaults to \code{FALSE}.
#' @param boxCox, Transforms the dependent variable using a Box-Cox transformation. Lambda is the transformation parameter. If lambda is chosen automatically, it minimizes the coefficient of variation for the dependent variable using Guerrero's method.
#'    Defaults to \code{FALSE}.
#' @param dependent, A variable that is measured repeatedly over time.
#' @param detrend, Fits a polynomial regression to the dependent variable using observation step as predictor, and keeps only the residuals. Differences between Time values do not affect detrending; rows with missing dependent values retain their step positions. If best fitting is selected, the polynomial regression is chosen based on the information criterion.
#'    Defaults to \code{FALSE}.
#' @param difference, Differences the dependent variable at the selected lag, repeated according to Order. The first Lag × Order positions are retained as missing values (up to the series length). Existing missing values also propagate into differences that use them. Time series plots and saved transformations retain these positions; stationarity tests and autocorrelation plots handle them according to their own Missing values setting.
#'    Defaults to \code{FALSE}.
#' @param filter, Selects an inclusive range of observations for analysis. Row number refers to original spreadsheet rows, even when Time changes the analysis order. Time index filters numeric Time values, including negative values and decimals. Date filters interpreted dates and times. Date filters use UTC. A date-only End includes the entire day; an End with a time uses that exact cutoff. Time or date filtering requires a usable Time variable; otherwise, select Row number or disable filtering.
#'    Defaults to \code{FALSE}.
#' @param log, Takes the log of the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param pacf, Plots the partial autocorrelation for a specified number of lags.
#'    Defaults to \code{FALSE}.
#' @param pacfNaAction, Applies only to this calculation. Listwise exclusion removes missing observations and treats the remaining observations as consecutive time steps, ignoring any internal gaps. Use the longest complete segment selects the longest consecutive run without missing values, using the earliest run when tied. When observations are omitted, a note reports the observations used and any effect on time spacing. The original data and saved values are unchanged.
#' \itemize{
#'   \item \code{"listwise"}
#'   \item \code{"longest"}
#' }
#' @param root, Takes the root of the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param testNaAction, Applies only to this calculation. Listwise exclusion removes missing observations and treats the remaining observations as consecutive time steps, ignoring any internal gaps. Use the longest complete segment selects the longest consecutive run without missing values, using the earliest run when tied. When observations are omitted, a note reports the observations used and any effect on time spacing. The original data and saved values are unchanged.
#' \itemize{
#'   \item \code{"listwise"}
#'   \item \code{"longest"}
#' }
#' @param time, Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.
#' @param timeSeriesPlot, Plots the (transformed) dependent variable (y-axis) over time (x-axis).
#'    Defaults to \code{TRUE}.
#' @param transformationSavedToData, Appends the transformed dependent variable to the spreadsheet, so these can be used in further analyses.
#'    Defaults to \code{FALSE}.
StationarityTimeSeries <- function(
          data = NULL,
          version = "1",
          acf = FALSE,
          acfCi = TRUE,
          acfCiLevel = 0.95,
          acfCiType = "whiteNoise",
          acfMaxLag = 10,
          acfNaAction = "listwise",
          acfZeroLag = FALSE,
          adfTest = FALSE,
          boxCox = FALSE,
          boxCoxLambda = 0,
          boxCoxLambdaSpecification = "auto",
          dateEnd = "",
          dateStart = "",
          dependent = list(types = list(), value = ""),
          detrend = FALSE,
          detrendPoly = 1,
          difference = FALSE,
          differenceLag = 1,
          differenceOrder = 1,
          filter = FALSE,
          filterBy = "row",
          kpssLevel = FALSE,
          kpssTrend = FALSE,
          log = FALSE,
          logBase = "10",
          pacf = FALSE,
          pacfCi = TRUE,
          pacfCiLevel = 0.95,
          pacfMaxLag = 10,
          pacfNaAction = "listwise",
          plotHeight = 320,
          plotWidth = 480,
          polynomialSpecification = "auto",
          polynomialSpecificationAutoIc = "aic",
          polynomialSpecificationAutoMax = 5,
          ppTestRegressionCoefficient = FALSE,
          ppTestStudentized = FALSE,
          root = FALSE,
          rootIndex = "square",
          rowEnd = 100,
          rowStart = 1,
          testNaAction = "listwise",
          time = list(types = list(), value = ""),
          timeEnd = 100,
          timeSeriesPlot = TRUE,
          timeSeriesPlotDistribution = "none",
          timeSeriesPlotType = "both",
          timeStart = 1,
          transformationColumn = "",
          transformationSavedToData = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dependent", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspTimeSeries", "StationarityTimeSeries", "StationarityTimeSeries.qml", options, version, TRUE))
}
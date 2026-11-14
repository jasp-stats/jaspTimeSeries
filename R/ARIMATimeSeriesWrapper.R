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

#' ARIMA
#'
#' Allows the user to fit a (seasonal) AR/MA/ARMA/ARIMA model to a time series.
#'
#' @param covariates, Optional numeric variables to include as external regressors. Forecasting requires covariate values in the next rows of the ordered dataset after the final row used for fitting. Use a filter to end the fitted series before those rows; missing dependent values alone do not exclude a row from fitting.
#' @param dependent, A variable that is measured repeatedly over time.
#' @param filter, Selects an inclusive range of observations for analysis. Row number refers to original spreadsheet rows, even when Time changes the analysis order. Time index filters numeric Time values, including negative values and decimals. Date filters interpreted dates and times. Date filters use UTC. A date-only End includes the entire day; an End with a time uses that exact cutoff. Time or date filtering requires a usable Time variable; otherwise, select Row number or disable filtering.
#'    Defaults to \code{FALSE}.
#' @param forecastLength, Number of observation steps to forecast after the final row included in the model, including rows with missing dependent values. A value of 0 produces no forecasts. When Time is assigned, the table reports steps ahead; future Time values or dates are not inferred.
#' @param forecastSave, Saves the forecasts in a separate .csv file. When Time is assigned, the first column, stepAhead, counts steps after the final modeled row. When Time is empty, the first column, t, continues the row index.
#' @param forecastTable, Reports forecast means and intervals. With Time assigned, Steps ahead counts from 1 after the final modeled row. With Time empty, t continues the row index.
#'    Defaults to \code{FALSE}.
#' @param forecastTimeSeries, Plots forecasts and optionally observed values. With Time assigned, the x-axis counts observation steps in model order and continues beyond the final modeled row; future Time values or dates are not inferred. With Time empty, t retains and continues the row index.
#'    Defaults to \code{FALSE}.
#' @param intercept, Include an intercept in the model.
#'    Defaults to \code{TRUE}.
#' @param modelSpecification, Specifies what ARIMA model to fit. The best fitting model is determined by the Hyndman-Khandakar algorithm.
#' \itemize{
#'   \item \code{"auto"}
#'   \item \code{"custom"}
#' }
#' @param residualAcf, Plots the autocorrelation for a specified number of lags. The confidence interval may be given assuming a white noise process.
#'    Defaults to \code{FALSE}.
#' @param residualAcfNaAction, Applies only to this calculation. Listwise exclusion removes missing observations and treats the remaining observations as consecutive time steps, ignoring any internal gaps. Use the longest complete segment selects the longest consecutive run without missing values, using the earliest run when tied. When observations are omitted, a note reports the observations used and any effect on time spacing. The original data and saved values are unchanged.
#' \itemize{
#'   \item \code{"listwise"}
#'   \item \code{"longest"}
#' }
#' @param residualLjungBox, Plots the p-values of the Ljung-Box test for a number of lags of which the null hypothesis assumes that the data independently distributed and therefore have no autocorrelation.
#'    Defaults to \code{FALSE}.
#' @param residualLjungBoxNaAction, Applies only to this calculation. Listwise exclusion removes missing observations and treats the remaining observations as consecutive time steps, ignoring any internal gaps. Use the longest complete segment selects the longest consecutive run without missing values, using the earliest run when tied. When observations are omitted, a note reports the observations used and any effect on time spacing. The original data and saved values are unchanged.
#' \itemize{
#'   \item \code{"listwise"}
#'   \item \code{"longest"}
#' }
#' @param residualQQ, Plots the quantile-quantile plot of the residuals.
#'    Defaults to \code{FALSE}.
#' @param residualSavedToData, Appends the residuals to the spreadsheet, so these can be used in further analyses.
#'    Defaults to \code{FALSE}.
#' @param residualTimeSeries, Plots the residuals (y-axis) over time (x-axis).
#'    Defaults to \code{FALSE}.
#' @param seasonal, Fit a seasonal ARIMA model. If the frequency is not known, a dominant frequency may be determined from a spectral analysis.
#'    Defaults to \code{FALSE}.
#' @param time, Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.
#' @param timeSeriesPlot, Plots the dependent variable (y-axis) over time (x-axis).
#'    Defaults to \code{TRUE}.
ARIMATimeSeries <- function(
          data = NULL,
          version = "1",
          D = 0,
          P = 0,
          Q = 0,
          covariates = list(types = list(), value = list()),
          d = 0,
          dateEnd = "",
          dateStart = "",
          dependent = list(types = list(), value = ""),
          filter = FALSE,
          filterBy = "row",
          forecastLength = 0,
          forecastSave = "",
          forecastTable = FALSE,
          forecastTimeSeries = FALSE,
          forecastTimeSeriesObserved = TRUE,
          forecastTimeSeriesType = "both",
          intercept = TRUE,
          ljungBoxSignificanceLevel = 0.05,
          m = 1,
          modelSpecification = "auto",
          modelSpecificationAutoIc = "aicc",
          p = 0,
          periodSpecification = "custom",
          plotHeight = 320,
          plotWidth = 480,
          q = 0,
          residualAcf = FALSE,
          residualAcfCi = TRUE,
          residualAcfCiLevel = 0.95,
          residualAcfNaAction = "listwise",
          residualAcfZeroLag = FALSE,
          residualColumn = "",
          residualLjungBox = FALSE,
          residualLjungBoxNaAction = "listwise",
          residualMaxLag = 10,
          residualQQ = FALSE,
          residualSavedToData = FALSE,
          residualTimeSeries = FALSE,
          residualTimeSeriesDistribution = "none",
          residualTimeSeriesType = "both",
          rowEnd = 100,
          rowStart = 1,
          seasonal = FALSE,
          time = list(types = list(), value = ""),
          timeEnd = 100,
          timeSeriesPlot = TRUE,
          timeSeriesPlotDistribution = "none",
          timeSeriesPlotType = "both",
          timeStart = 1) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("covariates", "dependent", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspTimeSeries", "ARIMATimeSeries", "ARIMATimeSeries.qml", options, version, TRUE))
}
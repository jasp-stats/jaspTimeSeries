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

#' Bayesian Gaussian State Space Models
#'
#' @param aggregatedStateAndErrorPlotsCiLevel, Sets the credible interval for the aggregated state, residual, and one-step-ahead prediction error plots.
#' @param dynamicRegressionComponent, This component allows you to model the effect of covariates or fixed factors on the dependent variable. Only available when covariates or fixed factors are present in the model.
#'    Defaults to \code{FALSE}.
#' @param time, Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.
bayesianStateSpace <- function(
          data = NULL,
          version = "1",
          formula = NULL,
          isNuisance = NULL,
          aggregatedStateAndErrorPlotsCiLevel = 0.95,
          aggregatedStatesPlot = FALSE,
          aggregatedStatesPlotObservationsShown = FALSE,
          autoregressiveComponent = FALSE,
          autoregressiveLagSelectionMethod = "manual",
          autoregressiveLags = 1,
          autoregressiveMaxLags = 1,
          burninAutomaticProportion = 0.1,
          burninManualDraws = 0,
          burninMethod = "auto",
          componentStatesPlot = FALSE,
          controlChartPeriodEnd = 100,
          controlChartPlot = FALSE,
          controlChartProbabilisticPlot = FALSE,
          controlChartSigmaThreshold = 2,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          dynamicRegressionComponent = FALSE,
          expectedPredictors = 1,
          fixedFactors = list(types = list(), value = list()),
          forecastErrorPlot = FALSE,
          localLevelComponent = TRUE,
          localLinearTrendComponent = FALSE,
          mcmcDraws = 2000,
          mcmcTimeoutSeconds = 120,
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          plotHeight = 320,
          plotWidth = 480,
          posteriorSummaryCiLevel = 0.95,
          posteriorSummaryConditionalEstimates = FALSE,
          posteriorSummaryTable = FALSE,
          predictionHorizon = 0,
          residualPlot = FALSE,
          seasonalities = list(),
          seed = 1,
          semiLocalLinearTrendComponent = FALSE,
          time = list(types = list(), value = "")) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }
   optionsWithFormula <- c("isNuisance", "covariates", "dependent", "fixedFactors", "modelTerms", "seasonalities", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspTimeSeries", "bayesianStateSpace", "bayesianStateSpace.qml", options, version, FALSE))
}
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

#' Gaussian State Space Models
#'
#' @param aggregatedStateAndErrorPlotsCiLevel, Sets the credible interval for the aggregated state, residual, and one-step-ahead prediction error plots.
#'    Defaults to \code{0.95}.
#' @param dynamicRegressionComponent, This component allows you to model the effect of covariates or fixed factors on the dependent variable. Only available when covariates or fixed factors are present in the model.
#'    Defaults to \code{FALSE}.
#' @param time, Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.
bayesianStateSpace <- function(
          data = NULL,
          version = "0.96.3",
          formula = NULL,
          isNuisance = NULL,
          aggregatedStatesPlot = FALSE,
          aggregatedStateAndErrorPlotsCiLevel = 0.95,
          aggregatedStatesPlotObservationsShown = FALSE,
          burninAutomaticProportion = 0.1,
          autoregressiveComponent = FALSE,
          burninMethod = "auto",
          componentStatesPlot = FALSE,
          controlChartPlot = FALSE,
          controlChartPeriodEnd = 100,
          controlChartSigmaThreshold = 2,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          dynamicRegressionComponent = FALSE,
          # Ignored legacy argument; retain for syntax compatibility when regenerating.
          dynamicRegregressionLags = 0,
          expectedPredictors = 1,
          fixedFactors = list(types = list(), value = list()),
          forecastErrorPlot = FALSE,
          autoregressiveLagSelectionMethod = "manual",
          autoregressiveLags = 1,
          localLevelComponent = TRUE,
          localLinearTrendComponent = FALSE,
          burninManualDraws = 0,
          autoregressiveMaxLags = 1,
          modelTerms = list(),
          plotHeight = 320,
          plotWidth = 480,
          posteriorSummaryCiLevel = 0.95,
          posteriorSummaryTable = FALSE,
          predictionHorizon = 0,
          controlChartProbabilisticPlot = FALSE,
          residualPlot = FALSE,
          mcmcDraws = 2000,
          seasonalities = list(),
          seed = 1,
          semiLocalLinearTrendComponent = FALSE,
          posteriorSummaryConditionalEstimates = FALSE,
          time = list(types = list(), value = ""),
          mcmcTimeoutSeconds = 120) {

   defaultArgCalls <- formals(jaspTimeSeries::bayesianStateSpace)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
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

   return(jaspBase::runWrappedAnalysis("jaspTimeSeries", "bayesianStateSpace", "bayesianStateSpace.qml", options, version, FALSE))
}

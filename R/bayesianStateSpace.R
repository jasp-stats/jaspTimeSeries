#
# Copyright (C) 2013-2022 University of Amsterdam
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

# Main function ----

bayesianStateSpaceInternal <- function(jaspResults, dataset, options) {


  # check if results can be computed
  ready <- (options$dependent != "" && any(options[c("autoregressiveComponent","localLevelComponent","localLinearTrendComponent","semiLocalLinearTrendComponent")]==TRUE,length(options$seasonalities)>0))
  # Init options: add variables to options to be used in the remainder of the analysis

  # ensure the prediction horizon is 0 whenever there are covariates or fixedFactors
  if (length(options[["covariates"]]) > 0L || length(options[["fixedFactors"]]) > 0L)
    options[["predictionHorizon"]] <- 0L

  # read dataset
  dataset <- .bstsReadData(options,ready)
  # error checking
  .bstsErrorHandling(dataset, options)

  .bstsCreateContainerMain(jaspResults,options,ready)

  # Fit once; burn-in selection only affects derived results.
  .bstsComputeResults(jaspResults, dataset, options,ready)

  # Compute burn amount and pass it to/create options$burn
  options <- .bstsBurnHelper(jaspResults,options)
  .bstsComputePredictions(jaspResults, options, ready)
  options <- .bstsTimeHelper(jaspResults,dataset,options)

  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .bstsCreateModelSummaryTable(jaspResults,options,ready)
  .bstsCreateCoefficientTable(jaspResults,options,ready)
  .bstsCreateConditionalCoefficientTable(jaspResults,options,ready)
  .bstsCreateStatePlots(jaspResults,dataset,options,ready)
  .bstsCreatePredictionPlot(jaspResults,options,ready)
  .bstsCreateControlPlots(jaspResults,options,ready)
  .bstsCreateErrorPlots(jaspResults,options,ready)

  return()
}

# Init functions ----
.bstsInitOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis

  return(options)
}

.bstsReadData <- function(options,ready) {
  # Read in the dataset using the built-in functions
  if(!ready) return()
  numericVariables  <- c(options$dependent,unlist(options$covariates))
  numericVariables  <- numericVariables[numericVariables != ""]
  nominalVars       <- unlist(options$fixedFactors)
  timeVar           <- unlist(options$time)
  timeVar           <- timeVar[timeVar != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric  = numericVariables,
                               columns.as.factor = nominalVars,
                               columns = timeVar)

  return(dataset)


}

.bstsErrorHandling <- function(dataset, options) {
  errors <- .hasErrors(dataset = dataset,
                       type = "missingValues",
                       missingValues.target = options$covariates,
                       exitAnalysisIfErrors = FALSE)
  if (identical(errors, FALSE)) return(FALSE)

  # A numeric read turns both missing entries and nonnumeric values into NA.
  # Read only the affected columns as factors to distinguish these cases using
  # JASP's missing-value handling, without changing the numeric analysis data.
  # Both reads use the same spreadsheet filter and preserve row order.
  affected <- errors$missingValues
  original <- .readDataSetToEnd(columns.as.factor = affected)
  missing <- nonNumeric <- character()
  for (variable in affected) {
    numericMissing <- is.na(dataset[[variable]])
    originalMissing <- is.na(original[[variable]])
    if (any(numericMissing & originalMissing))
      missing <- c(missing, variable)
    if (any(numericMissing & !originalMissing))
      nonNumeric <- c(nonNumeric, variable)
  }

  messages <- character()
  if (length(missing))
    messages <- c(messages, gettextf("The following covariates contain missing values: %1$s. Resolve the missing values before fitting the model.",
                                   paste(missing, collapse = ", ")))
  if (length(nonNumeric))
    messages <- c(messages, gettextf("The following covariates contain values that cannot be interpreted as numbers: %1$s. Covariates must be numeric. For categorical predictors, use Fixed Factors.",
                                   paste(nonNumeric, collapse = ", ")))
  .quitAnalysis(paste(messages, collapse = "\n\n"))
}

.bstsModelDependencies <- function() {
  return(c("dependent",
           "covariates",
           "expectedPredictors",
           "distFam",
           "samples",
           "seed",
           "modelTerms",
           "seasonalities",
           "autoregressiveComponent","lagSelectionMethod","lags","maxLags","arSdPrior","arSigmaGuess","arSigmaWeight",
           "localLevelComponent",'localLevelSdPrior','localLevelSigmaGuess','localLevelSigmaWeight',
           "localLinearTrendComponent",'lltLevelPrior','lltLevelSigmaGuess','lltLevelSigmaWeight','lltSlopePrior',
           'lltSlopeSigmaGuess','lltSlopeSigmaWeight',
           "semiLocalLinearTrendComponent",
           "dynamicRegregressionComponent"
  ))
}
.bstsStatePlotDependencies <- function(){
  return(c('aggregatedStatesPlot','aggregatedStatesPlotCiLevel',"aggregatedStatesPlotObservationsShown",'componentStatesPlot',
           "time", .bstsBurnDependencies()))
}

.bstsBurnDependencies <- function() {
  return(c("burninMethod", "automaticBurninProportion", "manualBurninAmount"))
}

.bstsPredictionDependencies <- function(){
  return(c("predictionHorizon", "seed", .bstsBurnDependencies()))
}


.bstsControlDependencies <- function(){
  return(c('controlChartPlot',"controlPeriod","controlSigma","probalisticControlPlot",
           "time", .bstsBurnDependencies()))
}


# Results functions "----",

.bstsComputeResults <- function(jaspResults, dataset, options,ready) {
  if (!ready) return()


  if (is.null(jaspResults[["bstsMainContainer"]][["bstsModelResults"]])) {
    bstsModelResultsState <- createJaspState()

    bstsModelResults <- .bstsResultsHelper(dataset,options)
    bstsModelResultsState$object <- bstsModelResults
    jaspResults[["bstsMainContainer"]][["bstsModelResults"]] <- bstsModelResultsState
  }

  return()
}

.bstsComputePredictions <- function(jaspResults, options, ready) {
  if (!ready) return()

  if (is.null(jaspResults[["bstsMainContainer"]][["bstsModelPredictions"]]) && options$predictionHorizon > 0) {
    bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object
    bstsModelPredictionsState <- createJaspState()
    bstsModelPredictionsState$dependOn(.bstsPredictionDependencies())
    bstsPredictionResults <- bsts::predict.bsts(object = bstsResults, horizon = options$predictionHorizon,
                                             burn = options$burn, seed = options$seed)
    bstsModelPredictionsState$object <- bstsPredictionResults
    jaspResults[["bstsMainContainer"]][["bstsModelPredictions"]] <- bstsModelPredictionsState
  }

  return()
}

.bstsResultsHelper <- function(dataset,options) {

  samples <- options$samples
  if (!is.numeric(samples) || is.complex(samples) || length(samples) != 1L ||
      !is.finite(samples) || samples < 1 || samples != floor(samples))
    .quitAnalysis(gettext("Desired MCMC draws must be a positive whole number."))

  response <- dataset[, options[["dependent"]]]
  # These two state constructors use y only for default prior scales and the
  # initial level. Use its first observed value, retaining every row for fitting.
  observedResponse <- response[!is.na(response)]
  if ((options$localLevelComponent || options$semiLocalLinearTrendComponent) &&
      length(observedResponse) < 2L)
    .quitAnalysis(gettext("At least two observed values of the dependent variable are required to initialize a local level or semilocal trend component."))

  predictors = NULL
  if (length(options$covariates)>0|length(options$fixedFactors) >0)
    predictors <- .bstsGetPredictors(options$modelTerms)
  formula = .bstsGetFormula(dependent=response,predictors = predictors,options)

  ss   <- list()
  #AddAr
  if(options$autoregressiveComponent){
    if(options$lagSelectionMethod == "manual")
      ss <- bsts::AddAr(ss,y = dataset[,options[["dependent"]]],lags =options$lags)

    if(options$lagSelectionMethod == "auto")
      ss <- bsts::AddAutoAr(ss,y=dataset[,options[["dependent"]]],lags=options$maxLags)
  }

  #Add local level Component

  if(options$localLevelComponent)
    ss <- bsts::AddLocalLevel(ss,y=observedResponse)

  # Add Local Linear trend component

  if(options$localLinearTrendComponent)
    ss <- bsts::AddLocalLinearTrend(ss,y=dataset[,options[["dependent"]]])

  # Add semi-local linear trend

  if(options$semiLocalLinearTrendComponent)
    ss <- bsts::AddSemilocalLinearTrend(ss,y=observedResponse)


  if (options$dynamicRegregressionComponent && !is.null(predictors))
    ss <- bsts::AddDynamicRegression(ss,formula=formula,data=dataset)


  if (!is.null(options$seasonalities)) {

    for (seas in options$seasonalities) {

      normalPriorSd.prior <- if(seas$inverseGammaPriorSd=="") NULL else{Boom::SdPrior(as.numeric(seas$inverseGammaPriorSd),seas$inverseGammaPriorN)}
      normal.prior <- if(seas$normalPriorSd=="") NULL else Boom::NormalPrior(seas$normalPriorMean,as.numeric(seas$normalPriorSd))
      ss <- bsts::AddSeasonal(ss,
                              y = dataset[,options[["dependent"]]],
                              nseasons = seas$number,
                              season.duration = seas$duration,
                              sigma.prior = normalPriorSd.prior,
                              initial.state.prior = normal.prior
      )
      if(!seas$name == "")
        ss[[length(ss)]]$name <- seas$name

    }
  }


  model <- tryCatch(bsts::bsts(formula = formula,
                      data=dataset,
                      state.specification = ss,
                      niter = options$samples,
                      ping = 0,
                      timestamps=NULL,
                      seed = options$seed,
                      expected.model.size = options$expectedPredictors,
                      model.options = bsts::BstsOptions(timeout.seconds = options$timeout )
  ), error = function(e) {
    detail <- conditionMessage(e)
    message <- if (grepl("Bmath domain error", detail, fixed = TRUE))
      gettextf("Model estimation failed because of a numerical problem during sampling. Check the dependent variable for extreme values and consider rescaling it to more moderate units. If the autoregressive component is enabled, temporarily disabling it can help identify the source of the problem.\n\nTechnical details:\n%1$s", detail)
    else
      gettextf("Model estimation failed.\n\n%1$s", detail)
    .quitAnalysis(message)
  }
  )

  return(model)
}





# Helper function to calculate burn-in amount and pass to options$burn-> needed for summary,plots,prediction
.bstsBurnHelper <- function(jaspResults,options) {

  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object
  # No fit exists while the analysis is not ready.
  if (is.null(bstsResults)) {
    options$burn <- 0L
    return(options)
  }

  options$burn <- tryCatch(.bstsResolveBurn(bstsResults, options),
                          error = function(e) .quitAnalysis(conditionMessage(e)))
  return(options)
}

.bstsResolveBurn <- function(bstsResults, options) {
  niter <- bstsResults$niter
  isFiniteScalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)

  if (!isFiniteScalar(niter) || niter < 1 || niter != floor(niter))
    stop(gettext("No completed MCMC draws are available for burn-in selection."), call. = FALSE)

  if (identical(options$burninMethod, "auto")) {
    proportion <- options$automaticBurninProportion
    if (!isFiniteScalar(proportion) || proportion <= 0 || proportion > 1)
      stop(gettext("The automatic burn-in proportion must be greater than 0 and at most 1. To discard no draws, select manual burn-in and enter 0."), call. = FALSE)

    # Preserve SuggestBurn's likelihood-threshold algorithm, but check its tail
    # length before it fails with an internal assertion on short runs.
    if (!is.null(bstsResults$log.likelihood) &&
        round(proportion * length(bstsResults$log.likelihood)) < 1)
      stop(gettext("Automatic burn-in needs a larger tail proportion or more completed MCMC draws. Alternatively, select manual burn-in."), call. = FALSE)

    burn <- bsts::SuggestBurn(proportion, bstsResults)
  } else if (identical(options$burninMethod, "manual")) {
    burn <- options$manualBurninAmount
  } else {
    stop(gettext("Select automatic or manual burn-in."), call. = FALSE)
  }

  if (!isFiniteScalar(burn) || burn < 0 || burn != floor(burn) || burn >= niter)
    stop(gettextf("Burn-in must be a whole number between 0 and %1$d, leaving at least one of the %2$d completed MCMC draws.",
                  niter - 1, niter), call. = FALSE)

  return(burn)
}

.bstsRetainedDraws <- function(bstsResults, burn) {
  # Positive selection is safe at burn = 0, unlike -(1:burn).
  draws <- seq_len(bstsResults$niter)
  return(draws[draws > burn])
}


.bstsTimeHelper <- function(jaspResults,dataset,options){
  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  actualValues <- as.numeric(bstsResults$original.series)
  options$time0 <- options$time
  if(options$time != "")
    options$time <- as.POSIXct(dataset[,options[["time"]]], tz = "UTC")
  else
    options$time <- 1:length(actualValues)

  return(options)

}

# Helper function to create regression formula that is passed into bsts functions

.bstsGetPredictors <- function(modelTerms, modelType = "alternative") {

  if (!is.character(modelType) || !modelType %in% c("alternative", "null"))
    stop(gettext("Unknown value provided for modelType, possible values: `alternative`, `null`"))

  predictors <- NULL

  for (i in seq_along(modelTerms)) {
    components <- unlist(modelTerms[[i]]$components)

    predictor <- paste0(components, collapse = ":")

    if (modelType == "alternative") {
      predictors <- c(predictors, predictor)
    } else if (modelType == "null") {
      isNuisance <- modelTerms[[i]]$isNuisance
      if (isNuisance)
        predictors <- c(predictors, predictor)
    }
  }

  return(predictors)
}

.bstsGetFormula <- function(dependent,options, predictors = NULL) {


  if (is.null(predictors))
    # if we don't have any predictors, the bsts function takes a vector as input
    return(dependent)
  else
    # if bsts has regression then we need an actual formula
    dependent = options[["dependent"]]
  formula <- paste(dependent, "~", paste(predictors, collapse = "+"))

  return(as.formula(formula, env = parent.frame(1)))
}



# helper function that determines which percentile rank the 2*sigma threshold is in the credible interval for each state
quantInv <- function(distr, value){

  if(sum(distr < value) == length(distr))
    return(1) # 1 if valuelarger than all MCMC draws
  else
    ecdf(distr)(value)
}





# Output functions ----
.bstsCreateContainerMain <- function(jaspResults, options,ready) {

  if (!is.null(jaspResults[["bstsMainContainer"]])) return()

  bstsMainContainer <- createJaspContainer()
  jaspResults[["bstsMainContainer"]] <- bstsMainContainer

  jaspResults[["bstsMainContainer"]]$dependOn(.bstsModelDependencies())

  return()
}


.bstsCreateModelSummaryTable <- function(jaspResults,options,ready){
  if(!is.null(jaspResults[["bstsMainContainer"]][["bstsModelSummaryTable"]])) return()

  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  bstsTable <- createJaspTable(title = gettext("Model Summary"))
  bstsTable$dependOn(.bstsBurnDependencies())
  bstsTable$position <- 1

  bstsTable$addColumnInfo(name="resSd",   title=gettext("Residual SD"),               type= "number")
  bstsTable$addColumnInfo(name="predSd",  title=gettext("Prediction SD"),             type= "number")
  bstsTable$addColumnInfo(name="R2",      title =gettextf("R%s", "\u00B2"),           type= "number")
  bstsTable$addColumnInfo(name="relGof",  title=gettext("Harvey's goodness of fit"),  type= "number")

  if(!is.null(bstsResults)){
    message <- if (bstsResults$niter < options$samples )
      gettextf("Only %1$s draws were sampled out of the desired %2$s. Additionally, %3$s MCMC draws out of %1$s are discarded as burn in.", bstsResults$niter, options$samples, options$burn)
    else
      gettextf("%1$d MCMC draws out of %2$d are discarded as burn in.", options$burn, bstsResults$niter)

    if (options$"localLevelComponent")
      message <- paste(message, gettext("The local level component has been selected (by default). If you wish to adjust the model, you can do so under 'Model Components'."), sep = "\n")
    bstsTable$addFootnote(message)
  }

  if(!is.null(bstsResults)){
    .bstsFillModelSummaryTable(bstsTable,bstsResults,ready,options$burn)
  }

  jaspResults[["bstsMainContainer"]][["bstsModelSummaryTable"]] <- bstsTable

  return()
}


.bstsFillModelSummaryTable <- function(bstsTable,bstsResults,ready,burn) {
  if(!ready) return()

  res <- summary(bstsResults, burn = burn)

  if (is.na(res$relative.gof)) {
    res$relative.gof <- "."
    message <- if (anyNA(bstsResults$original.series))
      gettext("Harvey's goodness of fit is unavailable when the dependent variable contains missing observations.")
    else
      gettext("Harvey's goodness of fit could not be computed.")
    bstsTable$addFootnote(message, colNames = "relGof")
  }

  bstsTable$addRows(list(
    resSd   = res$residual.sd,
    predSd   = res$prediction.sd,
    R2      = res$rsquare,
    relGof  = res$relative.gof

  ))

}

# table for regression coefficients"
.bstsCreateCoefficientTable <- function(jaspResults,options,ready){
  if (!ready || !length(options$modelTerms) || !options$posteriorSummaryTable ||
      !is.null(jaspResults[["bstsMainContainer"]][["bstsCoefficientSummaryTable"]])) return()


  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object
  bstsCoefficientTable <- createJaspTable(title = gettext("Model-Averaged Coefficient Estimates"))
  bstsCoefficientTable$dependOn(c("posteriorSummaryTable","posteriorSummaryCiLevel",
                                 .bstsBurnDependencies()))
  bstsCoefficientTable$position <- 2

  overtitle <- gettextf("%s%% Credible Interval", format(100*options[["posteriorSummaryCiLevel"]], digits = 3))
  bstsCoefficientTable$addColumnInfo(name = "coef",       title = gettext("Coefficients"),           type = "string")
  bstsCoefficientTable$addColumnInfo(name = "priorIncP",  title = gettext("P(incl)"),                type = "number")
  bstsCoefficientTable$addColumnInfo(name = "postIncP",   title = gettext("P(incl|data)"),           type = "number")
  bstsCoefficientTable$addColumnInfo(name = "BFinc",      title = gettext("BF<sub>inclusion</sub>"), type = "number")
  bstsCoefficientTable$addColumnInfo(name = 'mean',       title = gettext("Mean"),                   type = "number")
  bstsCoefficientTable$addColumnInfo(name = "sd",         title = gettext("SD"),                     type = "number")
  bstsCoefficientTable$addColumnInfo(name = "lowerCri",   title = gettext("Lower"),         type = "number", overtitle = overtitle)
  bstsCoefficientTable$addColumnInfo(name = "upperCri",   title = gettext("Upper"),         type = "number", overtitle = overtitle)
  bstsCoefficientTable$addFootnote(gettext("Estimates include all retained draws, including zeros when a predictor was excluded."))

  .bstsFillCoefficientTable(bstsResults,bstsCoefficientTable,options,ready)

  jaspResults[["bstsMainContainer"]][["bstsCoefficientSummaryTable"]] <- bstsCoefficientTable

}

.bstsFillCoefficientTable <- function(bstsResults,bstsCoefficientTable,options,ready) {
  if (!ready) return()
  res <- as.data.frame(summary(bstsResults, burn = options$burn, order = FALSE)$coefficients)
  res$priorInc <- bstsResults$prior$prior.inclusion.probabilities
  res$BFinc <- res$inc.prob/(1-res$inc.prob)



  ci <- options$posteriorSummaryCiLevel
  coefficients <- bstsResults$coefficients[.bstsRetainedDraws(bstsResults, options$burn), , drop = FALSE]
  res$lo_ci <- apply(coefficients, 2, stats::quantile, (1 - ci) / 2)
  res$hi_ci <- apply(coefficients, 2, stats::quantile, 1 - (1 - ci) / 2)
  res <- res[order(res$inc.prob,decreasing = TRUE),]


  for (i in 1:nrow(res)) {
    row <- list(
      coef = rownames(res[i,]),
      priorIncP = res$priorInc[i],
      postIncP = res$inc.prob[i],
      BFinc = res$BFinc[i],
      mean = res$mean[i],
      sd = res$sd[i],
      lowerCri = res$lo_ci[i],
      upperCri = res$hi_ci[i]
    )

    bstsCoefficientTable$addRows(row)

  }


}


.bstsCreateConditionalCoefficientTable <- function(jaspResults, options, ready) {
  if (!ready || !length(options$modelTerms) || !options$posteriorSummaryTable || !options$showCoefMeanInc ||
      !is.null(jaspResults[["bstsMainContainer"]][["bstsConditionalCoefficientSummaryTable"]])) return()

  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object
  table <- createJaspTable(title = gettext("Coefficient Estimates Conditional on Inclusion"))
  table$dependOn(c("posteriorSummaryTable", "showCoefMeanInc", "posteriorSummaryCiLevel",
                   .bstsBurnDependencies()))
  table$position <- 3
  overtitle <- gettextf("%s%% Credible Interval", format(100 * options$posteriorSummaryCiLevel, digits = 3))
  table$addColumnInfo(name = "coef",          title = gettext("Coefficients"),    type = "string")
  table$addColumnInfo(name = "postIncP",      title = gettext("P(incl|data)"),    type = "number")
  table$addColumnInfo(name = "includedDraws", title = gettext("Included draws"), type = "integer")
  table$addColumnInfo(name = "mean",          title = gettext("Mean"),            type = "number")
  table$addColumnInfo(name = "sd",            title = gettext("SD"),              type = "number")
  table$addColumnInfo(name = "lowerCri",      title = gettext("Lower"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upperCri",      title = gettext("Upper"), type = "number", overtitle = overtitle)
  table$addFootnote(gettext("Estimates use only retained draws in which the predictor was included (its coefficient was nonzero). These intervals describe coefficient uncertainty conditional on inclusion; they do not measure evidence for inclusion."))
  table$addFootnote(gettext("Conditional estimates may be unreliable when based on few included draws. Included draws are MCMC draws, not independent observations."))

  .bstsFillConditionalCoefficientTable(bstsResults, table, options, ready)
  jaspResults[["bstsMainContainer"]][["bstsConditionalCoefficientSummaryTable"]] <- table
}

.bstsFillConditionalCoefficientTable <- function(bstsResults, table, options, ready) {
  if (!ready) return()
  coefficients <- bstsResults$coefficients[.bstsRetainedDraws(bstsResults, options$burn), , drop = FALSE]
  inclusionProbability <- colMeans(coefficients != 0)
  probabilities <- c((1 - options$posteriorSummaryCiLevel) / 2,
                     1 - (1 - options$posteriorSummaryCiLevel) / 2)

  # Match the model-averaged table's order, including ties. Negative draws count
  # as inclusion too; zero draws belong only in the model-averaged summaries.
  for (j in order(inclusionProbability, decreasing = TRUE)) {
    included <- coefficients[coefficients[, j] != 0, j]
    nIncluded <- length(included)
    interval <- if (nIncluded > 1L) stats::quantile(included, probabilities, names = FALSE) else c(NA_real_, NA_real_)
    coefficient <- colnames(coefficients)[j]
    table$addRows(list(coef = coefficient,
                       postIncP = inclusionProbability[j],
                       includedDraws = nIncluded,
                       mean = if (nIncluded > 0L) mean(included) else NA_real_,
                       sd = if (nIncluded > 1L) stats::sd(included) else NA_real_,
                       lowerCri = interval[1L],
                       upperCri = interval[2L]), rowNames = coefficient)

    if (nIncluded == 0L)
      table$addFootnote(gettext("No retained draws included this predictor; conditional estimates are unavailable."),
                        rowNames = coefficient, colNames = c("mean", "sd", "lowerCri", "upperCri"))
    else if (nIncluded == 1L)
      table$addFootnote(gettext("Only one retained draw included this predictor; its conditional SD and credible interval are unavailable."),
                        rowNames = coefficient, colNames = c("sd", "lowerCri", "upperCri"))
  }
}


.bstsCreateStatePlots <- function(jaspResults,dataset,options,ready) {


  if (!is.null(jaspResults[["bstsMainContainer"]][["bstsStatePlots"]])) return()


  bstsStatePlots <- createJaspContainer(title = gettext("State Plots"))


  bstsStatePlots$dependOn(.bstsStatePlotDependencies())
  jaspResults[["bstsMainContainer"]][["bstsStatePlots"]] <- bstsStatePlots


  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  if(options$aggregatedStatesPlot) .bstsAggregatedStatePlot(bstsStatePlots,bstsResults,dataset,options,ready)
  if(options$componentStatesPlot)
    .bstsComponentStatePlot(bstsStatePlots,bstsResults,options,ready)


  return()
}

.bstsAggregatedStatePlot <- function(bstsStatePlots,bstsResults,dataset,options,ready) {
  if (!ready | !options$aggregatedStatesPlot) return()


  bstsAggregatedStatePlot <- createJaspPlot(title= gettext("Aggregated State"), height = 320, width = 480)


  # get all states
  state <- bstsResults$state.contributions
  # discard burn ins
  state <- state[.bstsRetainedDraws(bstsResults, options$burn), , , drop = FALSE]
  # sum to final state
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)
  actualValues <- as.numeric(bstsResults$original.series)



  ymin <- apply(state,2,quantile,probs= ((1- options$aggregatedStatesPlotCiLevel)/2))
  ymax <- apply(state,2,quantile,probs= 1-((1- options$aggregatedStatesPlotCiLevel)/2))

  time <- options$time

  mean <-colMeans(state)


  p <-  ggplot2::ggplot(NULL,ggplot2::aes(x=time,y=mean)) +
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=ymin,ymax =ymax),
                         fill ="blue",alpha=0.5) + ggplot2::xlab(gettext("Time")) +
    ggplot2::ylab(gettext("Distribution")) +
    ggplot2::geom_line(size=0.7)


  if(options$aggregatedStatesPlotObservationsShown)
    p <- p + ggplot2::geom_point(ggplot2::aes(y=actualValues))

  p <- jaspGraphs::themeJasp(p)

  bstsAggregatedStatePlot$plotObject <- p

  bstsStatePlots[["bstsAggregatedStatePlot"]] <- bstsAggregatedStatePlot

  return()
}

.bstsComponentStatePlot <- function(bstsStatePlots,bstsResults,options,ready){
  if (!ready) return()

  bstsComponentStatePlot <- createJaspPlot(title= gettext("Component States"), height = 320, width = 480)

  state <- bstsResults$state.contributions[.bstsRetainedDraws(bstsResults, options$burn), , , drop = FALSE]
  # Summarize iterations only, preserving time/component dimensions even when
  # a single draw remains.
  means <- as.data.frame(t(apply(state, c(2, 3), mean)))

  means$time <- options$time

  ymin <- as.data.frame(t(apply(state, c(2, 3), quantile, probs = 0.025)))
  ymax <- as.data.frame(t(apply(state, c(2, 3), quantile, probs = 0.975)))
  ymin$time <- options$time
  ymax$time <- options$time

  ymin <- reshape2::melt(ymin,id.vars='time',variable.name="component")
  ymax <- reshape2::melt(ymax,id.vars='time',variable.name="component")
  means2 <- reshape2::melt(means,id.vars='time',variable.name="component")

  p <-  ggplot2::ggplot(data=means2, ggplot2::aes(x=time, y=value)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=ymin$value,ymax=ymax$value),
                         fill ="blue",alpha=0.5) +
    ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank()) + ggplot2::ylab("") + ggplot2::xlab("")   +
    ggplot2::facet_grid(component ~ ., scales="free") +
    ggplot2::guides(colour=FALSE) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))


  bstsComponentStatePlot$plotObject <- p

  bstsStatePlots[["bstsComponentStatePlot"]] <- bstsComponentStatePlot

  return()
}

.bstsCreatePredictionPlot <- function(jaspResults,options,ready) {
  if(!ready | !options$predictionHorizon>0) return()
  if (!is.null(jaspResults[["bstsMainContainer"]][["bstsPredictionPlot"]])) return()

  bstsPredictionPlot <- createJaspPlot(title=gettext("Prediction plot"), height = 320, width = 480)
  bstsPredictionPlot$dependOn(.bstsPredictionDependencies())

  bstsModelResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  bstsPredictionResults <- jaspResults[["bstsMainContainer"]][["bstsModelPredictions"]]$object

  predictionRange <- (length(bstsPredictionResults$original.series)+1):(length(bstsPredictionResults$original.series)+options$predictionHorizon)

  predDF <- data.frame(time=seq_along(bstsPredictionResults$original.series),mean=bstsPredictionResults$original.series)
  predDF[predictionRange,"mean"] <-bstsPredictionResults$mean
  predDF[predictionRange,"LL"] <- bstsPredictionResults$interval[1,]
  predDF[predictionRange,"UL"] <- bstsPredictionResults$interval[2,]
  predDF[predictionRange,"time"] <- predictionRange

  p <- ggplot2::ggplot(data=predDF,ggplot2::aes(x=time,y=mean)) +
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=LL,ymax=UL),
                         fill ="blue",alpha=0.5) +
    ggplot2::xlab(gettext("Time")) +
    ggplot2::geom_line(size=0.7) +
    ggplot2::geom_vline(xintercept=length(bstsPredictionResults$original.series),linetype=2) +
    ggplot2::theme_classic()

  p <- jaspGraphs::themeJasp(p)

  bstsPredictionPlot$plotObject <- p
  jaspResults[["bstsMainContainer"]][["bstsPredictionPlot"]] <- bstsPredictionPlot

  return()
}
# control plot

.bstsCreateControlPlots <- function(jaspResults,options,ready){
  if (!is.null(jaspResults[["bstsMainContainer"]][["bstsControlPlots"]]) | !ready) return()


  bstsControlPlots <- createJaspContainer(title = gettext("Control Plots"))

  bstsControlPlots$dependOn(.bstsControlDependencies())
  jaspResults[["bstsMainContainer"]][["bstsControlPlots"]] <- bstsControlPlots
  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  if (options$controlChartPlot || options$probalisticControlPlot) {
    period <- options$controlPeriod
    nTime <- dim(bstsResults$state.contributions)[3L]
    if (!is.numeric(period) || length(period) != 1L || !is.finite(period) ||
        period != floor(period) || period < 2 || period > nTime) {
      message <- gettextf("Control period end must be a whole number of at least 2 and cannot exceed the number of time points (%1$d).", nTime)
      titles <- c(bstsControlPlotThreshold = gettext("Threshold Plot"),
                  bstsControlPlotProbability = gettext("Probability Plot"))
      requested <- c(options$controlChartPlot, options$probalisticControlPlot)
      for (key in names(titles)[requested]) {
        plot <- createJaspPlot(title = titles[[key]], height = 320, width = 480)
        plot$setError(message)
        bstsControlPlots[[key]] <- plot
      }
      return()
    }
  }

  if(options$controlChartPlot)
    .bstsFillControlPlotThreshold(bstsControlPlots,bstsResults,options,ready)

  if(options$probalisticControlPlot)
    .bstsFillControlPlotProbability(bstsControlPlots,bstsResults,options,ready)


  return()

}

.bstsFillControlPlotThreshold <- function(bstsControlPlots,bstsResults,options,ready){

  control = c(1:options$controlPeriod)
  L = options$controlSigma
  CI=.95
  bstsControlPlotThreshold <- createJaspPlot(title=gettext("Threshold Plot"), height = 320, width = 480)

  # extract model components
  state <- bstsResults$state.contributions
  # discard burn ins
  state <- state[.bstsRetainedDraws(bstsResults, options$burn), , , drop = FALSE]
  # sum to final state
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)

  # determine 0.95% credible interval
  ymin <- apply(state,2,quantile,probs= ((1- CI)/2))
  ymax <- apply(state,2,quantile,probs= 1-((1- CI)/2))

  time <- options$time
  mean <-colMeans(state)

  # compute limits based on the estimated state
  state_mean <- mean(mean[control])
  state_sd <- sd(mean[control])


  ul_state <- state_mean + L* state_sd
  ll_state <- state_mean - L* state_sd

  # ymin is lower 95 cI bound of state
  # ul is the threshold that has to be exceeded according to 2 sigmas
  # thus ymin must be larger  than the ul threshold
  CI_reached <- ymin > ul_state # cases where 95% confident that above threshold

  p <- ggplot2::ggplot(NULL,ggplot2::aes(x=time,y=mean))+
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=ymin,ymax =ymax ),
                         alpha=0.5,fill ="blue") + ggplot2::xlab(gettext("Time")) +
    ggplot2::ylab(gettext("Distribution")) +
    ggplot2::geom_line(size=0.7) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept=ul_state, linetype="dashed", color = "red") +
    ggplot2::geom_hline(yintercept=ll_state, linetype="dashed", color = "red")

  first_threshold <- time[which(ymin > ul_state)[1]]

    p <- jaspGraphs::themeJasp(p)

  if (!is.na(first_threshold)) {
    captionText <- if (options$time0 != "")
      gettextf("First date where %1$.3f*sigma is exceeded by %2$.1f%% state credible interval: %3$s", L, 100 * CI, first_threshold)
    else
      gettextf("First time point where %1$.3f*sigma is exceeded by %2$.1f%% state credible interval: %3$s", L, 100 * CI, first_threshold)
  } else {
    captionText <- gettext("Threshold never reached")
  }

  p <- p + ggplot2::labs(caption = captionText)



  p <- p+ ggplot2::theme(plot.caption = ggplot2::element_text(face="italic",hjust = 0,size=12)) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 10, 15))

  bstsControlPlotThreshold$plotObject <- p



  bstsControlPlots[["bstsControlPlotThreshold"]] <- bstsControlPlotThreshold
  return()
}

.bstsFillControlPlotProbability <- function(bstsControlPlots,bstsResults,options,ready){

  bstsControlPlotProbability <- createJaspPlot(title=gettext("Probability Plot"), height = 320, width = 480)


  control = c(1:options$controlPeriod)
  L = options$controlSigma
  CI=.95 # add option in JASP later
  bstsControlPlotThreshold <- createJaspPlot(title=gettext("Threshold Plot"))

  # extract model components
  state <- bstsResults$state.contributions
  # discard burn ins
  state <- state[.bstsRetainedDraws(bstsResults, options$burn), , , drop = FALSE]
  # sum to final state
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)

  # determine 0.95% credible interval
  ymin <- apply(state,2,quantile,probs= ((1- CI)/2))
  ymax <- apply(state,2,quantile,probs= 1-((1- CI)/2))

  time <- options$time
  mean <-colMeans(state)

  # compute limits based on the estimated state
  state_mean <- mean(mean[control])
  state_sd <- sd(mean[control])


  ul_state <- state_mean + L* state_sd
  ll_state <- state_mean - L* state_sd

  # ymin is lower 95 cI bound of state
  # ul is the threshold that has to be exceeded according to 2 sigmas
  # thus ymin must be larger  than the ul threshold
  CI_reached <- ymin > ul_state # cases where 95% confident that above threshold

  # now let's compute probability of exeeding the threshold by getting the percentile rank from the MCMC draws

  threshold_prob <- 1-apply(state, 2, quantInv,ul_state)



  p <- ggplot2::ggplot(NULL,ggplot2::aes(time,threshold_prob)) + ggplot2::xlab(gettext("Time")) +
    ggplot2::ylab(gettext("Probability")) + ggplot2::geom_line() + ggplot2::theme_classic() + ggplot2::ylim(0,1)


  p <- jaspGraphs::themeJasp(p) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 10, 15))

  bstsControlPlotProbability$plotObject <- p

  bstsControlPlots[["bstsControlPlotProbability"]] <- bstsControlPlotProbability
  return()
}

.bstsCreateErrorPlots <- function(jaspResults,options,ready) {
  if (!is.null(jaspResults[["bstsMainContainer"]][["bstsErrorPlots"]])) return()


  bstsErrorPlots <- createJaspContainer(title = gettext("Error Plots"))
  bstsErrorPlots$dependOn(c("residualPlot", "forecastErrorPlot", "aggregatedStatesPlotCiLevel",
                          "time", .bstsBurnDependencies()))
  jaspResults[["bstsMainContainer"]][["bstsErrorPlots"]] <- bstsErrorPlots

  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  if(options$residualPlot) .bstsResidualPlot(bstsErrorPlots,bstsResults,options,ready)
  if(options$forecastErrorPlot)
    .bstsForecastErrorPlot(bstsErrorPlots,bstsResults,options,ready)


  return()
}


.bstsResidualPlot <- function(bstsErrorPlots,bstsResults,options,ready){
  if (!ready | !options$residualPlot) return()


  bstsResidualPlot <- createJaspPlot(title= gettext("Residual Plot"), height = 320, width = 480)

  residuals <- bsts::residuals.bsts(bstsResults,options$burn)

  mean <- colMeans(residuals)
  ymin <- apply(residuals,2,quantile,probs= ((1- options$aggregatedStatesPlotCiLevel)/2),na.rm=TRUE)
  ymax <- apply(residuals,2,quantile,probs= 1-((1- options$aggregatedStatesPlotCiLevel)/2),na.rm=TRUE)

  time <- options$time

  p <-  ggplot2::ggplot(NULL,ggplot2::aes(x=time,y=mean)) +
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=ymin,ymax =ymax),
                         fill ="blue",alpha=0.5) + ggplot2::xlab(gettext("Time")) +
    ggplot2::ylab(gettext("Distribution")) +
    ggplot2::geom_line(size=0.7)

  p <- jaspGraphs::themeJasp(p)

  bstsResidualPlot$plotObject <- p

  bstsErrorPlots[["bstsResidualPlot"]] <- bstsResidualPlot

  return()
}

.bstsForecastErrorPlot <- function(bstsErrorPlots,bstsResults,options,ready){
  if (!ready | !options$forecastErrorPlot) return()


  bstsForecastErrorPlot <- createJaspPlot(title= gettext("Forecast Error Plot"), height = 320, width = 480)

  errors <- bsts::bsts.prediction.errors(bstsResults,burn=options$burn)$in.sample

  mean <- colMeans(errors)
  ymin <- apply(errors,2,quantile,probs= ((1- options$aggregatedStatesPlotCiLevel)/2))
  ymax <- apply(errors,2,quantile,probs= 1-((1- options$aggregatedStatesPlotCiLevel)/2))

  time <- options$time

  p <-  ggplot2::ggplot(NULL,ggplot2::aes(x=time,y=mean)) +
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=ymin,ymax =ymax),
                         fill ="blue",alpha=0.5) + ggplot2::xlab(gettext("Time")) +
    ggplot2::ylab(gettext("Distribution")) +
    ggplot2::geom_line(size=0.7)

  p <- jaspGraphs::themeJasp(p)

  bstsForecastErrorPlot$plotObject <- p

  bstsErrorPlots[["bstsForecastErrorPlot"]] <- bstsForecastErrorPlot

  return()
}

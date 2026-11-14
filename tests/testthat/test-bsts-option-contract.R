.bstsContractQml <- function() {
  path <- system.file("qml", "bayesianStateSpace.qml", package = "jaspTimeSeries")
  stopifnot(nzchar(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # Commented controls are deliberately inactive, even if their historical
  # dependency keys remain in the model container.
  text <- gsub("//[^\n]*", "", text)
  matches <- regmatches(text, gregexpr('\\bname\\s*:\\s*"[^"]+"', text, perl = TRUE))[[1L]]
  names <- unique(sub('.*"([^"]+)"$', "\\1", matches))
  list(names = names, path = path, defaults = jaspTools::analysisOptions("bayesianStateSpace"))
}

.bstsContractWalk <- function(expression, inspect) {
  inspect(expression)
  if (is.call(expression) || is.expression(expression)) {
    children <- as.list(expression)
    for (index in seq_along(children))
      if (!identical(children[[index]], quote(expr = )))
        .bstsContractWalk(children[[index]], inspect)
  }
  invisible(NULL)
}

.bstsContractOptionKeys <- function(functions, object) {
  keys <- character()
  for (fun in functions) .bstsContractWalk(body(fun), function(expression) {
    if (!is.call(expression) || length(expression) < 3L ||
        !identical(expression[[2L]], as.name(object))) return()
    operator <- as.character(expression[[1L]])[1L]
    if (operator == "$")
      keys <<- c(keys, as.character(expression[[3L]]))
    else if (operator %in% c("[", "[["))
      .bstsContractWalk(expression[[3L]], function(index) {
        if (is.character(index)) keys <<- c(keys, index)
      })
  })
  unique(keys)
}

.bstsContractRowKeys <- c("name", "number", "duration", "inverseGammaPriorSd",
                        "inverseGammaPriorN", "normalPriorMean", "normalPriorSd")

test_that("BSTS active controls, wrapper arguments, and R option keys agree", {
  qml <- .bstsContractQml()
  # Available lists do not serialize as analysis options; seasonal controls
  # serialize inside each seasonalities row, not as top-level options.
  active <- setdiff(qml$names, c("allVariablesList", "availableTerm", .bstsContractRowKeys))
  wrapper <- formals(jaspTimeSeries::bayesianStateSpace)
  framework <- c("data", "version", "formula", "isNuisance", "plotWidth", "plotHeight")
  # This pre-existing ignored argument is the sole syntax compatibility exception.
  legacy <- "dynamicRegregressionLags"
  expect_setequal(setdiff(names(wrapper), c(framework, legacy)), active)
  expect_identical(eval(wrapper[[legacy]]), 0)

  namespace <- asNamespace("jaspTimeSeries")
  names <- c("bayesianStateSpaceInternal", grep("^\\.bsts", ls(namespace, all.names = TRUE), value = TRUE))
  functions <- mget(names, namespace)
  # burn and time0 are deliberately constructed internally after fitting.
  expect_setequal(setdiff(.bstsContractOptionKeys(functions, "options"), c("burn", "time0")), active)
  expect_setequal(.bstsContractOptionKeys(functions, "seas"), .bstsContractRowKeys)
  expect_true(all(.bstsContractRowKeys %in% qml$names))
})

test_that("BSTS option naming preserves every QML and wrapper default", {
  defaults <- list(
    posteriorSummaryTable = FALSE, posteriorSummaryConditionalEstimates = FALSE,
    posteriorSummaryCiLevel = .95, expectedPredictors = 1,
    autoregressiveComponent = FALSE, autoregressiveLagSelectionMethod = "manual",
    autoregressiveLags = 1, autoregressiveMaxLags = 1, localLevelComponent = TRUE,
    localLinearTrendComponent = FALSE, semiLocalLinearTrendComponent = FALSE,
    dynamicRegressionComponent = FALSE, aggregatedStateAndErrorPlotsCiLevel = .95,
    aggregatedStatesPlot = FALSE, aggregatedStatesPlotObservationsShown = FALSE,
    componentStatesPlot = FALSE, residualPlot = FALSE, forecastErrorPlot = FALSE,
    predictionHorizon = 0, controlChartPlot = FALSE, controlChartPeriodEnd = 100,
    controlChartSigmaThreshold = 2, controlChartProbabilisticPlot = FALSE,
    mcmcDraws = 2000, mcmcTimeoutSeconds = 120, burninMethod = "auto",
    burninAutomaticProportion = .1, burninManualDraws = 0, seed = 1)
  qml <- .bstsContractQml()
  wrapper <- lapply(formals(jaspTimeSeries::bayesianStateSpace), eval)
  assignments <- c("dependent", "covariates", "fixedFactors", "time")
  nested <- c("modelTerms", "seasonalities")
  active <- setdiff(qml$names, c("allVariablesList", "availableTerm", .bstsContractRowKeys))
  expect_setequal(names(defaults), setdiff(active, c(assignments, nested)))
  # Native QML parsing keeps IntegerField values as integers. Compare their
  # values, independent of R storage type and analysisOptions' tracking metadata.
  qmlDefaults <- setNames(lapply(names(defaults), function(name) qml$defaults[[name]]), names(defaults))
  expect_equal(qmlDefaults, defaults)
  expect_identical(wrapper[names(defaults)], defaults)

  for (name in assignments) {
    value <- if (name %in% c("dependent", "time")) "" else list()
    # analysisOptions deliberately omits empty list options; the real form
    # recreates these when options are passed to runAnalysis.
    if (is.list(value)) expect_null(qml$defaults[[name]])
    else expect_identical(qml$defaults[[name]], value)
    expect_identical(wrapper[[name]], list(types = list(), value = value))
  }
  for (name in nested) {
    expect_null(qml$defaults[[name]])
    expect_identical(wrapper[[name]], list())
  }
  # Row controls are initialized only when a seasonal component exists.
  # Let the real QML form populate an empty row rather than flattening its fields.
  seasonal <- jaspSyntax::parseQmlOptions(
    qml$path, options = '{"seasonalities":[{}]}',
    moduleName = "jaspTimeSeries", analysisName = "bayesianStateSpace",
    version = as.character(utils::packageVersion("jaspTimeSeries")), preloadData = FALSE,
    includeMeta = FALSE, includeTypeOptions = FALSE, isolated = FALSE)$seasonalities
  expect_length(seasonal, 1L)
  expect_equal(seasonal[[1L]][.bstsContractRowKeys],
    list(name = "", number = 2, duration = 1, inverseGammaPriorSd = .01,
         inverseGammaPriorN = .01, normalPriorMean = 0, normalPriorSd = .01))
  expect_identical(wrapper[c("plotWidth", "plotHeight")], list(plotWidth = 480, plotHeight = 320))
})

test_that("BSTS dependency declarations use current option names", {
  namespace <- asNamespace("jaspTimeSeries")
  functions <- mget(grep("^\\.bsts", ls(namespace, all.names = TRUE), value = TRUE), namespace)
  helpers <- functions[grepl("Dependencies$", names(functions))]
  dependencies <- unlist(lapply(helpers, function(fun) fun()), use.names = FALSE)
  for (fun in functions) .bstsContractWalk(body(fun), function(expression) {
    if (!is.call(expression) || !is.call(expression[[1L]])) return()
    method <- expression[[1L]]
    if (length(method) != 3L || !identical(method[[1L]], as.name("$")) ||
        !identical(method[[3L]], as.name("dependOn"))) return()
    # Inspect dependency arguments only; the receiver may itself contain a
    # string subscript naming the result container.
    for (argument in as.list(expression)[-1L])
      .bstsContractWalk(argument, function(value) {
        if (is.character(value)) dependencies <<- c(dependencies, value)
      })
  })
  active <- setdiff(.bstsContractQml()$names,
                   c("allVariablesList", "availableTerm", .bstsContractRowKeys))
  dormant <- c("distFam", "arSdPrior", "arSigmaGuess", "arSigmaWeight", "localLevelSdPrior",
               "localLevelSigmaGuess", "localLevelSigmaWeight", "lltLevelPrior", "lltLevelSigmaGuess",
               "lltLevelSigmaWeight", "lltSlopePrior", "lltSlopeSigmaGuess", "lltSlopeSigmaWeight")
  expect_setequal(setdiff(unique(dependencies), active), dormant)
  # Every option consumed by the fit refits the model, including fixed factors
  # and the MCMC timeout.
  expect_setequal(helpers$.bstsModelDependencies(), c(
    "dependent", "time", "covariates", "fixedFactors", "mcmcTimeoutSeconds", "expectedPredictors", "mcmcDraws", "seed", "modelTerms",
    "seasonalities", "autoregressiveComponent", "autoregressiveLagSelectionMethod",
    "autoregressiveLags", "autoregressiveMaxLags", "localLevelComponent", "localLinearTrendComponent",
    "semiLocalLinearTrendComponent", "dynamicRegressionComponent", dormant))
})

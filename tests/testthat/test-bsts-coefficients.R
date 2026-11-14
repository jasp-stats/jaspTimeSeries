# Known draws separate the two estimands without relying on stochastic MCMC
# output. The first two rows deliberately differ from the retained posterior.
.bstsCoefficientTestFit <- function() {
  loadNamespace("bsts") # Register summary.bsts for the model-averaged table.
  coefficients <- cbind(
    never = c(9, -9, 0, 0, 0, 0, 0, 0),
    once = c(30, 40, 0, -3, 0, 0, 0, 0),
    twice = c(300, -300, 0, 2, 0, 4, 0, 0),
    always = c(-99, 99, 1, 2, 3, 4, 5, 6),
    negative = c(99, 100, -1, -2, -3, -4, -5, -6),
    mixed = c(101, -101, -2, 0, 4, 0, -6, 0),
    tied = c(-888, 888, 8, 0, 0, -4, 0, 0)
  )
  niter <- nrow(coefficients)
  structure(list(
    niter = niter, family = "gaussian", has.regression = TRUE,
    coefficients = coefficients,
    prior = list(prior.inclusion.probabilities = c(0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8)),
    sigma.obs = seq_len(niter) / 10,
    original.series = c(1, 4, 2, 6),
    one.step.prediction.errors = outer(seq_len(niter), 1:4, "+") / 10,
    state.contributions = array(seq_len(niter * 2L * 4L),
      dim = c(niter, 2L, 4L), dimnames = list(NULL, c("level", "trend"), NULL)),
    log.likelihood = c(-100, -100, rep(-3, 6)),
    timestamp.info = list(timestamps.are.trivial = TRUE, timestamps = 1:4)
  ), class = "bsts")
}

.bstsCoefficientTestOptions <- function() {
  list(burn = 2, posteriorSummaryCiLevel = 0.95,
       modelTerms = list(list(components = "predictor")),
       posteriorSummaryTable = TRUE, posteriorSummaryConditionalEstimates = TRUE)
}

# Plain R collectors keep native result construction out of these unit tests.
# The native supported runner must separately verify rendering/invalidation.
.bstsCoefficientTestTable <- function(title = "") {
  table <- new.env(parent = emptyenv())
  table$title <- title
  table$rows <- table$columns <- table$footnotes <- list()
  table$rowNames <- character()
  table$dependOn <- function(dependencies) table$dependencies <- dependencies
  table$addColumnInfo <- function(name, ...) table$columns[[name]] <- list(...)
  table$addRows <- function(row, rowNames = NULL) {
    table$rows[[length(table$rows) + 1L]] <- row
    table$rowNames <- c(table$rowNames, rowNames)
  }
  table$addFootnote <- function(message, ...) {
    table$footnotes[[length(table$footnotes) + 1L]] <- c(list(message = message), list(...))
  }
  table
}

.bstsCoefficientTestFunction <- function(name) getFromNamespace(name, "jaspTimeSeries")

.bstsCoefficientTestBuilder <- function(name) {
  builder <- .bstsCoefficientTestFunction(name)
  scope <- new.env(parent = environment(builder))
  scope$createJaspTable <- .bstsCoefficientTestTable
  environment(builder) <- scope
  builder
}

.bstsCoefficientTestResult <- function(fit) {
  main <- new.env(parent = emptyenv())
  main$bstsModelResults <- list(object = fit)
  list(bstsMainContainer = main)
}

.bstsCoefficientTestRows <- function(table) {
  setNames(table$rows, vapply(table$rows, function(row) row$coef, character(1)))
}

test_that("BSTS coefficient tables preserve predictor identities and distinguish estimands", {
  fit <- .bstsCoefficientTestFit()
  before <- serialize(fit, NULL)
  options <- .bstsCoefficientTestOptions()
  marginal <- .bstsCoefficientTestTable()
  conditional <- .bstsCoefficientTestTable()
  .bstsCoefficientTestFunction(".bstsFillCoefficientTable")(fit, marginal, options, TRUE)
  .bstsCoefficientTestFunction(".bstsFillConditionalCoefficientTable")(fit, conditional, options, TRUE)
  marginal <- .bstsCoefficientTestRows(marginal)
  conditional <- .bstsCoefficientTestRows(conditional)
  expectedOrder <- c("always", "negative", "mixed", "twice", "tied", "once", "never")
  expect_identical(names(marginal), expectedOrder)
  expect_identical(names(conditional), expectedOrder)
  expect_equal(vapply(conditional, function(row) unname(row$postIncP), numeric(1)),
               setNames(c(1, 1, 1 / 2, 1 / 3, 1 / 3, 1 / 6, 0), expectedOrder))
  expect_identical(vapply(conditional, function(row) row$includedDraws, integer(1)),
                   setNames(c(6L, 6L, 3L, 2L, 2L, 1L, 0L), expectedOrder))
  for (name in expectedOrder) {
    originalColumn <- match(name, colnames(fit$coefficients))
    expect_equal(marginal[[name]]$priorIncP, fit$prior$prior.inclusion.probabilities[originalColumn])
    expect_equal(marginal[[name]]$postIncP, unname(conditional[[name]]$postIncP))
    expect_false(any(c("meanInc", "sdInc") %in% names(marginal[[name]])))
    if (conditional[[name]]$includedDraws > 0L)
      expect_equal(marginal[[name]]$mean,
                   unname(conditional[[name]]$postIncP) * conditional[[name]]$mean)
  }
  # Zeros change marginal summaries; negative draws count as inclusion.
  expect_equal(marginal$twice$mean, 1)
  expect_equal(conditional$twice$mean, 3)
  expect_equal(conditional$twice$sd, sqrt(2))
  expect_equal(c(conditional$twice$lowerCri, conditional$twice$upperCri), c(2.05, 3.95))
  expect_equal(marginal$mixed$mean, -2 / 3)
  expect_equal(conditional$mixed$mean, -4 / 3)
  expect_equal(conditional$negative$mean, -3.5)
  expect_equal(c(conditional$negative$lowerCri, conditional$negative$upperCri), c(-5.875, -1.125))
  expect_equal(conditional$once$mean, -3)
  expect_true(all(is.na(unlist(conditional$once[c("sd", "lowerCri", "upperCri")]))))
  expect_true(all(is.na(unlist(conditional$never[c("mean", "sd", "lowerCri", "upperCri")]))))
  expect_identical(serialize(fit, NULL), before)
})

test_that("BSTS conditional summaries use the same burn and requested credible level", {
  fit <- .bstsCoefficientTestFit()
  before <- serialize(fit, NULL)
  fill <- .bstsCoefficientTestFunction(".bstsFillConditionalCoefficientTable")
  for (burn in c(0, 2, 7)) {
    for (ci in c(0.5, 0.8, 0.95)) {
      options <- .bstsCoefficientTestOptions()
      options$burn <- burn
      options$posteriorSummaryCiLevel <- ci
      table <- .bstsCoefficientTestTable()
      fill(fit, table, options, TRUE)
      expect_length(table$rows, ncol(fit$coefficients))
      marginal <- .bstsCoefficientTestTable()
      .bstsCoefficientTestFunction(".bstsFillCoefficientTable")(fit, marginal, options, TRUE)
      expect_identical(names(.bstsCoefficientTestRows(table)), names(.bstsCoefficientTestRows(marginal)))
      for (row in table$rows) {
        beta <- fit$coefficients[seq_len(fit$niter) > burn, row$coef]
        included <- beta[beta != 0]
        expect_identical(row$includedDraws, length(included))
        expect_equal(unname(row$postIncP), length(included) / length(beta))
        expect_equal(row$mean, if (length(included)) mean(included) else NA_real_)
        expect_equal(row$sd, if (length(included) > 1L) sd(included) else NA_real_)
        expectedInterval <- if (length(included) > 1L)
          quantile(included, c((1 - ci) / 2, (1 + ci) / 2), names = FALSE, type = 7) else c(NA_real_, NA_real_)
        expect_equal(c(row$lowerCri, row$upperCri), expectedInterval)
      }
    }
  }
  expect_identical(serialize(fit, NULL), before)
})

test_that("BSTS coefficient summaries retain one-predictor matrix dimensions", {
  fit <- .bstsCoefficientTestFit()
  fit$coefficients <- fit$coefficients[, "negative", drop = FALSE]
  fit$prior$prior.inclusion.probabilities <- 0.6
  for (burn in c(0, 2, 7)) {
    options <- .bstsCoefficientTestOptions()
    options$burn <- burn
    for (name in c(".bstsFillCoefficientTable", ".bstsFillConditionalCoefficientTable")) {
      table <- .bstsCoefficientTestTable()
      .bstsCoefficientTestFunction(name)(fit, table, options, TRUE)
      expect_length(table$rows, 1L)
      expect_identical(table$rows[[1L]]$coef, "negative")
      expect_equal(table$rows[[1L]]$mean, mean(fit$coefficients[seq_len(fit$niter) > burn, 1]))
    }
  }
})

test_that("BSTS missing conditional uncertainty is explained only at affected cells", {
  table <- .bstsCoefficientTestTable()
  .bstsCoefficientTestFunction(".bstsFillConditionalCoefficientTable")(
    .bstsCoefficientTestFit(), table, .bstsCoefficientTestOptions(), TRUE)
  expect_identical(table$rowNames, vapply(table$rows, function(row) row$coef, character(1)))
  expect_length(table$footnotes, 2L)
  notes <- setNames(table$footnotes, vapply(table$footnotes, function(note) note$rowNames, character(1)))
  expect_setequal(names(notes), c("never", "once"))
  expect_setequal(notes$never$colNames, c("mean", "sd", "lowerCri", "upperCri"))
  expect_setequal(notes$once$colNames, c("sd", "lowerCri", "upperCri"))
  expect_match(notes$never$message, "No retained draws", fixed = TRUE)
  expect_match(notes$once$message, "Only one retained draw", fixed = TRUE)
})

test_that("BSTS coefficient output schemas identify marginal and conditional summaries", {
  options <- .bstsCoefficientTestOptions()
  result <- .bstsCoefficientTestResult(.bstsCoefficientTestFit())
  .bstsCoefficientTestBuilder(".bstsCreateCoefficientTable")(result, options, TRUE)
  .bstsCoefficientTestBuilder(".bstsCreateConditionalCoefficientTable")(result, options, TRUE)
  marginal <- result$bstsMainContainer$bstsCoefficientSummaryTable
  conditional <- result$bstsMainContainer$bstsConditionalCoefficientSummaryTable
  expect_identical(marginal$title, "Model-Averaged Coefficient Estimates")
  expect_identical(conditional$title, "Coefficient Estimates Conditional on Inclusion")
  expect_identical(names(marginal$columns), c("coef", "priorIncP", "postIncP", "BFinc", "mean", "sd", "lowerCri", "upperCri"))
  expect_identical(names(conditional$columns), c("coef", "postIncP", "includedDraws", "mean", "sd", "lowerCri", "upperCri"))
  expect_lt(marginal$position, conditional$position)
  expect_identical(marginal$columns$lowerCri$overtitle, "95% Credible Interval")
  expect_identical(conditional$columns$upperCri$overtitle, "95% Credible Interval")
  expect_match(marginal$footnotes[[1L]]$message, "including zeros", fixed = TRUE)
  generalNotes <- Filter(function(note) is.null(note$rowNames), conditional$footnotes)
  expect_length(generalNotes, 2L)
  expect_true(any(vapply(generalNotes, function(note) grepl("nonzero", note$message, fixed = TRUE), logical(1))))
  expect_true(any(vapply(generalNotes, function(note) grepl("few included draws", note$message, fixed = TRUE), logical(1))))
})

test_that("BSTS coefficient output respects visibility, readiness and existing tables", {
  fit <- .bstsCoefficientTestFit()
  mainBuilder <- .bstsCoefficientTestBuilder(".bstsCreateCoefficientTable")
  conditionalBuilder <- .bstsCoefficientTestBuilder(".bstsCreateConditionalCoefficientTable")
  expect_null(mainBuilder(NULL, list(), FALSE))
  expect_null(conditionalBuilder(NULL, list(), FALSE))
  for (case in c("not ready", "no predictors", "summary disabled", "conditional disabled", "shown")) {
    options <- .bstsCoefficientTestOptions()
    ready <- case != "not ready"
    if (case == "no predictors") options$modelTerms <- list()
    if (case == "summary disabled") options$posteriorSummaryTable <- FALSE
    if (case == "conditional disabled") options$posteriorSummaryConditionalEstimates <- FALSE
    result <- .bstsCoefficientTestResult(fit)
    mainBuilder(result, options, ready)
    conditionalBuilder(result, options, ready)
    expect_identical(!is.null(result$bstsMainContainer$bstsCoefficientSummaryTable),
                     case %in% c("conditional disabled", "shown"))
    expect_identical(!is.null(result$bstsMainContainer$bstsConditionalCoefficientSummaryTable), case == "shown")
  }
  result <- .bstsCoefficientTestResult(fit)
  sentinel <- new.env(parent = emptyenv())
  result$bstsMainContainer$bstsCoefficientSummaryTable <- sentinel
  result$bstsMainContainer$bstsConditionalCoefficientSummaryTable <- sentinel
  mainBuilder(result, .bstsCoefficientTestOptions(), TRUE)
  conditionalBuilder(result, .bstsCoefficientTestOptions(), TRUE)
  expect_identical(result$bstsMainContainer$bstsCoefficientSummaryTable, sentinel)
  expect_identical(result$bstsMainContainer$bstsConditionalCoefficientSummaryTable, sentinel)
  for (name in c(".bstsFillCoefficientTable", ".bstsFillConditionalCoefficientTable")) {
    table <- .bstsCoefficientTestTable()
    .bstsCoefficientTestFunction(name)(NULL, table, list(), FALSE)
    expect_length(table$rows, 0L)
    expect_length(table$footnotes, 0L)
  }
})

test_that("BSTS coefficient output dependencies refresh summaries without refitting", {
  fit <- .bstsCoefficientTestFit()
  before <- serialize(fit, NULL)
  result <- .bstsCoefficientTestResult(fit)
  compute <- .bstsCoefficientTestFunction(".bstsComputeResults")
  scope <- new.env(parent = environment(compute))
  scope$.bstsResultsHelper <- function(...) stop("Coefficient display changes must not refit")
  environment(compute) <- scope
  mainBuilder <- .bstsCoefficientTestBuilder(".bstsCreateCoefficientTable")
  conditionalBuilder <- .bstsCoefficientTestBuilder(".bstsCreateConditionalCoefficientTable")
  burnOptions <- c("burninMethod", "burninAutomaticProportion", "burninManualDraws")
  displayOptions <- c("posteriorSummaryTable", "posteriorSummaryConditionalEstimates", "posteriorSummaryCiLevel")
  expect_false(any(c(burnOptions, displayOptions) %in% .bstsCoefficientTestFunction(".bstsModelDependencies")()))
  for (ci in c(0.95, 0.8)) {
    for (burn in c(0, 2)) {
      options <- .bstsCoefficientTestOptions()
      options$burn <- burn
      options$posteriorSummaryCiLevel <- ci
      # Model the engine invalidating derived output while preserving the fit.
      # Actual persistent native invalidation is a separate integration check.
      result$bstsMainContainer$bstsCoefficientSummaryTable <- NULL
      result$bstsMainContainer$bstsConditionalCoefficientSummaryTable <- NULL
      compute(result, NULL, options, TRUE)
      mainBuilder(result, options, TRUE)
      conditionalBuilder(result, options, TRUE)
      marginal <- result$bstsMainContainer$bstsCoefficientSummaryTable
      conditional <- result$bstsMainContainer$bstsConditionalCoefficientSummaryTable
      expect_setequal(marginal$dependencies, c("posteriorSummaryTable", "posteriorSummaryCiLevel", burnOptions))
      expect_setequal(conditional$dependencies, c(displayOptions, burnOptions))
      expectedTitle <- paste0(100 * ci, "% Credible Interval")
      expect_identical(marginal$columns$lowerCri$overtitle, expectedTitle)
      expect_identical(conditional$columns$lowerCri$overtitle, expectedTitle)
      mixed <- .bstsCoefficientTestRows(conditional)$mixed
      expect_identical(mixed$includedDraws, if (burn == 0) 5L else 3L)
      expect_identical(serialize(result$bstsMainContainer$bstsModelResults$object, NULL), before)
    }
  }
})

test_that("BSTS inclusion Bayes factor divides posterior by prior inclusion odds", {
  bf <- .bstsCoefficientTestFunction(".bstsInclusionBayesFactor")
  expect_equal(bf(0.25, 0.5), 3)
  expect_equal(bf(0.5, 0.8), 4)
  expect_equal(bf(0.75, 0.5), 1 / 3)
  expect_equal(bf(c(0.2, 0.6), c(0.2, 0.6)), c(1, 1))
  # Undefined when the prior fixes inclusion or the draws never vary.
  expect_true(all(is.na(bf(c(0, 1, 1, 0.5, 0.5, NA), c(0.5, 0.5, 1, 0, 1, 0.5)))))
})

test_that("BSTS model-averaged table reports inclusion Bayes factors with explained gaps", {
  fit <- .bstsCoefficientTestFit()
  table <- .bstsCoefficientTestTable()
  .bstsCoefficientTestFunction(".bstsFillCoefficientTable")(fit, table, .bstsCoefficientTestOptions(), TRUE)
  rows <- .bstsCoefficientTestRows(table)
  expect_identical(table$rowNames, names(rows))
  # After burn = 2: posterior inclusion 1, 1, 1/2, 1/3, 1/3, 1/6, 0.
  expected <- c(always = NA, negative = NA, mixed = 1 / (0.7 / 0.3), twice = 0.5 / (0.3 / 0.7),
                tied = 0.5 / (0.8 / 0.2), once = 0.2 / (0.2 / 0.8), never = NA)
  expect_equal(vapply(rows, function(row) row$BFinc, numeric(1)), expected)
  notes <- setNames(table$footnotes, vapply(table$footnotes, function(note) note$rowNames, character(1)))
  expect_setequal(names(notes), c("always", "negative", "never"))
  for (note in notes) expect_identical(note$colNames, "BFinc")
  expect_match(notes$always$message, "All 6 retained draws", fixed = TRUE)
  expect_match(notes$never$message, "None of the 6 retained draws", fixed = TRUE)
  # Neutral wording: the draws alone do not say whether the Bayes factor is large or small.
  expect_match(notes$always$message, "estimated as 1 and the inclusion Bayes factor cannot be estimated", fixed = TRUE)
  expect_match(notes$never$message, "estimated as 0 and the inclusion Bayes factor cannot be estimated", fixed = TRUE)
  expect_false(any(grepl("is large|is small", c(notes$always$message, notes$never$message))))

  fit$prior$prior.inclusion.probabilities[] <- 1
  table <- .bstsCoefficientTestTable()
  .bstsCoefficientTestFunction(".bstsFillCoefficientTable")(fit, table, .bstsCoefficientTestOptions(), TRUE)
  expect_true(all(vapply(table$rows, function(row) is.na(row$BFinc), logical(1))))
  expect_length(table$footnotes, ncol(fit$coefficients))
  expect_true(all(vapply(table$footnotes, function(note) grepl("prior inclusion probability is 1", note$message, fixed = TRUE), logical(1))))
})

test_that("BSTS inclusion summaries are withheld only for an intercept bsts absorbs into the trend", {
  fit <- .bstsCoefficientTestFit()
  fit$coefficients <- cbind("(Intercept)" = 0, fit$coefficients[, c("twice", "never")])
  fit$prior$prior.inclusion.probabilities <- c(0.4, 0.3, 0.1)
  fit$predictors <- cbind("(Intercept)" = 1, twice = 1:8, never = c(1, 1, 1, 1, 1, 1, 1, 2))
  absorbedColumn <- .bstsCoefficientTestFunction(".bstsAbsorbedInterceptColumn")
  for (states in list(c("trend", "seasonal.4.1"), "seasonal.4.1")) {
    fit$state.specification <- lapply(states, function(name) list(name = name))
    hasTrend <- "trend" %in% states
    expect_identical(absorbedColumn(fit), c(hasTrend, FALSE, FALSE))
    marginal <- .bstsCoefficientTestTable()
    conditional <- .bstsCoefficientTestTable()
    .bstsCoefficientTestFunction(".bstsFillCoefficientTable")(fit, marginal, .bstsCoefficientTestOptions(), TRUE)
    .bstsCoefficientTestFunction(".bstsFillConditionalCoefficientTable")(fit, conditional, .bstsCoefficientTestOptions(), TRUE)
    intercept <- .bstsCoefficientTestRows(marginal)[["(Intercept)"]]
    expect_identical(is.na(c(intercept$priorIncP, intercept$postIncP)), c(hasTrend, hasTrend))
    expect_true(is.na(intercept$BFinc)) # absorbed, or never included
    # Absorbed: the zeroed placeholder draws are not shown as estimates.
    estimates <- unlist(intercept[c("mean", "sd", "lowerCri", "upperCri")])
    expect_identical(unname(is.na(estimates)), rep(hasTrend, 4L))
    if (hasTrend) {
      marginalNote <- Filter(function(note) identical(note$rowNames, "(Intercept)"), marginal$footnotes)[[1L]]
      expect_setequal(marginalNote$colNames, c("priorIncP", "postIncP", "BFinc", "mean", "sd", "lowerCri", "upperCri"))
    }
    expect_identical(unname(is.na(.bstsCoefficientTestRows(conditional)[["(Intercept)"]]$postIncP)), hasTrend)
    expect_equal(.bstsCoefficientTestRows(marginal)$twice$BFinc, 0.5 / (0.3 / 0.7))
    for (table in list(marginal, conditional)) {
      interceptNotes <- Filter(function(note) identical(note$rowNames, "(Intercept)"), table$footnotes)
      expect_length(interceptNotes, 1L)
      expect_identical(grepl("absorbed into the level component", interceptNotes[[1L]]$message, fixed = TRUE), hasTrend)
    }
  }
})

test_that("BSTS inclusion Bayes factors match bsts fits with and without a trend", {
  skip_if_not_installed("bsts")
  set.seed(20261111)
  n <- 80
  data <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  data$y <- cumsum(rnorm(n, 0, 0.3)) + 0.8 * data$x1 + rnorm(n)
  options <- .bstsCoefficientTestOptions()
  options$burn <- 100
  for (trend in c(TRUE, FALSE)) {
    states <- if (trend) bsts::AddLocalLevel(list(), data$y) else bsts::AddSeasonal(list(), data$y, nseasons = 4)
    fit <- bsts::bsts(y ~ x1 + x2, data = data, state.specification = states, niter = 600,
                      ping = 0, seed = 1, expected.model.size = 1)
    absorbed <- .bstsCoefficientTestFunction(".bstsAbsorbedInterceptColumn")(fit)
    expect_identical(absorbed, c(trend, FALSE, FALSE))
    # bsts zeroes every saved intercept draw exactly when it absorbs the intercept.
    if (trend) expect_true(all(fit$coefficients[, "(Intercept)"] == 0))
    table <- .bstsCoefficientTestTable()
    .bstsCoefficientTestFunction(".bstsFillCoefficientTable")(fit, table, options, TRUE)
    rows <- .bstsCoefficientTestRows(table)
    retained <- fit$coefficients[-seq_len(options$burn), , drop = FALSE]
    for (name in colnames(retained)) {
      prior <- fit$prior$prior.inclusion.probabilities[match(name, colnames(retained))]
      post <- mean(retained[, name] != 0)
      expected <- if (name == "(Intercept)" && trend || post %in% c(0, 1)) NA_real_ else
        (post / (1 - post)) / (prior / (1 - prior))
      expect_equal(rows[[name]]$BFinc, expected)
    }
  }
})

test_that("BSTS coefficient tables say that inclusion concerns the static coefficient under dynamic regression", {
  for (dynamic in c(FALSE, TRUE)) {
    options <- .bstsCoefficientTestOptions()
    options$dynamicRegressionComponent <- dynamic
    result <- .bstsCoefficientTestResult(.bstsCoefficientTestFit())
    .bstsCoefficientTestBuilder(".bstsCreateCoefficientTable")(result, options, TRUE)
    .bstsCoefficientTestBuilder(".bstsCreateConditionalCoefficientTable")(result, options, TRUE)
    for (table in list(result$bstsMainContainer$bstsCoefficientSummaryTable,
                       result$bstsMainContainer$bstsConditionalCoefficientSummaryTable)) {
      general <- vapply(Filter(function(note) is.null(note$rowNames), table$footnotes),
                        function(note) note$message, character(1L))
      expect_identical(any(grepl("concern only the constant (static) coefficient", general, fixed = TRUE)), dynamic)
    }
  }
})

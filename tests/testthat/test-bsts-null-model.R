.bstsNullFunction <- function(name) getFromNamespace(name, "jaspTimeSeries")

.bstsNullData <- function(n = 120L, seed = 20261111L) {
  set.seed(seed)
  x <- rnorm(n)
  data.frame(x = x, z = 0.8 * x + 0.6 * rnorm(n),
             f = factor(rep(c("a", "b", "c"), length.out = n)),
             y = 10 + cumsum(rnorm(n, 0, .2)) + 0.25 * x + rnorm(n))
}

.bstsNullTerms <- function(terms, ticked = character()) {
  lapply(terms, function(term) list(components = strsplit(term, ":", fixed = TRUE)[[1L]], isNuisance = term %in% ticked))
}

test_that("null-model terms get prior inclusion probability 1 on all of their design columns", {
  inclusion <- .bstsNullFunction(".bstsPriorInclusionProbabilities")
  data <- .bstsNullData()
  formula <- y ~ x + z + f + x:z
  terms <- c("x", "z", "f", "x:z")
  # Design columns: (Intercept), x, z, fb, fc, x:z.
  result <- inclusion(formula, data, list(expectedPredictors = 1, modelTerms = .bstsNullTerms(terms, c("f", "x:z"))))
  expect_equal(result$probabilities, c(1 / 6, 1 / 6, 1 / 6, 1, 1, 1))
  expect_identical(result$nullModelColumns, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  # Other columns keep BoomSpikeSlab's default, including its cap at 1.
  result <- inclusion(formula, data, list(expectedPredictors = 2, modelTerms = .bstsNullTerms(terms, "x")))
  expect_equal(result$probabilities, c(1 / 3, 1, 1 / 3, 1 / 3, 1 / 3, 1 / 3))
  result <- inclusion(formula, data, list(expectedPredictors = 7, modelTerms = .bstsNullTerms(terms, "x")))
  expect_equal(result$probabilities, rep(1, 6))
  # Nothing ticked, an older analysis without the field, or no regression: bsts' own default.
  expect_null(inclusion(formula, data, list(expectedPredictors = 1, modelTerms = .bstsNullTerms(terms))))
  expect_null(inclusion(formula, data, list(expectedPredictors = 1, modelTerms = lapply(terms, function(t) list(components = t)))))
  expect_null(inclusion(data$y, data, list(expectedPredictors = 1, modelTerms = .bstsNullTerms(terms, "x"))))
  for (size in list(0, -1, NA_real_))
    expect_error(inclusion(formula, data, list(expectedPredictors = size, modelTerms = .bstsNullTerms(terms, "x"))),
                 "must be larger than 0")
})

test_that("null-model interactions are matched regardless of the order of their variables", {
  inclusion <- .bstsNullFunction(".bstsPriorInclusionProbabilities")
  data <- .bstsNullData()
  # Each case: formula as built from the model terms, the terms with their
  # stored component order, the ticked term, and the design column names that
  # must be forced in. R relabels interactions in formula order (x:z, f:x).
  cases <- list(
    list(formula = y ~ x + z + z:x, terms = list(c("x"), c("z"), c("z", "x")), ticked = "z:x", forced = "x:z"),
    list(formula = y ~ z + x + x:z, terms = list(c("z"), c("x"), c("x", "z")), ticked = "x:z", forced = "z:x"),
    list(formula = y ~ f + x + x:f, terms = list(c("f"), c("x"), c("x", "f")), ticked = "x:f", forced = c("fb:x", "fc:x")),
    list(formula = y ~ x + f + x:f, terms = list(c("x"), c("f"), c("x", "f")), ticked = "x:f", forced = c("x:fb", "x:fc")),
    list(formula = y ~ x + f + x:f, terms = list(c("x"), c("f"), c("x", "f")), ticked = "f", forced = c("fb", "fc"))
  )
  for (case in cases) {
    modelTerms <- lapply(case$terms, function(components)
      list(components = components, isNuisance = identical(paste(components, collapse = ":"), case$ticked)))
    result <- inclusion(case$formula, data, list(expectedPredictors = 1, modelTerms = modelTerms))
    frame <- stats::model.frame(case$formula, data)
    columns <- colnames(stats::model.matrix(attr(frame, "terms"), frame))
    expect_setequal(columns[result$nullModelColumns], case$forced)
    expect_true(all(result$probabilities[columns %in% case$forced] == 1))
    expect_true(all(result$probabilities[!columns %in% case$forced] == 1 / length(columns)))
  }
  # A ticked term that is not in the model gives a clear message.
  modelTerms <- list(list(components = "x", isNuisance = FALSE), list(components = c("x", "w"), isNuisance = TRUE))
  expect_error(inclusion(y ~ x, data, list(expectedPredictors = 1, modelTerms = modelTerms)),
               "not part of the model: x:w")
})

test_that("the inclusion vector reproduces bsts' default prior exactly when it is not modified", {
  skip_if_not_installed("bsts")
  data <- .bstsNullData()
  fit <- function(...) bsts::bsts(y ~ x + z + f, data = data, state.specification = bsts::AddLocalLevel(list(), data$y),
                                  niter = 300, ping = 0, seed = 3, ...)
  default <- fit(expected.model.size = 1)
  explicit <- fit(expected.model.size = 1, prior.inclusion.probabilities = rep(1 / 5, 5))
  expect_identical(explicit$prior$prior.inclusion.probabilities, default$prior$prior.inclusion.probabilities)
  expect_identical(explicit$coefficients, default$coefficients)
})

test_that("ticking Add to null model keeps the predictor in every draw and conditions the others on it", {
  data <- .bstsNullData()
  run <- function(ticked) {
    options <- jaspTools::analysisOptions("bayesianStateSpace")
    options$dependent <- "y"
    options$covariates <- c("x", "z")
    options$localLevelComponent <- TRUE
    options$posteriorSummaryTable <- TRUE
    options$mcmcDraws <- 1000
    options$seed <- 1
    options$modelTerms <- .bstsNullTerms(c("x", "z"), ticked)
    result <- jaspTools::runAnalysis("bayesianStateSpace", data[c("y", "x", "z")], options, view = FALSE, quiet = TRUE)
    expect_identical(result$status, "complete")
    table <- result$results$bstsMainContainer$collection$bstsMainContainer_bstsCoefficientSummaryTable
    fits <- Filter(function(value) inherits(value, "bsts"), result$state$other)
    list(rows = setNames(table$data, vapply(table$data, function(row) row$coef, character(1L))),
         notes = vapply(table$footnotes, function(note) note$text, character(1L)),
         fit = if (length(fits)) fits[[1L]] else NULL)
  }
  free <- run(character())
  fixed <- run("x")
  expect_equal(free$rows$x$priorIncP, 1 / 3)
  expect_lt(free$rows$x$postIncP, 1)
  expect_false(any(grepl("added to the null model", free$notes, fixed = TRUE)))

  expect_equal(fixed$rows$x$priorIncP, 1)
  expect_equal(fixed$rows$x$postIncP, 1)
  expect_identical(fixed$rows$x$BFinc, "")
  expect_true(any(grepl("added to the null model", fixed$notes, fixed = TRUE)))
  expect_false(any(grepl("Lower the expected number of predictors", fixed$notes, fixed = TRUE)))
  expect_equal(fixed$rows$z$priorIncP, 1 / 3)
  expect_true(is.numeric(fixed$rows$z$BFinc))
  # Column names are encoded inside the analysis; the fit records which columns are in the null model.
  expect_false(is.null(fixed$fit))
  expect_identical(attr(fixed$fit, "bstsNullModelColumns"), c(FALSE, TRUE, FALSE))
  expect_equal(fixed$fit$prior$prior.inclusion.probabilities, c(1 / 3, 1, 1 / 3))
  expect_true(all(fixed$fit$coefficients[, 2L] != 0))
  expect_null(attr(free$fit, "bstsNullModelColumns"))
})

test_that("a null-model column that bsts sets to prior 0 gets the prior-0 note, not the null-model note", {
  note <- .bstsNullFunction(".bstsInclusionBayesFactorNote")
  expect_match(note(FALSE, 0, 0, 100L, inNullModel = TRUE)$message, "prior inclusion probability is 0", fixed = TRUE)
  expect_match(note(FALSE, 1, 1, 100L, inNullModel = TRUE)$message, "added to the null model", fixed = TRUE)

  skip_if_not_installed("bsts")
  # Factor combination f = c, g = v never occurs, so the column fc:gv is zero
  # in every observation and bsts sets its prior inclusion probability to 0.
  set.seed(5)
  data <- data.frame(f = factor(rep(c("a", "b", "c", "a", "b", "c"), 20)),
                     g = factor(rep(c("u", "u", "u", "v", "v", "u"), 20)))
  data$y <- 10 + cumsum(rnorm(120, 0, .2)) + rnorm(120)
  expect_identical(sum(data$f == "c" & data$g == "v"), 0L)
  options <- list(expectedPredictors = 1, modelTerms = list(
    list(components = "f", isNuisance = FALSE), list(components = "g", isNuisance = FALSE),
    list(components = c("f", "g"), isNuisance = TRUE)))
  formula <- y ~ f + g + f:g
  inclusion <- .bstsNullFunction(".bstsPriorInclusionProbabilities")(formula, data, options)
  fit <- bsts::bsts(formula, data = data, state.specification = bsts::AddLocalLevel(list(), data$y),
                    niter = 200, ping = 0, seed = 1, prior.inclusion.probabilities = inclusion$probabilities)
  attr(fit, "bstsNullModelColumns") <- inclusion$nullModelColumns

  table <- new.env(parent = emptyenv())
  table$rows <- table$footnotes <- list()
  table$addRows <- function(row, ...) table$rows[[length(table$rows) + 1L]] <- row
  table$addFootnote <- function(message, ...) table$footnotes[[length(table$footnotes) + 1L]] <- c(list(message = message), list(...))
  .bstsNullFunction(".bstsFillCoefficientTable")(fit, table, list(burn = 50, posteriorSummaryCiLevel = .95), TRUE)
  rows <- setNames(table$rows, vapply(table$rows, function(row) row$coef, character(1L)))
  notes <- setNames(vapply(table$footnotes, function(note) note$message, character(1L)),
                    vapply(table$footnotes, function(note) note$rowNames, character(1L)))
  expect_equal(rows[["fc:gv"]]$priorIncP, 0)
  expect_match(notes[["fc:gv"]], "prior inclusion probability is 0", fixed = TRUE)
  expect_equal(rows[["fb:gv"]]$priorIncP, 1)
  expect_match(notes[["fb:gv"]], "added to the null model", fixed = TRUE)
})

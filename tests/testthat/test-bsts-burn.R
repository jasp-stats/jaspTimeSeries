# Deterministic posterior draws: these tests exercise draw selection and output
# calculations, not the sampler. No platform-specific MCMC baseline is needed.
.bstsBurnTestFit <- function() {
  loadNamespace("bsts") # Register summary.bsts even when this file runs alone.
  niter <- 20L
  ntime <- 4L
  coefficients <- cbind(
    first = c(-20, -18, -16, -14, rep(c(0, 1, 2, 3), 4)),
    second = c(20, 18, 16, 14, rep(c(-1, 0, 0, 0), 4))
  )
  structure(list(
    niter = niter,
    family = "gaussian",
    sigma.obs = seq_len(niter) / 10,
    original.series = c(1, 4, 2, 6),
    one.step.prediction.errors = outer(seq_len(niter), seq_len(ntime), "+") / 10,
    state.contributions = array(seq_len(niter * 2L * ntime),
      dim = c(niter, 2L, ntime),
      dimnames = list(NULL, c("level", "trend"), NULL)),
    log.likelihood = c(rep(-100, 4), rep(-3, 8), rep(0, 8)),
    has.regression = TRUE,
    coefficients = coefficients,
    prior = list(prior.inclusion.probabilities = c(0.5, 0.5)),
    timestamp.info = list(timestamps.are.trivial = TRUE, timestamps = seq_len(ntime))
  ), class = "bsts")
}

.bstsBurnTestFunction <- function(name) {
  getFromNamespace(name, "jaspTimeSeries")
}

# A plain R collector, not an internal JASP C++ result object.
.bstsBurnTestTable <- function() {
  table <- new.env(parent = emptyenv())
  table$rows <- list()
  table$addRows <- function(row) table$rows[[length(table$rows) + 1L]] <- row
  table
}

# Replace only the plot constructor in a private copy of the helper's scope.
# The actual draw selection, calculations and ggplot construction are exercised.
.bstsBurnTestStatePlot <- function(name, fit, options) {
  builder <- .bstsBurnTestFunction(name)
  scope <- new.env(parent = environment(builder))
  scope$createJaspPlot <- function(...) new.env(parent = emptyenv())
  environment(builder) <- scope
  plots <- new.env(parent = emptyenv())
  args <- list(bstsStatePlots = plots, bstsResults = fit, options = options, ready = TRUE)
  if (name == ".bstsAggregatedStatePlot")
    args["dataset"] <- list(NULL)
  do.call(builder, args)
  plots[[ls(plots)[1L]]]$plotObject
}

test_that("BSTS manual burn-in uses completed iterations and permits zero", {
  fit <- .bstsBurnTestFit()
  resolve <- .bstsBurnTestFunction(".bstsResolveBurn")
  retained <- .bstsBurnTestFunction(".bstsRetainedDraws")
  for (burn in c(0, 1, 4, 19)) {
    options <- list(burninMethod = "manual", manualBurninAmount = burn, samples = 1000)
    expect_equal(resolve(fit, options), burn)
    expect_identical(retained(fit, burn), seq_len(fit$niter)[seq_len(fit$niter) > burn])
  }
  for (burn in list(-1, 0.5, 1.5, 20, 21, 1000, Inf, NA_real_, NaN,
                   numeric(), c(0, 1), "4")) {
    expect_error(resolve(fit, list(burninMethod = "manual", manualBurninAmount = burn)))
  }
  expect_error(resolve(fit, list(burninMethod = "unknown")))
  for (niter in list(0, -1, 1.5, NA_real_, Inf, NULL)) {
    fit$niter <- niter
    expect_error(resolve(fit, list(burninMethod = "manual", manualBurninAmount = 0)))
  }
})

test_that("BSTS automatic burn-in preserves the likelihood-based suggestion", {
  fit <- .bstsBurnTestFit()
  resolve <- .bstsBurnTestFunction(".bstsResolveBurn")
  for (proportion in c(0.1, 0.5, 1)) {
    options <- list(burninMethod = "auto", automaticBurninProportion = proportion)
    expect_equal(resolve(fit, options), bsts::SuggestBurn(proportion, fit))
  }
  # For these likelihoods the suggestion is 12, not 10% of the 20 draws.
  expect_equal(resolve(fit, list(burninMethod = "auto", automaticBurninProportion = 0.1)), 12)
  for (proportion in list(0, -0.1, 0.001, 1.1, Inf, NA_real_, NaN,
                         numeric(), c(0.1, 0.2), "0.1")) {
    expect_error(resolve(fit, list(burninMethod = "auto", automaticBurninProportion = proportion)))
  }
  fit$log.likelihood <- NULL
  expect_equal(resolve(fit, list(burninMethod = "auto", automaticBurninProportion = 0.5)),
               bsts::SuggestBurn(0.5, fit))
  # A timeout can leave fewer iterations than requested. The default automatic
  # fraction cannot define a likelihood tail for this very short completed fit.
  fit$niter <- 4L
  fit$log.likelihood <- c(-10, -5, -3, -1)
  expect_error(resolve(fit, list(burninMethod = "auto", automaticBurninProportion = 0.1,
                                samples = 1000)))
})

test_that("BSTS burn helper preserves the cached fit and handles not-ready output", {
  helper <- .bstsBurnTestFunction(".bstsBurnHelper")
  fit <- .bstsBurnTestFit()
  cached <- list(bstsMainContainer = list(bstsModelResults = list(object = fit)))
  before <- serialize(cached, NULL)
  options <- list(burninMethod = "manual", manualBurninAmount = 4)
  expect_equal(helper(cached, options)$burn, 4)
  expect_identical(serialize(cached, NULL), before)
  expect_equal(helper(list(bstsMainContainer = list()), options)$burn, 0)
})

test_that("BSTS model-summary statistics use the selected burn-in", {
  fit <- .bstsBurnTestFit()
  fill <- .bstsBurnTestFunction(".bstsFillModelSummaryTable")
  for (burn in c(0, 4, 19)) {
    table <- .bstsBurnTestTable()
    fill(table, fit, TRUE, burn)
    keep <- seq_len(fit$niter) > burn
    residualSD <- mean(fit$sigma.obs[keep])
    errors <- colMeans(fit$one.step.prediction.errors[keep, , drop = FALSE])
    differences <- diff(fit$original.series)
    expected <- list(resSd = residualSD, predSd = sd(errors),
      R2 = 1 - residualSD^2 / var(fit$original.series),
      relGof = 1 - sum(errors^2) / (var(differences) * (length(differences) - 1)))
    expect_equal(table$rows, list(expected))
  }
})

test_that("BSTS coefficient statistics and intervals use the same retained draws", {
  fit <- .bstsBurnTestFit()
  fill <- .bstsBurnTestFunction(".bstsFillCoefficientTable")
  for (burn in c(0, 4, 19)) {
    table <- .bstsBurnTestTable()
    options <- list(burn = burn, posteriorSummaryCiLevel = 0.95, showCoefMeanInc = TRUE)
    fill(fit, table, options, TRUE)
    expect_length(table$rows, ncol(fit$coefficients))
    for (row in table$rows) {
      beta <- fit$coefficients[seq_len(fit$niter) > burn, row$coef]
      included <- beta[beta != 0]
      expect_equal(row$mean, mean(beta))
      expect_equal(row$sd, sd(beta))
      expect_equal(row$postIncP, mean(beta != 0))
      expect_equal(row$meanInc, if (length(included)) mean(included) else 0)
      expect_equal(row$sdInc, if (length(included) > 1L) sd(included) else 0)
      # Preserve existing marginal intervals: excluded (zero) draws still count.
      expect_equal(unname(row$lowerCri), unname(quantile(beta, 0.025)))
      expect_equal(unname(row$upperCri), unname(quantile(beta, 0.975)))
    }
  }
})

test_that("BSTS component curves and bands exclude burn-in without losing time points", {
  fit <- .bstsBurnTestFit()
  for (components in list(1L, 1:2)) {
    fit$state.contributions <- .bstsBurnTestFit()$state.contributions[, components, , drop = FALSE]
    for (burn in c(0, 4, 19)) {
      options <- list(burn = burn, time = seq_along(fit$original.series))
      plot <- .bstsBurnTestStatePlot(".bstsComponentStatePlot", fit, options)
      built <- ggplot2::ggplot_build(plot)
      state <- fit$state.contributions[seq_len(fit$niter) > burn, , , drop = FALSE]
      summarize <- function(fun, ...) {
        unlist(lapply(seq_len(dim(state)[2]), function(component) {
          vapply(seq_len(dim(state)[3]), function(time) fun(state[, component, time], ...), numeric(1))
        }), use.names = FALSE)
      }
      expectedMean <- summarize(mean)
      expectedLower <- summarize(quantile, probs = 0.025)
      expectedUpper <- summarize(quantile, probs = 0.975)
      expect_equal(built$data[[1]]$y, expectedMean)
      expect_equal(built$data[[2]]$ymin, expectedLower)
      expect_equal(built$data[[2]]$ymax, expectedUpper)
      expect_length(built$data[[1]]$x, length(components) * length(fit$original.series))
    }
  }
})

test_that("BSTS aggregated states keep every draw at zero burn-in", {
  fit <- .bstsBurnTestFit()
  for (burn in c(0, 4, 19)) {
    options <- list(burn = burn, time = seq_along(fit$original.series),
      aggregatedStatesPlot = TRUE, aggregatedStatesPlotCiLevel = 0.95,
      aggregatedStatesPlotObservationsShown = FALSE)
    plot <- .bstsBurnTestStatePlot(".bstsAggregatedStatePlot", fit, options)
    built <- ggplot2::ggplot_build(plot)
    state <- fit$state.contributions[seq_len(fit$niter) > burn, , , drop = FALSE]
    total <- apply(state, c(1, 3), sum)
    expect_equal(built$data[[2]]$y, unname(colMeans(total)))
    expect_equal(built$data[[1]]$ymin, unname(apply(total, 2, quantile, probs = 0.025)))
    expect_equal(built$data[[1]]$ymax, unname(apply(total, 2, quantile, probs = 0.975)))
  }
})

test_that("BSTS forecasts receive the resolved burn-in without refitting", {
  fit <- .bstsBurnTestFit()
  calls <- list()
  local_mocked_bindings(predict.bsts = function(object, horizon, seed,
      burn = bsts::SuggestBurn(0.1, object), ...) {
    calls[[length(calls) + 1L]] <<- list(object = object, horizon = horizon, burn = burn, seed = seed)
    list(selectedBurn = burn)
  }, .package = "bsts")

  compute <- .bstsBurnTestFunction(".bstsComputePredictions")
  scope <- new.env(parent = environment(compute))
  scope$createJaspState <- function(...) {
    state <- new.env(parent = emptyenv())
    state$dependOn <- function(dependencies) state$dependencies <- dependencies
    state
  }
  environment(compute) <- scope
  for (burn in c(0, 4, 19)) {
    main <- new.env(parent = emptyenv())
    main$bstsModelResults <- list(object = fit)
    cached <- list(bstsMainContainer = main)
    options <- list(burn = burn, predictionHorizon = 3, seed = 7)
    compute(cached, options, TRUE)
    expect_equal(tail(calls, 1)[[1]], list(object = fit, horizon = 3, burn = burn, seed = 7))
    expect_equal(main$bstsModelPredictions$object$selectedBurn, burn)
    expect_identical(main$bstsModelResults$object, fit)
    count <- length(calls)
    compute(cached, options, TRUE)
    expect_length(calls, count) # Reuse an existing forecast on an unchanged run.
  }
})

test_that("BSTS burn options invalidate derived results, not model fitting", {
  burnOptions <- c("burninMethod", "automaticBurninProportion", "manualBurninAmount")
  for (name in c(".bstsStatePlotDependencies", ".bstsPredictionDependencies", ".bstsControlDependencies"))
    expect_true(all(burnOptions %in% .bstsBurnTestFunction(name)()))
  expect_false(any(burnOptions %in% .bstsBurnTestFunction(".bstsModelDependencies")()))
})

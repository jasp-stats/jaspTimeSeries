test_that("BSTS fitting errors gain context without changing successful fits", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  data <- data.frame(response = c(1, 3, 2, 4))
  options <- list(dependent = "response", covariates = character(), fixedFactors = character(),
                  autoregressiveComponent = FALSE, localLevelComponent = FALSE,
                  localLinearTrendComponent = FALSE, semiLocalLinearTrendComponent = FALSE,
                  dynamicRegregressionComponent = FALSE, seasonalities = NULL,
                  samples = 10, seed = 1, expectedPredictors = 1, timeout = 120)

  numericalErrorPrefix <- paste0(
    "Model estimation failed because of a numerical problem during sampling. ",
    "Check the dependent variable for extreme values and consider rescaling it to more moderate units. ",
    "If the autoregressive component is enabled, temporarily disabling it can help identify the source of the problem.",
    "\n\nTechnical details:\n"
  )
  savedFailure <- paste0(
    "Caught an exception with the following error message in MCMC iteration 17946.  Aborting.\n",
    "rtrun_norm_2_mt caught an exception when called with arguments\n",
    "    mu = 0\n sigma = 1\n    lo = 1.4487e+52\n    hi = 5.11559e+55\n",
    "The error message of the captured exception is \nBmath domain error\n\n"
  )
  for (detail in c("Bmath domain error\nMCMC iteration 1271", savedFailure)) {
    local({
      local_mocked_bindings(bsts = function(...) stop(detail, call. = FALSE), .package = "bsts")
      error <- tryCatch(fit(data, options), error = identity)
      expect_s3_class(error, "validationError")
      expect_identical(conditionMessage(error), paste0(numericalErrorPrefix, detail))
    })
  }

  local({
    detail <- "A different error with 95% in its message"
    local_mocked_bindings(bsts = function(...) stop(detail, call. = FALSE), .package = "bsts")
    error <- tryCatch(fit(data, options), error = identity)
    expect_s3_class(error, "validationError")
    expect_identical(conditionMessage(error), paste0("Model estimation failed.\n\n", detail))
  })

  local({
    result <- list(niter = 10L, value = pi)
    local_mocked_bindings(bsts = function(...) result, .package = "bsts")
    expect_identical(fit(data, options), result)
  })

  local({
    local_mocked_bindings(bsts = function(...) { warning("Sampler warning"); list(niter = 10L) },
                         .package = "bsts")
    expect_warning(result <- fit(data, options), "Sampler warning", fixed = TRUE)
    expect_identical(result, list(niter = 10L))
  })

  local({
    local_mocked_bindings(AddLocalLevel = function(...) stop("State construction error", call. = FALSE),
                         .package = "bsts")
    options$localLevelComponent <- TRUE
    error <- tryCatch(fit(data, options), error = identity)
    expect_identical(conditionMessage(error), "State construction error")
    expect_false(inherits(error, "validationError"))
  })
})

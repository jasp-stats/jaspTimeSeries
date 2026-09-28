test_that("BSTS fitting errors gain context without changing successful fits", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  data <- data.frame(response = c(1, 3, 2, 4))
  options <- list(dependent = "response", covariates = character(), fixedFactors = character(),
                  autoregressiveComponent = FALSE, localLevelComponent = FALSE,
                  localLinearTrendComponent = FALSE, semiLocalLinearTrendComponent = FALSE,
                  dynamicRegregressionComponent = FALSE, seasonalities = NULL,
                  samples = 10, seed = 1, expectedPredictors = 1, timeout = 120)

  for (detail in c("Bmath domain error\nMCMC iteration 1271", "A different error with 95% in its message")) {
    local({
      local_mocked_bindings(bsts = function(...) stop(detail, call. = FALSE), .package = "bsts")
      error <- tryCatch(fit(data, options), error = identity)
      expect_s3_class(error, "validationError")
      expect_identical(conditionMessage(error), paste0("Model estimation failed.\n\n", detail))
    })
  }

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

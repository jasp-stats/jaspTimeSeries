.bstsSamplesTestOptions <- function(samples) {
  list(dependent = "response", covariates = character(), fixedFactors = character(),
       autoregressiveComponent = FALSE, localLevelComponent = TRUE,
       localLinearTrendComponent = FALSE, semiLocalLinearTrendComponent = FALSE,
       dynamicRegressionComponent = FALSE, seasonalities = NULL,
       mcmcDraws = samples, seed = 123, expectedPredictors = 1, mcmcTimeoutSeconds = 120)
}

test_that("BSTS rejects invalid draw counts before constructing or sampling a model", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  # An empty dataset makes an accidental attempt to prepare the model fail,
  # distinguishing early validation from a later error in the sampler.
  invalid <- list(NULL, numeric(), c(7, 25), NA_real_, NaN, Inf, -Inf,
                  0, -1, .5, 25.5, "25", TRUE, 1 + 1i, list(25))
  for (samples in invalid) {
    error <- tryCatch(fit(data.frame(), .bstsSamplesTestOptions(samples)), error = identity)
    expect_s3_class(error, "validationError")
    expect_identical(conditionMessage(error), "Desired MCMC draws must be a positive whole number.")
  }
})

test_that("BSTS completes requested draw counts that are not multiples of ten", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  step <- seq_len(40L)
  data <- data.frame(response = 10 + .03 * step + sin(step / 2.7) + .4 * cos(step / 1.3))
  before <- serialize(data, NULL)
  for (samples in c(1L, 7L, 25L, 1005L)) {
    options <- .bstsSamplesTestOptions(samples)
    optionsBefore <- serialize(options, NULL)
    set.seed(123)
    result <- fit(data, options)
    expect_s3_class(result, "bsts")
    expect_equal(result$niter, samples)
    expect_length(result$sigma.obs, samples)
    expect_equal(dim(result$state.contributions)[1L], samples)
    expect_true(all(is.finite(result$sigma.obs)))
    expect_identical(serialize(options, NULL), optionsBefore)
    expect_identical(serialize(data, NULL), before)
  }
})

test_that("BSTS disabling console progress preserves posterior draws and burn-in rules", {
  fit <- getFromNamespace(".bstsResultsHelper", "jaspTimeSeries")
  resolveBurn <- getFromNamespace(".bstsResolveBurn", "jaspTimeSeries")
  step <- seq_len(40L)
  data <- data.frame(response = 10 + .03 * step + sin(step / 2.7) + .4 * cos(step / 1.3))
  options <- .bstsSamplesTestOptions(40)
  set.seed(123)
  actual <- fit(data, options)
  set.seed(123)
  # Reference call uses the previous bsts default ping = niter / 10.
  invisible(capture.output(expected <- bsts::bsts(
    formula = data$response, data = data,
    state.specification = bsts::AddLocalLevel(list(), data$response),
    niter = options$mcmcDraws, timestamps = NULL, seed = options$seed,
    expected.model.size = options$expectedPredictors,
    model.options = bsts::BstsOptions(timeout.seconds = options$mcmcTimeoutSeconds))))
  for (name in c("sigma.obs", "state.contributions", "one.step.prediction.errors",
                 "log.likelihood", "final.state"))
    expect_identical(actual[[name]], expected[[name]])
  expect_equal(resolveBurn(actual, list(burninMethod = "manual", burninManualDraws = 0)), 0)
  expect_error(resolveBurn(actual, list(burninMethod = "manual", burninManualDraws = 40)),
               "leaving at least one", fixed = TRUE)
})

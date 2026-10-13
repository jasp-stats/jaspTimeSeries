# Exercise real forecast fits with plain R collectors for JASP output objects.
.arimaEmptyTestOptions <- function(...) {
  modifyList(list(modelSpecification = "custom", modelSpecificationAutoIc = "aicc",
                  p = 0, d = 0, q = 0, seasonal = FALSE, P = 0, D = 0, Q = 0,
                  periodSpecification = "custom", m = 4, intercept = FALSE,
                  covariates = character()), list(...))
}

.arimaEmptyTestFit <- function(y, options = .arimaEmptyTestOptions()) {
  fit <- getFromNamespace(".tsArimaResults", "jaspTimeSeries")
  scope <- new.env(parent = environment(fit))
  scope$createJaspState <- function(object) {
    state <- new.env(parent = emptyenv())
    state$object <- object
    state$dependOn <- function(...) invisible(NULL)
    state
  }
  environment(fit) <- scope
  results <- new.env(parent = emptyenv())
  fit(results, data.frame(y = y, t = seq_along(y)), options, TRUE, character())
  results[["arimaResult"]]$object
}

.arimaEmptyTestTable <- function(fit, options, name = ".tsCreateTableCoefficients") {
  table <- new.env(parent = emptyenv())
  table$rows <- list()
  table$footnotes <- character()
  table$dependOn <- function(...) invisible(NULL)
  table$addColumnInfo <- function(...) invisible(NULL)
  table$addRows <- function(rows) table$rows[[length(table$rows) + 1L]] <- rows
  table$addFootnote <- function(message) table$footnotes <- c(table$footnotes, message)
  builder <- getFromNamespace(name, "jaspTimeSeries")
  scope <- new.env(parent = environment(builder))
  scope$createJaspTable <- function(...) table
  environment(builder) <- scope
  builder(new.env(parent = emptyenv()), fit, NULL, options, TRUE, 1, character())
  table
}

.arimaEmptyTestNote <- paste0(
  "The selected model has no coefficients to report. Only the residual variance ",
  "is estimated; it is shown in the model summary."
)

test_that("ARIMA without coefficients retains the estimated white-noise variance", {
  y <- c(-2, 1, 0, 3, -1, 2, -3, 1, 2, -1)
  options <- .arimaEmptyTestOptions()
  fit <- .arimaEmptyTestFit(y, options)
  variance <- mean(y^2)
  logLikelihood <- -length(y) / 2 * (log(2 * pi) + 1 + log(variance))

  expect_length(fit$coef, 0)
  expect_equal(fit$sigma2, variance)
  expect_equal(fit$loglik, logLikelihood)
  expect_equal(attr(logLik(fit), "df"), 1)
  expect_equal(fit$aic, -2 * logLikelihood + 2)

  summary <- .arimaEmptyTestTable(fit, options, ".tsCreateTableModel")
  expect_equal(summary$rows[[1]]$sigma, variance)
  expect_equal(summary$rows[[1]]$ll, logLikelihood)
  expect_equal(summary$rows[[1]]$aic, -2 * logLikelihood + 2)

  coefficients <- .arimaEmptyTestTable(fit, options)
  expect_length(coefficients$rows, 0)
  expect_identical(coefficients$footnotes, c(
    "An ARIMA(0, 0, 0) model was fitted.", .arimaEmptyTestNote
  ))

  prediction <- forecast::forecast(fit, h = 6, level = 95)
  expect_equal(as.numeric(prediction$mean), rep(0, 6))
  expect_equal(as.numeric(prediction$upper), rep(qnorm(0.975) * sqrt(variance), 6))
  expect_equal(as.numeric(prediction$lower), -as.numeric(prediction$upper))
})

test_that("ARIMA random walks without drift remain forecastable", {
  y <- c(3, 4, 2, 5, 6, 4, 7, 5, 8, 9)
  options <- .arimaEmptyTestOptions(d = 1)
  fit <- .arimaEmptyTestFit(y, options)
  expect_length(fit$coef, 0)
  coefficients <- .arimaEmptyTestTable(fit, options)
  expect_length(coefficients$rows, 0)
  expect_identical(coefficients$footnotes, c(
    "An ARIMA(0, 1, 0) model was fitted.", .arimaEmptyTestNote
  ))

  prediction <- forecast::forecast(fit, h = 6, level = 95)
  expectedMean <- rep(tail(y, 1), 6)
  expectedMargin <- qnorm(0.975) * sqrt(seq_len(6) * fit$sigma2)
  expect_equal(as.numeric(prediction$mean), expectedMean)
  expect_equal(as.numeric(prediction$upper), expectedMean + expectedMargin)
  expect_equal(as.numeric(prediction$lower), expectedMean - expectedMargin)
})

test_that("Seasonal random walks without coefficients retain their model order", {
  y <- c(1, 4, 2, 6, 3, 5, 1, 7, 2, 6, 3, 5, 4, 7, 2, 8)
  options <- .arimaEmptyTestOptions(seasonal = TRUE, D = 1, m = 4)
  fit <- .arimaEmptyTestFit(y, options)
  expect_length(fit$coef, 0)
  coefficients <- .arimaEmptyTestTable(fit, options)
  expect_length(coefficients$rows, 0)
  expect_identical(coefficients$footnotes, c(
    "An ARIMA(0, 0, 0)(0, 1, 0)[4] model was fitted.", .arimaEmptyTestNote
  ))

  prediction <- forecast::forecast(fit, h = 6, level = 95)
  expectedMean <- rep(tail(y, 4), length.out = 6)
  expectedMargin <- qnorm(0.975) * sqrt(ceiling(seq_len(6) / 4) * fit$sigma2)
  expect_equal(as.numeric(prediction$mean), expectedMean)
  expect_equal(as.numeric(prediction$upper), expectedMean + expectedMargin)
  expect_equal(as.numeric(prediction$lower), expectedMean - expectedMargin)
})

test_that("Nonempty ARIMA coefficient tables still report the fitted coefficients", {
  y <- c(3, 4, 2, 5, 6, 4, 7, 5, 8, 9, 6, 7, 4, 5, 3, 6, 8, 5, 7, 4)
  for (p in c(0, 1)) {
    options <- .arimaEmptyTestOptions(p = p, intercept = TRUE)
    fit <- .arimaEmptyTestFit(y, options)
    coefficients <- .arimaEmptyTestTable(fit, options)
    expect_length(coefficients$rows, 1)
    rows <- coefficients$rows[[1]]
    order <- c("intercept", if (p == 1) "ar1")
    expect_identical(rows$coefficients, c("Intercept", if (p == 1) "AR(1)"))
    expect_equal(rows$estimate, unname(fit$coef[order]))
    expect_equal(rows$SE, unname(sqrt(diag(fit$var.coef))[order]))
    expect_identical(coefficients$footnotes,
                     sprintf("An ARIMA(%s, 0, 0) model was fitted.", p))
  }
})

test_that("ARIMA backend fitting failures remain validation errors", {
  # A negative AR order is a real backend rejection, not an empty valid model.
  error <- NULL
  invisible(capture.output(
    error <- tryCatch(.arimaEmptyTestFit(1:10, .arimaEmptyTestOptions(p = -1)),
                      error = identity), type = "message"
  ))
  expect_s3_class(error, "validationError")
  expect_identical(conditionMessage(error), "The ARIMA model could not be fit.")
})

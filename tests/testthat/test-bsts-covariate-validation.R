test_that("BSTS explains missing or non-numeric covariates", {
  validate <- getFromNamespace(".bstsErrorHandling", "jaspTimeSeries")
  for (values in list(c(1, NA_real_), suppressWarnings(as.numeric(c("control", "experimental"))))) {
    data <- data.frame(predictor = values, valid = c(1, 2))
    error <- tryCatch(validate(data, list(covariates = c("predictor", "valid"))),
                      validationError = identity)
    expect_s3_class(error, "validationError")
    expect_match(conditionMessage(error), "missing values or values that cannot be interpreted as numbers", fixed = TRUE)
    expect_match(conditionMessage(error), "numbers: predictor.", fixed = TRUE)
    expect_match(conditionMessage(error), "For categorical predictors, use Fixed Factors.", fixed = TRUE)
  }

  data <- data.frame(first = c(NA, 2), second = c(1, NA))
  expect_error(validate(data, list(covariates = names(data))),
               "numbers: first, second.", fixed = TRUE, class = "validationError")
})

test_that("BSTS covariate validation preserves valid and not-ready inputs", {
  validate <- getFromNamespace(".bstsErrorHandling", "jaspTimeSeries")
  data <- data.frame(response = c(1, NA_real_), predictor = c(1, 2),
                     group = factor(c("control", "experimental")))
  before <- data
  expect_identical(validate(data, list(covariates = "predictor", fixedFactors = "group")), FALSE)
  expect_identical(data, before)
  expect_identical(validate(data, list(covariates = character())), FALSE)
  expect_identical(validate(NULL, list(covariates = "predictor")), FALSE)
  expect_identical(validate(data.frame(), list(covariates = "predictor")), FALSE)
})

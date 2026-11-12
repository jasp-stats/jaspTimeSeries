# Use the real jaspBase data-frame reader for requested type conversions, while
# replacing only the native data source in private copies of the BSTS functions.
.bstsCovariateTestHarness <- function(raw) {
  validate <- getFromNamespace(".bstsErrorHandling", "jaspTimeSeries")
  read <- getFromNamespace(".bstsReadData", "jaspTimeSeries")
  scope <- new.env(parent = environment(validate))
  reads <- new.env(parent = emptyenv())
  reads$calls <- list()
  scope$.readDataSetToEnd <- function(...) {
    reads$calls[[length(reads$calls) + 1L]] <- list(...)
    jaspBase::.vdf(raw, ...)
  }
  environment(validate) <- scope
  environment(read) <- scope
  list(validate = validate, read = read, reads = reads)
}

.bstsCovariateTestError <- function(harness, data, covariates) {
  tryCatch(harness$validate(data, list(covariates = covariates)),
           validationError = identity)
}

test_that("BSTS missing covariates get missing-value advice without categorical advice", {
  raw <- data.frame(predictor = c(1, NA_real_, 3), valid = c(1, 2, 3))
  harness <- .bstsCovariateTestHarness(raw)
  before <- serialize(raw, NULL)
  error <- .bstsCovariateTestError(harness, raw, c("predictor", "valid"))
  expect_s3_class(error, "validationError")
  expect_match(conditionMessage(error), "covariates contain missing values: predictor.", fixed = TRUE)
  expect_match(conditionMessage(error), "Resolve the missing values before fitting the model.", fixed = TRUE)
  expect_false(grepl("cannot be interpreted", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("Fixed Factors", conditionMessage(error), fixed = TRUE))
  expect_identical(harness$reads$calls, list(list(columns.as.factor = "predictor")))
  expect_identical(serialize(raw, NULL), before)
})

test_that("BSTS nonnumeric covariates get numeric and categorical advice", {
  raw <- data.frame(predictor = factor(c("control", "experimental", "control")),
                    valid = c(1, 2, 3))
  converted <- data.frame(predictor = rep(NA_real_, 3), valid = raw$valid)
  harness <- .bstsCovariateTestHarness(raw)
  before <- serialize(converted, NULL)
  error <- .bstsCovariateTestError(harness, converted, c("predictor", "valid"))
  expect_s3_class(error, "validationError")
  expect_match(conditionMessage(error), "values that cannot be interpreted as numbers: predictor.", fixed = TRUE)
  expect_match(conditionMessage(error), "For categorical predictors, use Fixed Factors.", fixed = TRUE)
  expect_false(grepl("contain missing values", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("Resolve the missing values", conditionMessage(error), fixed = TRUE))
  expect_identical(harness$reads$calls, list(list(columns.as.factor = "predictor")))
  expect_identical(serialize(converted, NULL), before)
})

test_that("BSTS distinguishes both causes across covariates or within one covariate", {
  raw <- data.frame(missing = c("1", NA, "3"),
                    text = c("low", "high", "low"), valid = c(1, 2, 3))
  converted <- data.frame(missing = c(1, NA_real_, 3), text = rep(NA_real_, 3), valid = raw$valid)
  harness <- .bstsCovariateTestHarness(raw)
  error <- .bstsCovariateTestError(harness, converted, names(raw))
  expect_s3_class(error, "validationError")
  parts <- strsplit(conditionMessage(error), "\n\n", fixed = TRUE)[[1L]]
  expect_length(parts, 2L)
  expect_match(parts[1L], "covariates contain missing values: missing.", fixed = TRUE)
  expect_match(parts[2L], "values that cannot be interpreted as numbers: text.", fixed = TRUE)
  expect_false(grepl("Fixed Factors", parts[1L], fixed = TRUE))
  expect_identical(harness$reads$calls, list(list(columns.as.factor = c("missing", "text"))))

  raw <- data.frame(predictor = c(NA, "group", "-2.5"))
  converted <- data.frame(predictor = c(NA_real_, NA_real_, -2.5))
  harness <- .bstsCovariateTestHarness(raw)
  error <- .bstsCovariateTestError(harness, converted, "predictor")
  expect_s3_class(error, "validationError")
  parts <- strsplit(conditionMessage(error), "\n\n", fixed = TRUE)[[1L]]
  expect_length(parts, 2L)
  expect_match(parts[1L], "covariates contain missing values: predictor.", fixed = TRUE)
  expect_match(parts[2L], "values that cannot be interpreted as numbers: predictor.", fixed = TRUE)
  expect_identical(harness$reads$calls, list(list(columns.as.factor = "predictor")))
})

test_that("BSTS classifies only numeric-missing positions using reader missingness", {
  # Native JASP decides which values count as missing. This fixture represents
  # its returned factors; do not infer a second set of missing-value tokens here.
  # Row labels are deliberately unrelated to order: classification is positional.
  raw <- data.frame(missing = c(NA, "2", "3"),
                    text = c(NA, "group", "3"),
                    row.names = c("sheet-9", "sheet-2", "sheet-7"))
  converted <- data.frame(missing = c(NA_real_, 2, 3), text = c(1, NA_real_, 3),
                          row.names = row.names(raw))
  harness <- .bstsCovariateTestHarness(raw)
  error <- .bstsCovariateTestError(harness, converted, names(raw))
  parts <- strsplit(conditionMessage(error), "\n\n", fixed = TRUE)[[1L]]
  expect_length(parts, 2L)
  expect_match(parts[1L], "covariates contain missing values: missing.", fixed = TRUE)
  expect_match(parts[2L], "values that cannot be interpreted as numbers: text.", fixed = TRUE)
  expect_false(grepl("missing, text", parts[1L], fixed = TRUE))
  expect_identical(harness$reads$calls, list(list(columns.as.factor = names(raw))))
})

test_that("BSTS does not invent missing-value tokens from nonmissing factor labels", {
  # This case applies when the reader returns a literal label, not an NA.
  # Desktop's configured missing-value strings are handled by the native reader.
  raw <- data.frame(predictor = factor(c("NA", "1", "2")))
  converted <- data.frame(predictor = c(NA_real_, 1, 2))
  harness <- .bstsCovariateTestHarness(raw)
  error <- .bstsCovariateTestError(harness, converted, "predictor")
  expect_s3_class(error, "validationError")
  expect_match(conditionMessage(error), "values that cannot be interpreted as numbers: predictor.", fixed = TRUE)
  expect_false(grepl("contain missing values", conditionMessage(error), fixed = TRUE))
  expect_identical(harness$reads$calls, list(list(columns.as.factor = "predictor")))
})

test_that("BSTS covariate validation avoids diagnostic reads for valid or not-ready input", {
  data <- data.frame(response = c(1, NA_real_), predictor = c(0, -2.5),
                     ignored = c(NA_real_, NA_real_),
                     group = factor(c("control", "experimental")))
  harness <- .bstsCovariateTestHarness(data)
  before <- serialize(data, NULL)
  expect_identical(harness$validate(data, list(covariates = "predictor", fixedFactors = "group")), FALSE)
  expect_identical(harness$validate(data, list(covariates = character())), FALSE)
  expect_identical(harness$validate(NULL, list(covariates = "predictor")), FALSE)
  expect_identical(harness$validate(data.frame(), list(covariates = "predictor")), FALSE)
  expect_null(harness$read(list(), FALSE))
  expect_length(harness$reads$calls, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("BSTS numeric text and numeric factor labels retain their actual values", {
  raw <- data.frame(response = c(10, 20, NA_real_, 40),
                    text = c("0", "-2.5", " 1.25 ", "3e-2"),
                    labels = factor(c("0", "-2.5", "1.25", "0.03")),
                    group = factor(c("a", "b", "a", "b")),
                    time = c("2003", "2001", "2000", "2002"))
  before <- serialize(raw, NULL)
  options <- list(dependent = "response", covariates = c("text", "labels"),
                  fixedFactors = "group", time = "time")
  harness <- .bstsCovariateTestHarness(raw)
  converted <- harness$read(options, TRUE)
  expect_equal(converted$text, c(0, -2.5, 1.25, 0.03))
  expect_equal(converted$labels, converted$text)
  expect_false(identical(converted$labels, as.numeric(raw$labels)))
  expect_identical(converted$response, raw$response)
  expect_identical(converted$time, raw$time)
  expect_identical(converted$group, raw$group)
  expect_identical(harness$validate(converted, options), FALSE)
  expect_length(harness$reads$calls, 1L) # Only the initial data read.
  expect_identical(harness$reads$calls[[1L]]$columns.as.numeric, c("response", "text", "labels"))
  expect_identical(serialize(raw, NULL), before)
})

test_that("BSTS read and validation distinguish factor labels from missing values", {
  raw <- data.frame(response = c(30, NA_real_, 10, 40),
                    missing = c("3", NA, "1", "4"),
                    category = factor(c("experimental", "control", "control", "experimental")),
                    valid = c("0", "-2.5", "1.25", "3e-2"),
                    ignored = rep(NA_real_, 4),
                    group = factor(c("b", "a", "a", "b")),
                    time = c("2003", "2001", "2000", "2002"))
  before <- serialize(raw, NULL)
  options <- list(dependent = "response", covariates = c("missing", "category", "valid"),
                  fixedFactors = "group", time = "time")
  harness <- .bstsCovariateTestHarness(raw)
  # Deliberate nonnumeric labels cause the base-R reader's coercion warning.
  converted <- suppressWarnings(harness$read(options, TRUE))
  convertedBefore <- serialize(converted, NULL)
  expect_identical(converted$response, raw$response)
  expect_equal(converted$missing, c(3, NA_real_, 1, 4))
  expect_true(all(is.na(converted$category)))
  expect_equal(converted$valid, c(0, -2.5, 1.25, 0.03))
  expect_identical(converted$time, raw$time)
  expect_false("ignored" %in% names(converted))
  error <- tryCatch(harness$validate(converted, options), validationError = identity)
  expect_s3_class(error, "validationError")
  parts <- strsplit(conditionMessage(error), "\n\n", fixed = TRUE)[[1L]]
  expect_length(parts, 2L)
  expect_match(parts[1L], "covariates contain missing values: missing.", fixed = TRUE)
  expect_match(parts[2L], "values that cannot be interpreted as numbers: category.", fixed = TRUE)
  expect_length(harness$reads$calls, 2L)
  expect_identical(harness$reads$calls[[2L]], list(columns.as.factor = c("missing", "category")))
  expect_identical(serialize(converted, NULL), convertedBefore)
  expect_identical(serialize(raw, NULL), before)
})

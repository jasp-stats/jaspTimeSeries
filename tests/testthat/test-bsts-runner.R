.bstsRunnerFixture <- function() {
  scope <- new.env(parent = baseenv())
  scope$outputFiles <- scope$nestedFiles <- character()
  scope$outputExistedAtExit <- logical()
  scope$nestedHelper <- function() {
    path <- tempfile()
    writeLines("nested helper artifact", path)
    nestedFiles <<- c(nestedFiles, path)
  }
  environment(scope$nestedHelper) <- scope
  scope$runner <- function(value, fail = FALSE) {
    path <- tempfile()
    outputFiles <<- c(outputFiles, path)
    sink(path)
    on.exit({
      sink(NULL)
      outputExistedAtExit <<- c(outputExistedAtExit, file.exists(path))
    }, add = TRUE)
    cat("discarded analysis output\n")
    nestedHelper()
    if (fail) stop("runner failed", call. = FALSE)
    invisible(value)
  }
  environment(scope$runner) <- scope
  scope
}

test_that("BSTS runner removes only its own output file after normal return", {
  fixture <- .bstsRunnerFixture()
  unrelated <- tempfile()
  writeLines("unrelated artifact", unrelated)
  on.exit(unlink(c(unrelated, fixture$outputFiles, fixture$nestedFiles)), add = TRUE)
  local_mocked_bindings(runAnalysis = fixture$runner, .package = "jaspTools")

  expected <- list(status = "complete", estimates = c(1.25, -0.5))
  actual <- withVisible(runBsts(value = expected))
  expect_identical(actual$value, expected)
  expect_false(actual$visible)
  expect_length(fixture$outputFiles, 1L)
  expect_identical(fixture$outputExistedAtExit, TRUE)
  expect_false(file.exists(fixture$outputFiles))
  expect_identical(readLines(unrelated), "unrelated artifact")
  expect_identical(readLines(fixture$nestedFiles), "nested helper artifact")
})

test_that("BSTS runner cleans its output after an error without swallowing the error", {
  fixture <- .bstsRunnerFixture()
  on.exit(unlink(c(fixture$outputFiles, fixture$nestedFiles)), add = TRUE)
  local_mocked_bindings(runAnalysis = fixture$runner, .package = "jaspTools")
  sinkDepth <- sink.number()

  expect_error(runBsts(value = NULL, fail = TRUE), "runner failed", fixed = TRUE)
  expect_identical(sink.number(), sinkDepth)
  expect_length(fixture$outputFiles, 1L)
  expect_identical(fixture$outputExistedAtExit, TRUE)
  expect_false(file.exists(fixture$outputFiles))
  expect_true(file.exists(fixture$nestedFiles))
})

test_that("repeated BSTS calls leave the original runner and helper environments unchanged", {
  fixture <- .bstsRunnerFixture()
  on.exit(unlink(c(fixture$outputFiles, fixture$nestedFiles)), add = TRUE)
  originalRunner <- fixture$runner
  local_mocked_bindings(runAnalysis = originalRunner, .package = "jaspTools")

  for (value in seq_len(3L)) {
    expect_identical(runBsts(value), value)
    expect_false(any(file.exists(fixture$outputFiles)))
    expect_true(all(file.exists(fixture$nestedFiles)))
    expect_identical(jaspTools::runAnalysis, originalRunner)
    expect_identical(environment(jaspTools::runAnalysis), fixture)
    expect_identical(environment(fixture$nestedHelper), fixture)
  }
  expect_length(fixture$outputFiles, 3L)
  expect_length(fixture$nestedFiles, 3L)
})

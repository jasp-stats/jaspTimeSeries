test_that("BSTS seed changes invalidate the model container and its cached fit", {
  dependencies <- getFromNamespace(".bstsModelDependencies", "jaspTimeSeries")()
  expect_true("seed" %in% dependencies)

  builder <- getFromNamespace(".bstsCreateContainerMain", "jaspTimeSeries")
  scope <- new.env(parent = environment(builder))
  # Plain R collector checks the actual dependency declaration.
  container <- new.env(parent = emptyenv())
  container$dependOn <- function(dependencies) container$dependencies <- dependencies
  scope$createJaspContainer <- function(...) container
  environment(builder) <- scope
  results <- new.env(parent = emptyenv())
  builder(results, list(), TRUE)
  expect_identical(results$bstsMainContainer$dependencies, dependencies)
  expect_true("seed" %in% results$bstsMainContainer$dependencies)
})

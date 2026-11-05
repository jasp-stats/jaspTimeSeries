test_that("cached BSTS summaries work in a fresh R process", {
  # A separate process is essential: earlier tests may already have loaded bsts.
  # Deterministic saved draws avoid running MCMC to test package loading.
  fit <- structure(list(
    niter = 4L,
    family = "gaussian",
    sigma.obs = c(0.1, 0.2, 0.3, 0.4),
    original.series = c(1, 4, 2, 6),
    one.step.prediction.errors = matrix(seq_len(16) / 10, nrow = 4),
    has.regression = TRUE,
    coefficients = cbind(first = c(100, 200, 1, 3), second = c(-100, 0, -2, 0)),
    prior = list(prior.inclusion.probabilities = c(0.5, 0.5)),
    timestamp.info = list(timestamps.are.trivial = TRUE, timestamps = 1:4)
  ), class = "bsts")

  scratch <- tempfile("bsts-namespace-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  input <- file.path(scratch, "input.rds")
  output <- file.path(scratch, "output.rds")
  script <- file.path(scratch, "check.R")
  modulePath <- getNamespaceInfo(asNamespace("jaspTimeSeries"), "path")
  saveRDS(list(fit = fit, libraries = unique(c(dirname(modulePath), .libPaths()))), input)

  check <- quote({
    args <- commandArgs(trailingOnly = TRUE)
    saved <- readRDS(args[1L])
    .libPaths(saved$libraries, include.site = FALSE)
    stopifnot(!"bsts" %in% loadedNamespaces())
    loadNamespace("jaspTimeSeries")

    # Reuse the saved fit with manual burn-in and no predictions or refit.
    cached <- list(bstsMainContainer = list(bstsModelResults = list(object = saved$fit)))
    options <- getFromNamespace(".bstsBurnHelper", "jaspTimeSeries")(cached,
      list(burninMethod = "manual", manualBurninAmount = 2, posteriorSummaryCiLevel = 0.95))
    collector <- function() {
      table <- new.env(parent = emptyenv())
      table$rows <- list()
      table$addRows <- function(row) table$rows[[length(table$rows) + 1L]] <- row
      table
    }
    model <- collector()
    coefficients <- collector()
    getFromNamespace(".bstsFillModelSummaryTable", "jaspTimeSeries")(model, saved$fit, TRUE, options$burn)
    getFromNamespace(".bstsFillCoefficientTable", "jaspTimeSeries")(saved$fit, coefficients, options, TRUE)
    saveRDS(list(burn = options$burn, model = model$rows, coefficients = coefficients$rows,
                 fit = cached$bstsMainContainer$bstsModelResults$object), args[2L])
  })
  writeLines(deparse(check, width.cutoff = 100L), script)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  log <- suppressWarnings(system2(rscript, shQuote(c("--vanilla", script, input, output)),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(log, "status")
  if (is.null(status)) status <- 0L
  expect_identical(status, 0L, info = paste(log, collapse = "\n"))
  if (status == 0L) {
    result <- readRDS(output)
    expect_equal(result$burn, 2)
    expect_length(result$model, 1L)
    expect_equal(result$model[[1L]]$resSd, mean(fit$sigma.obs[3:4]))
    expect_length(result$coefficients, 2L)
    expect_equal(vapply(result$coefficients, `[[`, "", "coef"), c("first", "second"))
    expect_equal(vapply(result$coefficients, `[[`, 0, "mean"), c(2, -1))
    expect_equal(vapply(result$coefficients, `[[`, 0, "postIncP"), c(1, 0.5))
    expect_identical(result$fit, fit)
  }
})

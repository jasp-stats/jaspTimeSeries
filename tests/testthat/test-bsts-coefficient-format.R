test_that("BSTS coefficient means and SDs use JASP's standard numeric formatting", {
  builder <- getFromNamespace(".bstsCreateCoefficientTable", "jaspTimeSeries")
  scope <- new.env(parent = environment(builder))
  # Inspect column declarations without fitting a model or constructing native
  # result objects; the supported runner verifies actual table serialization.
  table <- new.env(parent = emptyenv())
  table$columns <- list()
  table$dependOn <- function(...) NULL
  table$addColumnInfo <- function(name, ...) table$columns[[name]] <- list(...)
  scope$createJaspTable <- function(...) table
  scope$.bstsFillCoefficientTable <- function(...) NULL
  environment(builder) <- scope
  result <- list(bstsMainContainer = new.env(parent = emptyenv()))
  options <- list(modelTerms = list(list(components = "predictor")),
                  posteriorSummaryTable = TRUE, showCoefMeanInc = TRUE,
                  posteriorSummaryCiLevel = 0.95)
  builder(result, options, TRUE)
  for (column in c("mean", "sd", "meanInc", "sdInc")) {
    expect_identical(table$columns[[column]]$type, "number")
    expect_null(table$columns[[column]]$format)
  }
})
